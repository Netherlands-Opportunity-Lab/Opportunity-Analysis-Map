# =============================================================================
# JGZ -- map tables (BMI, Wiechen language items, SDQ) per PC3 and PC4
#
# Aggregates the contact-moment files from the preprocessing
# (Opportunity-Preprocessing/src/jgz/02_contact_moments.R) per postcode area
# of the GBA address. One measurement per child per visit.
#
#   Input : loc$contact_moment_folder/cm_<visit>.rds (preprocessing output)
#   Output: loc$maps_folder/jgz_maps_bmi.xlsx      sheets bmi_<visit>_<pc3|pc4>[_sex]
#           loc$maps_folder/jgz_maps_wiechen.xlsx  sheets wiechen_<visit>_<pc3|pc4>
#           loc$maps_folder/jgz_maps_sdq.xlsx      sheets sdq_<visit>_<pc3|pc4>
#
# Disclosure control (CBS output rules, cfg$min_cell = 10):
#   - areas with fewer than min_cell children are suppressed entirely
#   - a count (and its %) is suppressed when the count or n - count < min_cell
#   Not handled: secondary suppression (e.g. boys = total - girls). Check
#   this before requesting export.
#
# Run from the repository root (Opportunity-Analysis-Map).
# =============================================================================

source("JGZ/Scripts/jgz_helpers.R")
suppressPackageStartupMessages(library(openxlsx))

ensure_dir(loc$maps_folder)

CMS <- c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
         "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2")

wiechen_items <- c("zin2w", "pop6", "noemt", "wijstaan", "zin3w", "verstaan1",
                   "spontaan", "vragen1", "verstaan2", "vragen2")

# The raw health file codes the Wiechen items as numbers (codebook: 1 = plus,
# 2 = min, 3 = M / onbekend); the old Stata panel had the text "+", "-", "M".
# Both are turned into "+", "-", "M" here, anything else becomes missing.
wiechen_code <- function(x) {
  x <- trimws(as.character(haven::zap_labels(x)))
  x <- unname(c("1" = "+", "2" = "-", "3" = "M", "+" = "+", "-" = "-", "M" = "M")[x])
  x
}


jgz <- bind_rows(lapply(CMS, function(k) readRDS(file.path(loc$contact_moment_folder, paste0("cm_", k, ".rds"))))) %>%
  # Wiechen items as "+" / "-" / "M"; "" and ".r" (conflict marker) missing
  mutate(across(any_of(wiechen_items), wiechen_code))

# ---- Helpers ----------------------------------------------------------------------

# One measurement per child: closest to the target age, then the oldest.
one_per_child <- function(d) {
  d %>%
    mutate(dist = abs(target_age - leeftijd)) %>%
    arrange(RINPERSOONS, RINPERSOON, dist, desc(leeftijd)) %>%
    distinct(RINPERSOONS, RINPERSOON, .keep_all = TRUE) %>%
    select(-dist)
}

# Apply a summary function per area, for PC3 and PC4, with and without sex.
# Returns a named list of tables: <prefix>_pc3, <prefix>_pc3_sex, ...
area_tables <- function(d, prefix, summary_fun, by_sex = TRUE) {
  out <- list()
  for (geo in c("pc3", "pc4")) {
    d_geo <- d %>% filter(!is.na(.data[[geo]])) %>% rename(area = all_of(geo))
    out[[paste0(prefix, "_", geo)]] <- summary_fun(group_by(d_geo, area)) %>%
      rename(!!(if (geo == "pc3") "postcode3" else "postcode") := area)
    if (by_sex) {
      out[[paste0(prefix, "_", geo, "_sex")]] <- summary_fun(group_by(d_geo %>% filter(!is.na(sex)), area, sex)) %>%
        rename(!!(if (geo == "pc3") "postcode3" else "postcode") := area, geslacht = sex)
    }
  }
  out
}

# Suppress rows of areas with too few children; NA n means "suppressed".
drop_small <- function(tab) {
  tab %>% mutate(n = ifelse(n < cfg$min_cell, NA, n))
}

write_sheets <- function(tables, file) {
  wb <- createWorkbook()
  for (nm in names(tables)) {
    addWorksheet(wb, nm)
    writeData(wb, nm, tables[[nm]])
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  message("Written: ", file, " (", length(tables), " sheets)")
}


# =============================================================================
# BMI
# =============================================================================
bmi_summary <- function(g) {
  g %>%
    summarise(n = n(), bmi_mean = mean(BMI), bmi_sd = sd(BMI),
              bmi_90p = sum(bmi_90), .groups = "drop") %>%
    mutate(
      bmi_mean = round(suppress_small_n(bmi_mean, n), 2),
      bmi_sd   = round(suppress_small_n(bmi_sd, n), 2),
      bmi_90p_perc = round(suppress_count(bmi_90p / n * 100, bmi_90p, n), 1),
      bmi_90p      = suppress_count(bmi_90p, bmi_90p, n)
    ) %>%
    drop_small()
}

bmi_tables <- list()
for (k in CMS) {
  d <- jgz %>%
    filter(cm == k, !is.na(BMI), !is.na(sex)) %>%
    # outliers: |z| > bmi_z_max within visit x sex
    group_by(sex) %>%
    mutate(z = (BMI - mean(BMI)) / sd(BMI)) %>%
    ungroup()
  n_out <- sum(abs(d$z) > cfg$bmi_z_max, na.rm = TRUE)
  d <- d %>%
    filter(is.na(z) | abs(z) <= cfg$bmi_z_max) %>%
    one_per_child() %>%
    # top 10%: BMI above the 90th percentile of this visit, within sex
    # (cfg$bmi_p90_by_sex = TRUE) or over boys and girls together (FALSE, as
    # Coen's gradient do-file: bmi > p90 per contact moment)
    group_by(grp = if (isTRUE(cfg$bmi_p90_by_sex)) sex else "all") %>%
    mutate(bmi_90 = as.integer(BMI > quantile(BMI, 0.9))) %>%
    ungroup() %>%
    select(-grp)
  message(sprintf("BMI %-8s: %6d children, %d outliers removed", k, nrow(d), n_out))
  bmi_tables <- c(bmi_tables, area_tables(d, paste0("bmi_", k), bmi_summary))
}
write_sheets(bmi_tables, file.path(loc$maps_folder, "jgz_maps_bmi.xlsx"))


# =============================================================================
# WIECHEN LANGUAGE ITEMS
# =============================================================================
# Per child, the last observed value of each item within the window
# (ordered by age). "Taalontwikkeling achterstand" (taal_ont) = at least one
# item scored "-" ("onvoldoende"). Denominator: children with any item.
#
# DISCLOSURE (Bastian, 20 Oct 2026): exporting all three categories (+, M, -)
# per item lets anyone derive a suppressed one from the other two. His
# instruction is to export two categories, above and below the cut-off.
# Note that exporting BOTH proposed cut-offs ("+ or M" vs "-", and "+" vs
# "M or -") has the same problem. Not changed here -- decide which split is
# exported before the next export request.
wiechen_children <- function(window_cms, items) {
  d <- jgz %>%
    filter(cm %in% window_cms) %>%
    filter_at(vars(all_of(items)), any_vars(!is.na(.))) %>%
    arrange(RINPERSOONS, RINPERSOON, leeftijd)
  # last observed value per item
  item_vals <- d %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    summarise(across(all_of(items), function(x) { x <- x[!is.na(x)]; if (length(x)) x[length(x)] else NA_character_ }),
              .groups = "drop")
  # geography and sex from the child's last row in the window
  geo <- d %>%
    group_by(RINPERSOONS, RINPERSOON) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(RINPERSOONS, RINPERSOON, pc3, pc4, sex)
  item_vals %>%
    left_join(geo, by = c("RINPERSOONS", "RINPERSOON")) %>%
    mutate(taal_ont = as.integer(rowSums(across(all_of(items), function(x) x %in% "-")) > 0))
}

wiechen_summary <- function(items) {
  function(g) {
    tab <- g %>% summarise(n = n(), taal_ont_achts = sum(taal_ont), .groups = "drop")
    counts <- g %>% summarise(across(all_of(items), list(
      vol   = function(x) sum(x %in% "+"),
      M     = function(x) sum(x %in% "M"),
      onvol = function(x) sum(x %in% "-")
    ), .names = "{.col}_{.fn}"), .groups = "drop")
    tab <- bind_cols(tab, counts %>% select(-any_of(group_vars(g))))
    count_cols <- setdiff(names(counts), group_vars(g))
    tab <- tab %>%
      mutate(taal_ont_achts_perc = round(suppress_count(taal_ont_achts / n * 100, taal_ont_achts, n), 1),
             taal_ont_achts      = suppress_count(taal_ont_achts, taal_ont_achts, n))
    for (cc in count_cols) {
      tab[[paste0(cc, "_perc")]] <- round(suppress_count(tab[[cc]] / tab$n * 100, tab[[cc]], tab$n), 1)
    }
    tab %>% select(-all_of(count_cols)) %>% drop_small()
  }
}

wiechen_sets <- list(
  `2years`   = list(window = c("18months", "2years"), items = c("zin2w", "pop6")),
  `3years9m` = list(window = c("3years", "3years9m"), items = c("vragen1", "verstaan2", "vragen2"))
)
wiechen_tables <- list()
for (k in names(wiechen_sets)) {
  s <- wiechen_sets[[k]]
  d <- wiechen_children(s$window, s$items)
  message(sprintf("Wiechen %-8s: %6d children, %.1f%% with a language delay flag",
                  k, nrow(d), 100 * mean(d$taal_ont)))
  wiechen_tables <- c(wiechen_tables,
                      area_tables(d, paste0("wiechen_", k), wiechen_summary(s$items), by_sex = FALSE))
}
write_sheets(wiechen_tables, file.path(loc$maps_folder, "jgz_maps_wiechen.xlsx"))


# =============================================================================
# SDQ (Strengths and Difficulties Questionnaire, total score)
# =============================================================================
sdq_summary <- function(g) {
  tab <- g %>% summarise(n = n(), sdq_mean = mean(sdq_t), sdq_sd = sd(sdq_t), .groups = "drop")
  for (cut_off in cfg$sdq_cutoffs) {
    cnt <- g %>% summarise(c = sum(sdq_t >= cut_off), .groups = "drop") %>% pull(c)
    tab[[paste0("sdq_", cut_off, "_perc")]] <- round(suppress_count(cnt / tab$n * 100, cnt, tab$n), 1)
    tab[[paste0("sdq_", cut_off, "_n")]]    <- suppress_count(cnt, cnt, tab$n)
  }
  tab %>%
    mutate(sdq_mean = round(suppress_small_n(sdq_mean, n), 2),
           sdq_sd   = round(suppress_small_n(sdq_sd, n), 2)) %>%
    drop_small()
}

sdq_tables <- list()
for (k in c("3years9m", "groep2", "groep7", "klas2")) {
  d <- jgz %>% filter(cm == k, !is.na(sdq_t)) %>% one_per_child()
  message(sprintf("SDQ %-8s: %6d children", k, nrow(d)))
  sdq_tables <- c(sdq_tables, area_tables(d, paste0("sdq_", k), sdq_summary, by_sex = FALSE))
}
write_sheets(sdq_tables, file.path(loc$maps_folder, "jgz_maps_sdq.xlsx"))
