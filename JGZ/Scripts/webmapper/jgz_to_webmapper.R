#-------------------------------------------------------------------------------
# JGZ results -> webmapper format (prototype)
#-------------------------------------------------------------------------------
# What this does
#   Takes the JGZ results exported from CBS (xlsx, one value per postcode area,
#   for ALL children together) and writes them in the same wide format as the
#   regular kansenkaart webmapper files (see c30_income.csv / c11_class_math.csv):
#
#     "", id, name, type, all_all_all, all_all_native, ..., BW_low_female_totalnonwestern
#
#   The JGZ results have no breakdown by parental income, gender or migration
#   background. Agreed with Bastiaan/Edward (meeting 20-10-2026): paste the SAME
#   value into every filter combination, so the filters in the interface work
#   mechanically but show no variation between groups.
#
# Choices made here (all in the CONFIG block, easy to change)
#   - only areas belonging to the selected JGZ organisation(s) are written
#   - one outcome per file; English names: y_m<age in months>_<measure>
#   - percentages with 1 decimal (as in c11_class_math.csv)
#   - gender "diff" columns are NA (a difference of 0 would claim "no gender gap")
#   - BW = half-width of a 95% Wald interval, only where a denominator n is
#     available in the export; NA otherwise
#
# Runs outside the CBS environment, on already-exported results.
#-------------------------------------------------------------------------------

library(tidyverse)
library(openxlsx)
options(scipen = 999)


#### CONFIG --------------------------------------------------------------------

# root of the Opportunity Lab SharePoint folder (Opportunity Lab - Documenten)
base_dir <- "C:/Users/nilsv/Erasmus University Rotterdam/Opportunity Lab - Documenten"
if (nzchar(Sys.getenv("OPPLAB_DIR"))) base_dir <- Sys.getenv("OPPLAB_DIR")  # for testing elsewhere

jgz_dir    <- file.path(base_dir, "Opportunity-Analysis-Map/JGZ")
export_dir <- file.path(jgz_dir, "Export 27-03-2025")           # CBS export to read
pc4_org_file <- file.path(jgz_dir, "resources/PC4gebieden.csv") # PC4 -> organisation
mun_file   <- file.path(base_dir, "Opportunity-Analysis-Gap/areas/Gemeenten alfabetisch 2022.xlsx")

# JGZ organisations (codes as in org_catchment / PC4gebieden.csv)
org_names <- c("6" = "Limburg-Noord", "7" = "ZONL", "10" = "Verian", "12" = "org12")
orgs_selected <- c("6")              # start: Limburg-Noord. Never add "12" (org12): no permission.

sample_id <- "jgz_1_0"               # own JGZ sample numbering (not s2_0); used as output folder
out_dir   <- file.path(jgz_dir, "webmapper", sample_id,
                       paste(org_names[orgs_selected], collapse = "_"))

decimals   <- 1                      # decimals for all percentages
diff_value <- NA_real_               # value for *_diff_* columns (NA, or 0)
min_areas  <- 5                      # skip an outcome with fewer areas than this (nothing to show)

# age at measurement in months (M-prefix). School groups use the mean age in
# that group: PLACEHOLDERS until the real mean ages come out of the CBS data.
age_months <- c(
  "4wks" = 1, "4weeks" = 1, "8wks" = 2, "8weeks" = 2, "3months" = 3, "4months" = 4,
  "6months" = 6, "9months" = 9, "11months" = 11, "14months" = 14, "18months" = 18,
  "2years" = 24, "3years" = 36, "3years9m" = 45, "3y9m" = 45,
  "groep2" = 60, "groep7" = 120, "klas2" = 162,   # <- placeholders, check!
  "takeup" = 45                                    # take-up covers 2y, 3y and 3y9m visits
)

make_name <- function(age, measure) paste0("y_m", age_months[[age]], "_", measure)


#### WHAT TO CONVERT -----------------------------------------------------------
# One row per (outcome, region level). Columns:
#   outcome     output name (one file per outcome)
#   file/sheet  where the numbers are in the export
#   level       municipality / pc3 / pc4
#   id_col      column with the area code (NULL-safe: "1" = first column)
#   value_col   column with the estimate
#   scale       multiply value by this to get a percentage (pr 0-1 -> 100)
#   n_col       denominator for BW (NA if not available)
#   filter      for long sheets: which row(s) to take; several categories
#               separated by "+" are summed (e.g. "zin2w_vol+zin2w_M")
#   filter_col  column that `filter` refers to

spec <- list()

# 1. Take-up (bereik): proportions export -----------------------------------
# always = seen at all of 2y, 3y, 3y9m; any = seen at least once
takeup_vars <- c(always = "d_obs_always", any = "d_observed")
for (m in names(takeup_vars)) {
  v <- takeup_vars[[m]]
  measure <- paste0("takeup_", m)
  spec[[length(spec) + 1]] <- tribble(
    ~level,         ~sheet,
    "municipality", "gem_proportions.xlsx",
    "pc3",          "pc3_proportions.xlsx",
    "pc4",          "pc4_proportions.xlsx"
  ) %>% mutate(age = "takeup", measure = measure,
               outcome = make_name("takeup", measure), file = "proportions_export.xlsx",
               id_col = "1", value_col = "pr", scale = 100, n_col = "N_total",
               filter_col = "variable", filter = v)
}

# 2. BMI: % above the 90th percentile, all ages --------------------------------
bmi_pc4 <- c("4wks", "8wks", "3months", "4months", "6months", "9months", "11months",
             "14months", "18months", "2years", "3years", "3years9m", "groep2", "groep7", "klas2")
bmi_pc3 <- str_replace(bmi_pc4, "wks$", "weeks")    # the PC3 file spells weeks out
spec[[length(spec) + 1]] <- bind_rows(
  tibble(age = bmi_pc4, level = "pc4", file = "output_cases_postcode4_bmi_export.xlsx",
         sheet = paste0("bmi_90p_", bmi_pc4), id_col = "postcode"),
  tibble(age = bmi_pc3, level = "pc3", file = "output_cases_postcode3_bmi_export.xlsx",
         sheet = paste0("bmi_90p_", bmi_pc3), id_col = "postcode3")
) %>% mutate(outcome = map_chr(age, make_name, measure = "bmi_p90"),
             measure = "bmi_p90",
             value_col = "bmi_90p_perc", scale = 1, n_col = NA, filter_col = NA, filter = NA)

# 3. SDQ total score >= 11 (share above the cut-off, not the mean) -------------
sdq_ages <- c("groep2", "groep7", "klas2", "3years9m")
spec[[length(spec) + 1]] <- expand_grid(age = sdq_ages, level = c("pc4", "pc3")) %>%
  mutate(outcome = map_chr(age, make_name, measure = "sdq11"),
         file = "output_cases_postcode4_3_sdq_export.xlsx",
         sheet = paste0("sdq11_", age, "_", level),
         id_col = if_else(level == "pc4", "postcode", "postcode3"),
         measure = "sdq11",
         value_col = "sdq_11_perc", scale = 1, n_col = NA, filter_col = NA, filter = NA)

# 4. Language items (Van Wiechen) ---------------------------------------------
# Bastiaan's two cut-offs per item:
#   _can     : child can do it according to parent OR professional = vol + M(elding)
#   _notseen : professional has not seen it                         = onvol + M
# The current export (3 categories) only has all parts for zin2w and vragen1;
# pop6 and verstaan2 only have "_vol", so they are skipped until the new
# 2-category export arrives.
wiechen <- tribble(
  ~age,    ~item,     ~measure,         ~filter,
  "2years", "zin2w",   "words2_can",     "zin2w_vol+zin2w_M",
  "2years", "zin2w",   "words2_notseen", "zin2w_onvol+zin2w_M",
  "3y9m",   "vragen1", "questions_can",  "vragen1_vol+vragen1_M",
  "3y9m",   "vragen1", "questions_notseen", "vragen1_onvol+vragen1_M"
)
spec[[length(spec) + 1]] <- expand_grid(wiechen, level = c("pc4", "pc3")) %>%
  mutate(outcome = map2_chr(age, measure, make_name),
         file = "output_cases_postcode4_3_wiechen_export.xlsx",
         sheet = paste0("wiechen_", age, "_", level),
         id_col = if_else(level == "pc4", "postcode", "postcode3"),
         value_col = "perc", scale = 1, n_col = "n", filter_col = "outcome") %>%
  select(-item)

spec <- bind_rows(spec)


#### AREAS OF THE SELECTED ORGANISATION(S) -------------------------------------
# PC4 -> organisation. A PC4 is kept only if it belongs to a selected org and to
# no other org (PC4 8080 is listed under both Verian and org12).
pc4_org <- read_csv(pc4_org_file, col_types = cols(.default = "c")) %>%
  mutate(postcode_4 = str_pad(postcode_4, 4, pad = "0"))

pc4_keep <- pc4_org %>%
  group_by(postcode_4) %>%
  filter(all(org %in% orgs_selected)) %>%
  pull(postcode_4) %>% unique()

# PC3: kept only if ALL its PC4s in the lookup are kept (no mixing with other orgs)
pc3_keep <- pc4_org %>%
  mutate(pc3 = substr(postcode_4, 1, 3), keep = postcode_4 %in% pc4_keep) %>%
  group_by(pc3) %>% filter(all(keep)) %>% pull(pc3) %>% unique()

# municipalities: names from the lookup -> 2022 codes
mun <- read.xlsx(mun_file) %>%
  transmute(code = as.integer(Gemeentecode), gemeentenaam = Gemeentenaam)
mun_keep <- pc4_org %>%
  group_by(gemeente) %>% filter(all(org %in% orgs_selected)) %>% ungroup() %>%
  distinct(gemeente) %>%
  left_join(mun, by = c("gemeente" = "gemeentenaam"))
stopifnot(!anyNA(mun_keep$code))   # every municipality name must match the 2022 list

cat("Selected:", paste(org_names[orgs_selected], collapse = ", "), "->",
    nrow(mun_keep), "municipalities,", length(pc3_keep), "PC3,", length(pc4_keep), "PC4\n")


#### READ ONE SPEC ROW -> long table (outcome, level, region, value, n) ----------
read_one <- function(file, sheet, level, id_col, value_col, scale, n_col,
                     filter_col, filter, outcome) {
  path <- file.path(export_dir, file)
  if (!sheet %in% getSheetNames(path)) {
    message("  skip (sheet not in export): ", file, " / ", sheet)
    return(NULL)
  }
  df <- read.xlsx(path, sheet = sheet)
  names(df)[if (id_col == "1") 1 else which(names(df) == id_col)] <- "region"
  df <- df %>% mutate(region = as.character(as.integer(as.numeric(region))),
                      value  = as.numeric(.data[[value_col]]),
                      n      = if (is.na(n_col)) NA_real_ else as.numeric(.data[[n_col]]))

  if (!is.na(filter)) {
    cats <- str_split_1(filter, fixed("+"))
    df <- df %>%
      filter(.data[[filter_col]] %in% cats) %>%
      group_by(region) %>%
      # sum of the categories; NA if one of them is missing (suppressed)
      summarise(value = if (n_distinct(.data[[filter_col]]) == length(cats) & !anyNA(value))
                          sum(value) else NA_real_,
                n = first(n), .groups = "drop")
  }

  df %>%
    filter(!is.na(region)) %>%
    transmute(outcome, level, region, value = value * scale, n)
}

long <- pmap(select(spec, -age, -measure), read_one) %>% bind_rows() %>% filter(!is.na(value))

# keep only the selected organisation's areas
long <- long %>%
  filter((level == "pc4" & region %in% pc4_keep) |
         (level == "pc3" & region %in% pc3_keep) |
         (level == "municipality" & as.integer(region) %in% mun_keep$code))


#### TO WEBMAPPER FORMAT --------------------------------------------------------
# column names exactly as in the regular webmapper files
income    <- c("all", "high", "mid", "low")
gender    <- c("all", "male", "female", "diff")
migration <- c("all", "native", "turkey", "morocco", "surinam", "antilles",
               "otherswestern", "othersnonwestern", "totalnonwestern")
est_cols <- expand_grid(i = income, g = gender, m = migration) %>%
  mutate(col = paste(i, g, m, sep = "_")) %>% pull(col)
bw_cols  <- paste0("BW_", est_cols[!str_detect(est_cols, "_diff_")])

to_webmapper <- function(d) {
  d <- d %>%
    mutate(
      # 95% CI half-width in percentage points, only if n is known
      bw   = 1.96 * sqrt((value / 100) * (1 - value / 100) / n) * 100,
      est  = round(value, decimals),
      bw   = round(bw, decimals),
      type = factor(level, levels = c("municipality", "pc3", "pc4")),
      code = as.integer(region)
    ) %>%
    left_join(mun, by = "code") %>%
    mutate(id   = if_else(type == "municipality", sprintf("%04d0000", code), ""),  # empty for pc3/pc4, as in the examples
           name = if_else(type == "municipality", gemeentenaam, region)) %>%
    arrange(type, code)

  wide <- d %>% select(id, name, type) %>% mutate(type = as.character(type))
  # same estimate in every filter combination (the agreed prototype trick)
  for (col in est_cols) wide[[col]] <- if (str_detect(col, "_diff_")) diff_value else d$est
  for (col in bw_cols)  wide[[col]] <- d$bw
  wide
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

too_small <- long %>% count(outcome) %>% filter(n < min_areas) %>% pull(outcome)
if (length(too_small)) message("Skipped (fewer than ", min_areas, " areas): ", paste(too_small, collapse = ", "))
long <- long %>% filter(!outcome %in% too_small)

overview <- long %>% count(outcome, level) %>% pivot_wider(names_from = level, values_from = n, values_fill = 0)
for (oc in unique(long$outcome)) {
  out <- to_webmapper(filter(long, outcome == oc))
  write.csv(out, file.path(out_dir, paste0(oc, ".csv")))   # row names + "NA", as in the examples
}

#### OUTCOME TABLE (labels for the website) -----------------------------------
# Same layout as the existing webmapper outcome_table.xlsx (sheet "outcome"):
#   prefix_postfix  dummy = percentage (all JGZ outcomes)
#   analyse_outcome variable name (= file name)
#   outcome_name    short Dutch label
#   type            domain / theme the outcome is listed under
#   definition      sentence that follows the label: "is het percentage ..."
#   population      who it is about
#   format          number of decimals
# {pop} in a definition is replaced by the population of that age.
# Uncertain wording is NOT put in the definition; it goes to sheet "notes".

population <- c(
  "4wks" = "kinderen van 4 weken", "4weeks" = "kinderen van 4 weken",
  "8wks" = "kinderen van 8 weken", "8weeks" = "kinderen van 8 weken",
  "3months" = "kinderen van 3 maanden", "4months" = "kinderen van 4 maanden",
  "6months" = "kinderen van 6 maanden", "9months" = "kinderen van 9 maanden",
  "11months" = "kinderen van 11 maanden", "14months" = "kinderen van 14 maanden",
  "18months" = "kinderen van 18 maanden", "2years" = "kinderen van 2 jaar",
  "3years" = "kinderen van 3 jaar", "3years9m" = "kinderen van 3 jaar en 9 maanden",
  "3y9m" = "kinderen van 3 jaar en 9 maanden",
  "groep2" = "leerlingen in groep 2", "groep7" = "leerlingen in groep 7",
  "klas2" = "leerlingen in klas 2 van het voortgezet onderwijs",
  "takeup" = "kinderen van 2 tot 4 jaar"
)

labels <- tribble(
  ~measure,            ~outcome_name,                 ~type,                 ~definition,
  "takeup_always",     "Altijd gezien door de JGZ",   "Bereik",              "is het percentage {pop} dat bij alle drie de contactmomenten op 2 jaar, 3 jaar en 3 jaar en 9 maanden door de jeugdgezondheidszorg is gezien.",
  "takeup_any",        "Minstens eens gezien door de JGZ", "Bereik",         "is het percentage {pop} dat bij minstens één van de contactmomenten op 2 jaar, 3 jaar of 3 jaar en 9 maanden door de jeugdgezondheidszorg is gezien.",
  "bmi_p90",           "Hoog BMI",                    "Gewicht",             "is het percentage {pop} met een BMI boven het 90e percentiel van alle gemeten {pop}.",
  "sdq11",             "Sociaal-emotionele problemen", "Sociaal-emotioneel", "is het percentage {pop} met een totaalscore van 11 of hoger op de SDQ (Strengths and Difficulties Questionnaire), een signaal voor sociaal-emotionele problemen.",
  "words2_can",        "Zegt zinnen van twee woorden", "Taalontwikkeling",   "is het percentage {pop} dat zinnen van twee woorden zegt, vastgesteld door de jeugdgezondheidszorg of gemeld door de ouder (Van Wiechen-onderzoek).",
  "words2_notseen",    "Twee-woordzinnen niet vastgesteld", "Taalontwikkeling", "is het percentage {pop} bij wie de jeugdgezondheidszorg niet zelf heeft vastgesteld dat het kind zinnen van twee woorden zegt: onvoldoende, of alleen gemeld door de ouder (Van Wiechen-onderzoek).",
  "questions_can",     "Stelt vragen",                "Taalontwikkeling",    "is het percentage {pop} dat vragen stelt (wie, wat, waar, waarom), vastgesteld door de jeugdgezondheidszorg of gemeld door de ouder (Van Wiechen-onderzoek).",
  "questions_notseen", "Vragen stellen niet vastgesteld", "Taalontwikkeling", "is het percentage {pop} bij wie de jeugdgezondheidszorg niet zelf heeft vastgesteld dat het kind vragen stelt: onvoldoende, of alleen gemeld door de ouder (Van Wiechen-onderzoek)."
)

# open questions per measure, to settle before publishing
notes <- tribble(
  ~measure,         ~to_check,
  "takeup_always",  "M-age 45 chosen for the whole 2y-3y9m window. Which proportions sheet is final (with/without _6/_7/_10 suffix)?",
  "takeup_any",     "Mostly suppressed (take-up close to 100%); consider leaving it out.",
  "bmi_p90",        "90th percentile of which reference group (all children in the data, per sex, per age)? Bastiaan: check in the code. Name 'Hoog BMI' instead of 'overgewicht' on purpose (not the clinical definition). PC4 export has only 99 rows per sheet.",
  "sdq11",          "Cut-off 11 to be reviewed by Bastiaan before the final run. Who filled in the SDQ (parent / teacher / youth)?",
  "words2_can",     "Item label assumed from 'zin2w' (zinnen van 2 woorden); check in the BDS who assesses (jeugdarts / jeugdverpleegkundige).",
  "words2_notseen", "Current export has 3 categories that do not always sum to 100%; replace by the new 2-category export.",
  "questions_can",  "Item 'vragen1' assumed to be the wie/wat/waar/waarom question; check in the BDS.",
  "questions_notseen", "As words2_notseen."
)

school_note <- "Months for school groups (groep 2 = 60, groep 7 = 120, klas 2 = 162) are placeholders until the mean age is computed in CBS."

outcome_table <- spec %>%
  distinct(outcome, .keep_all = TRUE) %>%            # PC3 and PC4 rows share the outcome
  filter(outcome %in% unique(long$outcome)) %>%      # only outcomes that were written
  select(analyse_outcome = outcome, age, measure) %>%
  left_join(labels, by = "measure") %>%
  mutate(pop            = population[age],
         definition     = str_replace_all(definition, fixed("{pop}"), pop),
         population     = pop,
         prefix_postfix = "dummy",
         format         = decimals) %>%
  arrange(type, measure, age_months[age]) %>%
  select(prefix_postfix, analyse_outcome, outcome_name, type, definition, population, format)
stopifnot(!anyNA(outcome_table))   # every written outcome needs a label

notes_table <- spec %>%
  distinct(outcome, .keep_all = TRUE) %>%
  filter(outcome %in% outcome_table$analyse_outcome) %>%
  left_join(notes, by = "measure") %>%
  mutate(to_check = if_else(age %in% c("groep2", "groep7", "klas2"),
                            paste(to_check, school_note), to_check)) %>%
  select(analyse_outcome = outcome, to_check)

wb <- createWorkbook()
addWorksheet(wb, "outcome"); writeData(wb, "outcome", outcome_table)
addWorksheet(wb, "notes");   writeData(wb, "notes", notes_table)
saveWorkbook(wb, file.path(out_dir, "outcome_table.xlsx"), overwrite = TRUE)

cat("\nWritten to", out_dir, "\n")
print(overview, n = Inf)
