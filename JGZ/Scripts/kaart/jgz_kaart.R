# =============================================================================
# JGZ kaart: outcomes per organisation, PC3 and PC4 with the kansenkaart method
#
#   Input : loc$cohort_file (output/jgz_cohort.rds of Opportunity-Preprocessing,
#           cohort jgz: one row per child, y_m<age>_* outcomes, pc3/pc4/org per age)
#   Output: loc$kaart_folder/
#             jgz_kaart_estimates.rds  all estimates with exact n (internal, never export)
#             jgz_kaart_results.rds    after the CBS rules (reason_of_na, n rounded)
#             jgz_kaart_results.csv    same, for checking in Excel
#
# Per outcome, area and group: kansenkaart_expect() of the national kaart, i.e.
# lm(outcome ~ income_parents_perc) within the area and group, predicted at the
# 25th/50th/75th percentile for Low/Mid/High, and the mean for income "all".
# Settings (groups, areas, CBS rules): JGZ/config.yml, "kaart".
#
# Parental income (20-11-2026): for an outcome y_<age>_* the cohort's
# income_parents_perc_<age> is used: the mother's household income over the 5
# years up to the year the child reaches that age (2024 prices), ranked among
# all children in NL reaching that age in the same year. Children without an
# income at that age are left out.
#
# Run from the repository root:
#   setwd("H:/Opportunity-Analysis-Map"); source("JGZ/Scripts/kaart/jgz_kaart.R")
# =============================================================================

source("JGZ/Scripts/jgz_helpers.R")
source("JGZ/Scripts/kaart/jgz_kaart_functions.R")
source_expectation_function("src/income_parents")

ensure_dir(loc$kaart_folder)
t_start <- Sys.time()

cohort <- readRDS(loc$cohort_file)
outcomes <- kaart_outcome_list(cohort, cfg$kaart_outcomes)

region_types <- unlist(cfg$kaart_region_types)
if (isTRUE(cfg$export_pooled)) region_types <- c("all", region_types)
groups <- kaart_groups(cfg$kaart_income_groups, cfg$kaart_gender_groups,
                       cfg$kaart_migration_groups, cfg$kaart_household_groups)

message(sprintf("JGZ kaart: %d children, %d outcomes, %d groups, areas %s, organisations %s",
                nrow(cohort), length(outcomes), nrow(groups), paste(region_types, collapse = "/"),
                paste(cfg$export_orgs, collapse = ", ")))


# ---- Estimates -----------------------------------------------------------------------------

estimates <- vector("list", length(outcomes))
for (k in seq_along(outcomes)) {
  oc <- outcomes[k]
  t0 <- Sys.time()
  d <- kaart_outcome_data(cohort, oc, cfg$export_orgs)
  estimates[[k]] <- kaart_estimate_outcome(d, oc, region_types, groups, conf_level = cfg$kaart_conf_level)
  message(sprintf("  [%d/%d] %s: %d children, %d estimates, %.1f s", k, length(outcomes), oc, nrow(d),
                  if (is.null(estimates[[k]])) 0L else nrow(estimates[[k]]),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
estimates <- do.call(rbind, estimates)
saveRDS(estimates, file.path(loc$kaart_folder, "jgz_kaart_estimates.rds"))


# ---- CBS output rules ----------------------------------------------------------------------

results <- kaart_disclosure(estimates,
                            min_n_mean = cfg$kaart_min_n_mean, min_n_regression = cfg$kaart_min_n_regression,
                            min_count = cfg$kaart_min_count, round_n = cfg$kaart_round_n)
saveRDS(results, file.path(loc$kaart_folder, "jgz_kaart_results.rds"))
write.csv(results, file.path(loc$kaart_folder, "jgz_kaart_results.csv"), row.names = FALSE, na = "")

message("Estimates per region type and share published (not suppressed):")
print(results %>%
        group_by(region_type) %>%
        summarise(areas = n_distinct(region_id), estimates = n(), published = mean(is.na(reason_of_na))))
message("Reasons for suppression:")
print(table(results$reason_of_na, useNA = "ifany"))
message(sprintf("JGZ kaart done in %.1f min", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
