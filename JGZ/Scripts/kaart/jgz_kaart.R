# =============================================================================
# JGZ kaart: outcomes per organisation, PC3 and PC4 with the kansenkaart method
#
#   Input : loc$cohort_file (output/jgz_cohort.rds of Opportunity-Preprocessing,
#           cohort jgz: one row per child, m<age>_* outcomes, pc3/pc4/org per age)
#           loc$catchment_cohort_file (output/jgz_catchment_cohort.rds, cohort
#           jgz_catchment: one row per child living in a catchment area, seen
#           or not, with the bereik outcomes m024/m036/m045_takeup,
#           m045_takeup_always, m045_takeup_any and the same area and income
#           columns; org = catchment area, pc4/pc3 = the child's address)
#   Output: loc$kaart_folder/
#             jgz_kaart_estimates.rds  all estimates with exact n (internal, never export)
#             jgz_kaart_results.rds    after the CBS rules (reason_of_na, n rounded)
#             jgz_kaart_results.csv    same, for checking in Excel
#             internal/INTERNAL_jgz_kaart_*  the same for internal_orgs (org 12):
#                                    internal use only, NEVER export (20-11-2026)
#
# Bereik (27-11-2026; Bastian, 20-11-2026): the take-up outcomes are outcomes of
# the kaart like the others (per area and income group), with column
# sample = "jgz_catchment" in the results; the separate bereik script
# (JGZ/Scripts/bereik) was removed. Their denominator is every child living in
# the area with a parental income at that age.
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

jgz_check_orgs(cfg)
ensure_dir(loc$kaart_folder)
t_start <- Sys.time()

cohort_files <- kaart_cohort_files(loc)
for (f in cohort_files) if (!file.exists(f)) stop("Cohort file not found: ", f)
cohorts <- lapply(cohort_files, readRDS)
outcomes <- kaart_outcomes_by_cohort(cohorts, cfg$kaart_outcomes)

region_types <- unlist(cfg$kaart_region_types)
if (isTRUE(cfg$export_pooled)) region_types <- c("all", region_types)
groups <- kaart_groups(cfg$kaart_income_groups, cfg$kaart_gender_groups,
                       cfg$kaart_migration_groups, cfg$kaart_household_groups)

for (s in names(cohorts)) {
  message(sprintf("JGZ kaart, %s: %d children, %d outcomes", s, nrow(cohorts[[s]]), length(outcomes[[s]])))
}
message(sprintf("%d groups, areas %s, organisations %s", nrow(groups), paste(region_types, collapse = "/"),
                paste(cfg$export_orgs, collapse = ", ")))


# ---- Estimates -----------------------------------------------------------------------------

# "all" (pooled) = the children of export_orgs only, so never organisation 12
estimates <- kaart_run_cohorts(cohorts, outcomes, unlist(cfg$export_orgs), region_types, groups,
                               conf_level = cfg$kaart_conf_level)
saveRDS(estimates, file.path(loc$kaart_folder, "jgz_kaart_estimates.rds"))


# ---- CBS output rules ----------------------------------------------------------------------

results <- kaart_disclosure(estimates,
                            min_n_mean = cfg$kaart_min_n_mean, min_n_regression = cfg$kaart_min_n_regression,
                            min_count = cfg$kaart_min_count, round_n = cfg$kaart_round_n)
saveRDS(results, file.path(loc$kaart_folder, "jgz_kaart_results.rds"))
write.csv(results, file.path(loc$kaart_folder, "jgz_kaart_results.csv"), row.names = FALSE, na = "")

message("Estimates per region type and share published (not suppressed):")
print(results %>%
        group_by(sample, region_type) %>%
        summarise(areas = n_distinct(region_id), estimates = n(), published = mean(is.na(reason_of_na))))
message("Reasons for suppression:")
print(table(results$reason_of_na, useNA = "ifany"))

# ---- Internal run (internal_orgs, e.g. org 12): never export ------------------------------
if (length(unlist(cfg$internal_orgs)) > 0) {
  internal_folder <- ensure_dir(file.path(loc$kaart_folder, "internal"))
  message("INTERNAL run for organisation(s) ", paste(unlist(cfg$internal_orgs), collapse = ", "),
          " (not for export) -> ", internal_folder)
  est_int <- kaart_run_cohorts(cohorts, outcomes, unlist(cfg$internal_orgs), unlist(cfg$kaart_region_types),
                               groups, conf_level = cfg$kaart_conf_level)
  saveRDS(est_int, file.path(internal_folder, "INTERNAL_jgz_kaart_estimates.rds"))
  res_int <- kaart_disclosure(est_int,
                              min_n_mean = cfg$kaart_min_n_mean, min_n_regression = cfg$kaart_min_n_regression,
                              min_count = cfg$kaart_min_count, round_n = cfg$kaart_round_n)
  saveRDS(res_int, file.path(internal_folder, "INTERNAL_jgz_kaart_results.rds"))
}

message("Take-up per organisation (exact n, internal):")
take <- estimates[estimates$sample == "jgz_catchment" & estimates$region_type == "org" &
                    estimates$gender_group == "all", c("outcome", "income_group", "region_id", "n", "est")]
print(take, row.names = FALSE)

message(sprintf("JGZ kaart done in %.1f min", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
