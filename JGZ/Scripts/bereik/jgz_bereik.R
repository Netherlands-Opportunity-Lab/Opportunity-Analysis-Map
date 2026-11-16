# =============================================================================
# JGZ bereik (take-up) per organisation, PC3 and PC4
#
#   Input : loc$catchment_cohort_file (output/jgz_catchment_cohort.rds of
#           Opportunity-Preprocessing, cohort jgz_catchment: one row per child
#           living in a catchment area; y_m24/m36/m45_takeup,
#           y_m45_takeup_always, y_m45_takeup_any; org_catchment, postcode_4)
#   Output: loc$bereik_results_folder/
#             jgz_bereik_estimates.rds  exact n (internal, never export)
#             jgz_bereik_results.rds / .csv  after the CBS rules
#
# Bereik is about where the child lives, not which organisation saw it
# (27-10-2026), so the areas are the catchment area (org_catchment) and the
# child's PC4 / PC3, the same for every take-up outcome (same denominator).
# Estimate and CBS rules as the kaart (kansenkaart_expect() with income group
# "all" = the share; rules 1-4, n rounded). Only catchment areas of
# organisations in export_orgs.
#
# Run from the repository root:
#   setwd("H:/Opportunity-Analysis-Map"); source("JGZ/Scripts/bereik/jgz_bereik.R")
# =============================================================================

source("JGZ/Scripts/jgz_helpers.R")
source("JGZ/Scripts/kaart/jgz_kaart_functions.R")
source_expectation_function("src/income_parents")
source("JGZ/Scripts/bereik/jgz_bereik_functions.R")


ensure_dir(loc$bereik_results_folder)
catch <- readRDS(loc$catchment_cohort_file)
d_all <- bereik_data(catch, cfg$org_catchment_codes, cfg$export_orgs)
outcomes <- grep("^y_m[0-9]+_takeup", names(d_all), value = TRUE)

region_types <- unlist(cfg$bereik_region_types)
if (isTRUE(cfg$export_pooled)) region_types <- c("all", region_types)
groups <- kaart_groups("all", cfg$bereik_gender_groups, "all", "all")

message(sprintf("JGZ bereik: %d children in the catchment areas of organisations %s; outcomes %s",
                nrow(d_all), paste(cfg$export_orgs, collapse = ", "), paste(outcomes, collapse = ", ")))

estimates <- do.call(rbind, lapply(outcomes, function(oc) {
  d <- d_all[!is.na(d_all[[oc]]), , drop = FALSE]
  kaart_estimate_outcome(d, oc, region_types, groups, conf_level = cfg$kaart_conf_level)
}))
saveRDS(estimates, file.path(loc$bereik_results_folder, "jgz_bereik_estimates.rds"))

results <- kaart_disclosure(estimates,
                            min_n_mean = cfg$kaart_min_n_mean, min_n_regression = cfg$kaart_min_n_regression,
                            min_count = cfg$kaart_min_count, round_n = cfg$kaart_round_n)
saveRDS(results, file.path(loc$bereik_results_folder, "jgz_bereik_results.rds"))
write.csv(results, file.path(loc$bereik_results_folder, "jgz_bereik_results.csv"), row.names = FALSE, na = "")

message("Take-up per organisation (exact n, internal):")
print(estimates[estimates$region_type == "org", c("outcome", "region_id", "n", "est")])
message("Share published per region type:")
print(results %>% group_by(region_type) %>%
        summarise(areas = n_distinct(region_id), published = mean(is.na(reason_of_na)), .groups = "drop"))
