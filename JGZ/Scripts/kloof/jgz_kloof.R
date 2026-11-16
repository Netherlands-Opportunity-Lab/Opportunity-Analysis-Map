# =============================================================================
# JGZ kloof: outcome per parental income / wealth bin, kansenkloof method
#
#   Input : loc$cohort_file (output/jgz_cohort.rds of Opportunity-Preprocessing)
#   Output: loc$kloof_folder/
#             jgz_kloof_results.rds / .csv   mean and 5/10/20 bins per subgroup,
#                                            after the CBS rules (reason_of_na)
#
# Per outcome, parental measure and subgroup (organisation x sex x migration x
# household type): the mean, and the mean per bin of the parental measure
# (ntile within the subgroup), as Opportunity-Analysis-Gap. Functions and the
# differences with Gap: JGZ/Scripts/kloof/jgz_kloof_functions.R. Settings:
# JGZ/config.yml, "kloof".
#
# Only children whose contact for that age was with an organisation in
# export_orgs (org_m<age>); pooled over organisations only if export_pooled.
#
# Run from the repository root:
#   setwd("H:/Opportunity-Analysis-Map"); source("JGZ/Scripts/kloof/jgz_kloof.R")
# =============================================================================

source("JGZ/Scripts/jgz_helpers.R")
source("JGZ/Scripts/kaart/jgz_kaart_functions.R")   # outcome list, kaart_outcome_data(), is_binary()
source("JGZ/Scripts/kloof/jgz_kloof_functions.R")

ensure_dir(loc$kloof_folder)
t_start <- Sys.time()

cohort <- readRDS(loc$cohort_file)
outcomes <- kaart_outcome_list(cohort, cfg$kloof_outcomes)
measures <- cfg$kloof_parental_measures              # e.g. income: income_parents
missing <- setdiff(unlist(measures), names(cohort))
if (length(missing) > 0) stop("Parental measure columns not in the cohort file: ", paste(missing, collapse = ", "))

geography <- as.character(cfg$export_orgs)
if (isTRUE(cfg$export_pooled)) geography <- c("pooled", geography)
subgroups <- kloof_subgroups(geography, cfg$kloof_sex, cfg$kloof_migration_background, cfg$kloof_type_household)

message(sprintf("JGZ kloof: %d children, %d outcomes, measures %s, %d subgroups (organisations %s)",
                nrow(cohort), length(outcomes), paste(names(measures), collapse = "/"), nrow(subgroups),
                paste(geography, collapse = ", ")))

results <- list()
for (k in seq_along(outcomes)) {
  oc <- outcomes[k]
  t0 <- Sys.time()
  d <- kaart_outcome_data(cohort, oc, cfg$export_orgs, extra = unlist(measures))
  for (m in names(measures)) {
    results[[length(results) + 1]] <- kloof_outcome(d, oc, measures[[m]], m, subgroups, cfg)
  }
  message(sprintf("  [%d/%d] %s: %d children, %.1f s", k, length(outcomes), oc, nrow(d),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
results <- do.call(rbind, results)
results$sample <- "jgz"

saveRDS(results, file.path(loc$kloof_folder, "jgz_kloof_results.rds"))
write.csv(results, file.path(loc$kloof_folder, "jgz_kloof_results.csv"), row.names = FALSE, na = "")

message("Rows per number of bins and share published:")
print(results %>% group_by(measure, bins) %>%
        summarise(rows = n(), subgroups = n_distinct(paste(outcome, geography, sex, migration_background, type_household)),
                  published = mean(is.na(reason_of_na)), .groups = "drop"))
message(sprintf("JGZ kloof done in %.1f min", as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
