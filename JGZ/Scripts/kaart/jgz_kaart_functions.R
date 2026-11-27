# JGZ kaart -- functions
#
# Used by JGZ/Scripts/kaart/jgz_kaart.R; tested in tests/testthat/test-jgz-kaart.R.
# The estimate itself is the national kansenkaart function kansenkaart_expect()
# (src/income_parents/*_01_expectation_function_income.R), so the JGZ kaart uses
# exactly the same model as kansenkaart.nl. What is JGZ-specific is only which
# children and which area columns go in:
#   - every age has its own area columns (pc3_m<age>, pc4_m<age>, org_m<age>:
#     the GBA address and organisation at the contact used for that age);
#   - only children seen by an organisation in export_orgs are used;
#   - only children with a value for the outcome are passed, because
#     kansenkaart_expect() reports n = nrow(data), not the number used by lm();
#   - two cohort files (27-11-2026): the JGZ cohort (children seen by the JGZ,
#     the measured outcomes) and the catchment cohort (children living in a
#     catchment area, seen or not: the bereik / take-up outcomes m<age>_takeup*).
#     Bastian (20-11-2026): bereik is a dummy outcome like the others, so it is
#     estimated per area and income group with the same function and rules.
#     Both files have the same area, income and group columns; each outcome is
#     estimated on the file it is in (kaart_run_cohorts()).


# Load kansenkaart_expect() from the national kaart scripts. The version
# prefix (A_2_0_M1_) exists on GitHub but not in the RA, so match on the end.
source_expectation_function <- function(dir = "src/income_parents") {
  f <- list.files(dir, "01_expectation_function_income\\.R$", full.names = TRUE)
  if (length(f) != 1) stop("Expected one *01_expectation_function_income.R in ", dir, ", found ", length(f))
  source(f, local = globalenv())
  invisible(f)
}

# Outcome names (20-11-2026): the age in months first, three digits, as c16_
# in the other cohorts: "m024_bmi_p90". Cohort files from before 20-11-2026
# have "y_m24_bmi_p90"; both are recognised.
OUTCOME_PATTERN <- "^(y_)?m[0-9]+_"

# "m024_bmi_p90" -> "m024" (old: "y_m24_bmi_p90" -> "m24")
outcome_age <- function(outcome) sub("^(y_)?(m[0-9]+)_.*$", "\\2", outcome)

# Outcomes in the cohort file: every m<age>_* column, or the configured list
kaart_outcome_list <- function(cohort, setting = "all") {
  all_y <- grep(OUTCOME_PATTERN, names(cohort), value = TRUE)
  if (identical(setting, "all")) return(all_y)
  missing <- setdiff(unlist(setting), all_y)
  if (length(missing) > 0) stop("kaart_outcomes not in the cohort file: ", paste(missing, collapse = ", "))
  unlist(setting)
}

# Binary = only 0/1 values (flags, Wiechen, language delay, visus); otherwise
# continuous (BMI, SDQ total score). Decides which CBS rules apply.
is_binary <- function(x) {
  v <- unique(x[!is.na(x)])
  length(v) > 0 && all(v %in% c(0, 1))
}

# Columns kansenkaart_expect() filters on
GROUP_COLUMNS <- c("income_parents_perc", "income_group", "income_group_tails", "sex",
                   "migration_background", "has_migration", "type_household")

# The parental income columns for one outcome age. Since 20-11-2026 the JGZ
# cohort has parental income per outcome age (income_parents_perc_<age>,
# income_group_<age>, income_group_tails_<age>: mean over the 5 years up to the
# year the child reaches that age, ranked among all children in NL reaching it
# that year). Older cohort files have one income per child (no suffix).
income_columns <- function(cohort, age) {
  by_age <- c(income_parents_perc = paste0("income_parents_perc_", age),
              income_group        = paste0("income_group_", age),
              income_group_tails  = paste0("income_group_tails_", age))
  if (by_age[["income_parents_perc"]] %in% names(cohort)) return(by_age[by_age %in% names(cohort)])
  if (any(grepl("^income_parents_perc_m[0-9]+$", names(cohort)))) {
    stop("The cohort file has parental income per age, but not for ", age, ".")
  }
  generic <- c(income_parents_perc = "income_parents_perc", income_group = "income_group",
               income_group_tails = "income_group_tails")
  generic[generic %in% names(cohort)]
}

# Children with a value for `outcome` and a parental income at that age, seen at
# that age by an organisation in `export_orgs`, with that age's area columns
# renamed to org / pc3 / pc4, that age's income columns renamed to
# income_parents_perc / income_group / income_group_tails, and a constant
# column all = "all" (for pooled results).
kaart_outcome_data <- function(cohort, outcome, export_orgs) {
  age <- outcome_age(outcome)
  area_cols <- c(org = paste0("org_", age), pc3 = paste0("pc3_", age), pc4 = paste0("pc4_", age))
  missing <- setdiff(area_cols, names(cohort))
  if (length(missing) > 0) stop(outcome, ": area columns missing in the cohort file: ", paste(missing, collapse = ", "),
                                " (a catchment cohort file from before 27-11-2026? Rerun jgz_catchment 02-04 in Opportunity-Preprocessing)")
  inc_cols <- income_columns(cohort, age)
  if (!"income_parents_perc" %in% names(inc_cols)) stop(outcome, ": no parental income column in the cohort file.")
  keep <- setdiff(intersect(GROUP_COLUMNS, names(cohort)), c(names(inc_cols), unname(inc_cols)))
  rows <- !is.na(cohort[[outcome]]) & !is.na(cohort[[inc_cols[["income_parents_perc"]]]])
  d <- cohort[rows, c(outcome, unname(area_cols), unname(inc_cols), keep), drop = FALSE]
  names(d)[match(area_cols, names(d))] <- names(area_cols)
  names(d)[match(inc_cols, names(d))] <- names(inc_cols)
  d <- d[!is.na(d$org) & d$org %in% export_orgs, , drop = FALSE]
  d$org <- as.character(d$org)
  d$pc3 <- as.character(d$pc3)
  d$pc4 <- as.character(d$pc4)
  d$all <- "all"
  d
}

# Every combination of the configured groups
kaart_groups <- function(income, gender, migration, household) {
  expand.grid(income_group = unlist(income), gender_group = unlist(gender),
              migration_group = unlist(migration), household_group = unlist(household),
              stringsAsFactors = FALSE)
}

# Estimates for one outcome: every area of every region type x every group.
# The data are split by area first, so kansenkaart_expect() only filters the
# children of one area (this is what keeps the run time short). Areas with a
# missing code (no GBA address at the contact) are left out.
kaart_estimate_outcome <- function(d, outcome, region_types, groups, conf_level = 0.95) {
  binary <- is_binary(d[[outcome]])
  out <- list()
  for (rt in region_types) {
    d_rt <- d[!is.na(d[[rt]]) & d[[rt]] != "NA", , drop = FALSE]
    parts <- split(d_rt, d_rt[[rt]])
    for (id in names(parts)) {
      dd <- parts[[id]]
      res <- lapply(seq_len(nrow(groups)), function(i) {
        r <- kansenkaart_expect(dd, outcome,
                                income_grp = groups$income_group[i], migration_grp = groups$migration_group[i],
                                gender_grp = groups$gender_group[i], hh_grp = groups$household_group[i],
                                conf_level = conf_level)
        c(r$est, r$lwr, r$upr, r$n)
      })
      m <- do.call(rbind, res)
      out[[length(out) + 1]] <- data.frame(
        outcome = outcome, binary = binary, groups,
        region_type = rt, region_id = id,
        n = as.integer(m[, 4]), est = m[, 1], lwr = m[, 2], upr = m[, 3],
        stringsAsFactors = FALSE)
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# Estimates for every outcome, for the children of organisations `orgs`
kaart_run <- function(cohort, outcomes, orgs, region_types, groups, conf_level = 0.95) {
  estimates <- vector("list", length(outcomes))
  for (k in seq_along(outcomes)) {
    oc <- outcomes[k]
    t0 <- Sys.time()
    d <- kaart_outcome_data(cohort, oc, orgs)
    est_k <- kaart_estimate_outcome(d, oc, region_types, groups, conf_level = conf_level)
    # `estimates[[k]] <- NULL` would delete element k and shorten the list
    # (subscript out of bounds later on); empty outcomes stay NULL instead.
    if (!is.null(est_k)) estimates[[k]] <- est_k
    message(sprintf("  [%d/%d] %s: %d children, %d estimates, %.1f s", k, length(outcomes), oc, nrow(d),
                    if (is.null(est_k)) 0L else nrow(est_k),
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  do.call(rbind, estimates)
}

# The cohort files of the kaart (27-11-2026), as a named vector sample -> path:
# jgz = the JGZ cohort (measured outcomes), jgz_catchment = the catchment cohort
# (bereik / take-up outcomes). Both are outputs of Opportunity-Preprocessing.
kaart_cohort_files <- function(loc) {
  files <- c(jgz = loc$cohort_file, jgz_catchment = loc$catchment_cohort_file)
  if (length(files) != 2) stop("Set both cohort_file and catchment_cohort_file in JGZ/config.yml (file_locations).")
  files
}

# Outcomes per cohort: a named list sample -> outcomes. "all" = every m<age>_*
# column of each file; a list = those outcomes, each taken from the file it is
# in. An outcome name may occur in one file only (otherwise it would be unclear
# which denominator its results have).
kaart_outcomes_by_cohort <- function(cohorts, setting = "all") {
  per <- lapply(cohorts, kaart_outcome_list, setting = "all")
  all_y <- unlist(per, use.names = FALSE)
  dup <- unique(all_y[duplicated(all_y)])
  if (length(dup) > 0) stop("Outcome(s) in more than one cohort file: ", paste(dup, collapse = ", "))
  if (identical(setting, "all")) return(per)
  wanted <- unlist(setting)
  missing <- setdiff(wanted, all_y)
  if (length(missing) > 0) stop("kaart_outcomes not in the cohort files: ", paste(missing, collapse = ", "))
  lapply(per, function(y) intersect(wanted, y))
}

# kaart_run() for every cohort (names(cohorts) = the samples), one table with a
# first column `sample` (jgz / jgz_catchment): the take-up outcomes are shares
# of all children living in the area, the other outcomes of the children seen.
kaart_run_cohorts <- function(cohorts, outcomes_by_cohort, orgs, region_types, groups, conf_level = 0.95) {
  out <- lapply(names(cohorts), function(s) {
    oc <- outcomes_by_cohort[[s]]
    if (length(oc) == 0) return(NULL)
    est <- kaart_run(cohorts[[s]], oc, orgs, region_types, groups, conf_level = conf_level)
    if (is.null(est) || nrow(est) == 0) return(NULL)
    cbind(sample = s, est, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# CBS output rules of the national kaart post-processing
# (src/<ses>_parents/*_05_post_process_<ses>.R): the same function,
# kaart_suppress() in the shared file src/lib/suppression_functions.R (source it
# first). reason_of_na holds ALL the reasons of a cell, ordered by code
# ("0. no estimate", "1. fewer than 11 obs", "2. est*n < 10", "3. (1-est)*n < 10",
# "4. est == 0"), separated by "; " (27-11-2026; before, only the first rule
# that applied). Rule 5 there (fewer than 3 schools) has no JGZ equivalent.
# Suppressed cells get est, lwr, upr and n = NA; n is rounded to tens.
kaart_disclosure <- function(res, min_n_mean = 11, min_n_regression = 12, min_count = 10, round_n = 10) {
  kaart_suppress(res, regression = res$income_group != "all", min_n_mean = min_n_mean,
                 min_n_regression = min_n_regression, min_count = min_count, round_n = round_n)
}
