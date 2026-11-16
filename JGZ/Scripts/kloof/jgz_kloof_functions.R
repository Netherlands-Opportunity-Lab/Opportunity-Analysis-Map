# JGZ kloof -- functions
#
# Used by JGZ/Scripts/kloof/jgz_kloof.R; tested in tests/testthat/test-jgz-kloof.R.
# Same method as the national kansenkloof (Opportunity-Analysis-Gap,
# src/A_2_0_G1_02a_gradients_parent_income_wealth.R and src/lib/A_2_0_G1_*_functions.R):
#   - per subgroup the mean, and the outcome per parental income (or wealth) bin:
#     dplyr::ntile() of the raw parental measure within the subgroup;
#   - 20 bins when the subgroup has >= min_n_bins_20 children, 10 and 5 bins then
#     averaged from the 20 (pairs / fours of bins); with fewer children 10 or 5
#     bins directly, as the Gap script does;
#   - CBS rules: N >= cbs_min_n_cell behind every number, and for binary outcomes
#     N * mean and N * (1 - mean) >= cbs_min_n_category; if one bin fails, the
#     whole set of bins of that subgroup is suppressed.
# Differences, all JGZ-specific: the geography is the organisation of the
# contact used for that age (org_m<age>, only export_orgs; pooled only if
# export_pooled), only children with a value for the outcome are counted, the
# migration labels are those of the cohort file ("Turkiye", has_migration), and
# whether an outcome is binary is read from its values (0/1) instead of from
# "all means <= 1". Rewritten in base R + dplyr (no tidytable / RcppRoll).


# "0% - 5%", "5% - 10%", ... as in Gap's bins_check()
kloof_bin_labels <- function(n_bins) {
  w <- 100 / n_bins
  paste0((seq_len(n_bins) - 1) * w, "% - ", seq_len(n_bins) * w, "%")
}

# Subgroups: every combination of geography x sex x migration x household.
# Values as in Gap's filter_options ("Total" = everyone).
kloof_subgroups <- function(geography, sex, migration, household) {
  expand.grid(geography = as.character(unlist(geography)), sex = unlist(sex),
              migration_background = unlist(migration), type_household = unlist(household),
              stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
}

# Rows of `d` in subgroup `g` (one row of kloof_subgroups()). Geography "pooled"
# = all export organisations together.
kloof_filter <- function(d, g) {
  keep <- rep(TRUE, nrow(d))
  if (g$geography != "pooled") keep <- keep & d$org == g$geography
  if (g$sex != "Total") keep <- keep & d$sex %in% g$sex
  mb <- g$migration_background
  if (mb == "With Migration Background") keep <- keep & d$has_migration %in% 1
  else if (mb != "Total") keep <- keep & d$migration_background %in% mb
  if (g$type_household != "Total") keep <- keep & d$type_household %in% g$type_household
  d[keep, , drop = FALSE]
}

# Mean of the whole subgroup (Gap's compute_mean)
kloof_mean <- function(y, x) {
  data.frame(bin = "Total", N = sum(!is.na(y)), mean = mean(y, na.rm = TRUE),
             parent_income_wealth = mean(x, na.rm = TRUE), sd = stats::sd(y, na.rm = TRUE),
             stringsAsFactors = FALSE)
}

# Outcome per bin of the parental measure (Gap's compute_bins). Children without
# a value for the parental measure are left out of the bins, as ntile() gives
# them bin NA and Gap keeps only bins 1..n.
kloof_bins <- function(y, x, n_bins) {
  ok <- !is.na(x)
  y <- y[ok]; x <- x[ok]
  b <- dplyr::ntile(x, n_bins)
  out <- data.frame(bin = seq_len(n_bins),
                    N = as.integer(tabulate(b, n_bins)),
                    mean = as.numeric(tapply(y, factor(b, levels = seq_len(n_bins)), mean)),
                    parent_income_wealth = as.numeric(tapply(x, factor(b, levels = seq_len(n_bins)), mean)),
                    sd = as.numeric(tapply(y, factor(b, levels = seq_len(n_bins)), stats::sd)))
  out
}

# Fewer bins from more (Gap's approx_bins): consecutive groups of bins, N summed,
# mean / income / sd averaged (ntile bins differ by at most one child in size)
kloof_approx_bins <- function(bins, n_bins) {
  k <- nrow(bins) / n_bins
  grp <- rep(seq_len(n_bins), each = k)
  data.frame(bin = seq_len(n_bins),
             N = as.integer(tapply(bins$N, grp, sum)),
             mean = as.numeric(tapply(bins$mean, grp, mean)),
             parent_income_wealth = as.numeric(tapply(bins$parent_income_wealth, grp, mean)),
             sd = as.numeric(tapply(bins$sd, grp, mean)))
}

# CBS rules on one table (the mean, or one set of bins): suppress the whole table
# when any row fails (Gap's mean_check / bins_check)
kloof_check <- function(tab, binary, min_n_cell = 50, min_n_category = 10) {
  tab$reason_of_na <- NA_character_
  fail <- NA_character_
  if (any(tab$N < min_n_cell, na.rm = TRUE)) {
    fail <- paste0("N < ", min_n_cell)
  } else if (binary && any(tab$N * tab$mean < min_n_category | tab$N * (1 - tab$mean) < min_n_category, na.rm = TRUE)) {
    fail <- paste0("N*mean or N*(1-mean) < ", min_n_category)
  }
  if (!is.na(fail)) {
    tab[, c("N", "mean", "parent_income_wealth", "sd")] <- NA
    tab$reason_of_na <- fail
  }
  tab
}

# All tables (mean, 5, 10, 20 bins) for one subgroup, with the Gap thresholds.
# Returns NULL when the subgroup is below min_n_mean (Gap writes nothing then).
kloof_subgroup_tables <- function(y, x, binary, s) {
  n <- length(y)
  if (n < s$kloof_min_n_mean) return(NULL)
  tabs <- list(mean = kloof_check(kloof_mean(y, x), binary, s$kloof_cbs_min_n_cell, s$kloof_cbs_min_n_category))
  if (n >= s$kloof_min_n_bins_20) {
    b20 <- kloof_bins(y, x, 20)
    raw <- list(`20` = b20, `10` = kloof_approx_bins(b20, 10), `5` = kloof_approx_bins(b20, 5))
  } else if (n >= s$kloof_min_n_bins_10) {
    b10 <- kloof_bins(y, x, 10)
    raw <- list(`10` = b10, `5` = kloof_approx_bins(b10, 5))
  } else if (n >= s$kloof_min_n_bins_5) {
    raw <- list(`5` = kloof_bins(y, x, 5))
  } else {
    raw <- list()
  }
  for (nb in names(raw)) {
    t <- kloof_check(raw[[nb]], binary, s$kloof_cbs_min_n_cell, s$kloof_cbs_min_n_category)
    t$bin <- kloof_bin_labels(as.integer(nb))
    tabs[[nb]] <- t
  }
  out <- do.call(rbind, lapply(names(tabs), function(nb) cbind(bins = nb, tabs[[nb]], stringsAsFactors = FALSE)))
  out$bin <- as.character(out$bin)
  out
}

# Everything for one outcome and one parental measure (column `measure_col`)
kloof_outcome <- function(d, outcome, measure_col, measure_label, subgroups, s) {
  binary <- is_binary(d[[outcome]])
  out <- list()
  for (i in seq_len(nrow(subgroups))) {
    g <- subgroups[i, ]
    dd <- kloof_filter(d, g)
    t <- kloof_subgroup_tables(dd[[outcome]], dd[[measure_col]], binary, s)
    if (is.null(t)) next
    out[[length(out) + 1]] <- cbind(outcome = outcome, binary = binary, measure = measure_label,
                                    g, t, stringsAsFactors = FALSE, row.names = NULL)
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}
