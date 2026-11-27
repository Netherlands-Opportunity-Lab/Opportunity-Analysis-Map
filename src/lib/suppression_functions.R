#-------------------------------------------------------------------------------
# Disclosure control: which cells are suppressed, and why (shared file)
#
# The SAME file is in Opportunity-Analysis-Gap, Opportunity-Analysis-Intragenerational
# and Opportunity-Analysis-Map (src/lib/suppression_functions.R). Change it in all
# three. Version: 2026-11-27.
#
# Every results table gets the column reason_of_na: ALL the reasons why a cell is
# not published, ordered by code and separated by "; ", or NA when the cell is
# published. Suppressed cells stay in the table with their statistics set to NA,
# so every missing number in an export can be traced to its reason(s).
#
# Reason codes (the same in every repository; the thresholds come from the config):
#   0. no estimate                        the model gave no estimate (Map)
#   0. not computed: fewer than k obs in the subgroup
#                                         the subgroup is too small to compute the mean (Gap, Intragenerational)
#   1. fewer than k obs                   N below the minimum per cell
#   2. est*n < k                          binary outcome: too few cases
#   3. (1-est)*n < k                      binary outcome: too few non-cases
#   4. est == 0                           continuous estimate of exactly 0 (Map; not for _class_ and _neighborhood_ outcomes)
#   5. n of school < 3                    PC4 areas with fewer than 3 schools (Map, _class_ outcomes)
#   6. sd == 0                            no variation in the outcome (Intragenerational)
#   7. fewer than k non-zero x values     (Intragenerational)
#   8. other cell of the gradient or group suppressed
#                                         complementary suppression: a gradient (all its bins)
#                                         or a group (all its categories) is published whole or not at all
#
# Base R only (runs on data.frames, tibbles and tidytables, in and outside the RA).
#-------------------------------------------------------------------------------

REASON_SEP <- "; "

REASON_NO_ESTIMATE    <- "0. no estimate"
REASON_EST_ZERO       <- "4. est == 0"
REASON_FEW_SCHOOLS    <- "5. n of school < 3"
REASON_SD_ZERO        <- "6. sd == 0"
REASON_COMPLEMENTARY  <- "8. other cell of the gradient or group suppressed"

reason_not_computed <- function(min_n) paste0("0. not computed: fewer than ", min_n, " obs in the subgroup")
reason_few_obs      <- function(min_n) paste0("1. fewer than ", min_n, " obs")
reason_few_cases    <- function(min_count) paste0("2. est*n < ", min_count)
reason_few_noncases <- function(min_count) paste0("3. (1-est)*n < ", min_count)
reason_few_nonzero  <- function(min_nonzero) paste0("7. fewer than ", min_nonzero, " non-zero x values")


# ---- Building blocks ----------------------------------------------------------

# Append `code` to the reasons of the cells where `rule` is TRUE (NA counts as
# FALSE). `code` is one string or one per cell. Apply the rules in the order of
# their codes, so the reasons of a cell come out ordered.
add_reason <- function(reason, rule, code) {
  rule <- rule %in% TRUE
  if (length(rule) != length(reason)) stop("add_reason(): rule and reason differ in length")
  if (!any(rule)) return(reason)
  code <- rep_len(as.character(code), length(reason))[rule]
  old <- reason[rule]
  reason[rule] <- ifelse(is.na(old), code, paste0(old, REASON_SEP, code))
  reason
}

# Complementary suppression within ONE gradient or group: when any cell is
# suppressed (or `force`, e.g. the group is incomplete), the cells that are
# still published get code 8.
add_complementary <- function(reason, force = FALSE) {
  if (force || any(!is.na(reason))) reason[is.na(reason)] <- REASON_COMPLEMENTARY
  reason
}

# The reasons already in the table (from an earlier step), NA when there are none
current_reasons <- function(data) {
  if ("reason_of_na" %in% names(data)) as.character(data$reason_of_na) else rep(NA_character_, nrow(data))
}

# Is the reason "not computed" (code 0, subgroup too small)?
is_not_computed <- function(reason) !is.na(reason) & startsWith(reason, "0. not computed")

# Store `reason` in reason_of_na and set `stat_cols` to NA in every cell with a
# reason. Returns the table in the class it came in (data.frame, tibble, tidytable).
suppress_cells <- function(data, reason, stat_cols) {
  if (length(reason) != nrow(data)) stop("suppress_cells(): one reason per row needed")
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  out$reason_of_na <- as.character(reason)
  hide <- !is.na(out$reason_of_na)
  for (cl in intersect(stat_cols, names(out))) {
    out[[cl]][hide] <- NA
  }
  as_input_class(out, data)
}

as_input_class <- function(data, like) {
  if (inherits(like, "tidytable")) return(tidytable::as_tidytable(data))
  if (inherits(like, "data.table")) return(data.table::as.data.table(data))
  if (inherits(like, "tbl_df")) return(tibble::as_tibble(data))
  data
}

# Complementary suppression over many groups at once: within every group of
# `keys`, when any cell is suppressed or the group has fewer than `n_expected`
# rows, the published cells get code 8 and are set to NA.
complete_groups <- function(data, keys, stat_cols, n_expected = NULL) {
  if (nrow(data) == 0) return(data)
  reason <- current_reasons(data)
  key <- do.call(paste, c(lapply(as.data.frame(data)[keys], as.character), sep = "\r"))
  any_suppressed <- ave(as.numeric(!is.na(reason)), key, FUN = max) > 0
  incomplete <- if (is.null(n_expected)) FALSE else ave(rep(1, length(key)), key, FUN = length) < n_expected
  reason <- add_reason(reason, is.na(reason) & (any_suppressed | incomplete), REASON_COMPLEMENTARY)
  suppress_cells(data, reason, stat_cols)
}


# ---- Rules for gradients and means (Gap, Intragenerational) ---------------------

# Reasons for the cells of ONE gradient or group: the bins of one subgroup x
# outcome, the categories of a categorical x variable, or the single row of a
# mean. Every rule is checked on the cell's own (unsuppressed) values; when any
# cell fails, the other cells get code 8 (published whole or not at all).
# `reason`: reasons from an earlier step (kept, the new ones are added).
# A binary outcome is recognised as before: every mean known and at most 1.
gradient_reasons <- function(N, mean, min_n, min_count, sd = NULL, n_nonzero = NULL, min_nonzero = NULL,
                             reason = rep(NA_character_, length(N))) {
  reason <- add_reason(reason, N < min_n, reason_few_obs(min_n))
  if (length(mean) > 0 && all(!is.na(mean)) && all(mean <= 1)) {
    reason <- add_reason(reason, N * mean < min_count, reason_few_cases(min_count))
    reason <- add_reason(reason, N * (1 - mean) < min_count, reason_few_noncases(min_count))
  }
  if (!is.null(sd)) reason <- add_reason(reason, sd == 0, REASON_SD_ZERO)
  if (!is.null(n_nonzero)) reason <- add_reason(reason, n_nonzero < min_nonzero, reason_few_nonzero(min_nonzero))
  add_complementary(reason)
}


# ---- Rules for the kansenkaart (Map, JGZ kaart and bereik) -----------------------

# Reasons for every cell of a kansenkaart estimate table (columns n, est,
# binary, outcome; region_type and region_id for rule 5). `regression`: TRUE for
# cells estimated with a regression (Low/Mid/High), which need min_n_regression
# observations (one more degree of freedom); FALSE for means (min_n_mean).
kaart_reasons <- function(data, regression, min_n_mean = 11, min_n_regression = 12, min_count = 10,
                          school_postcodes = NULL) {
  n <- data$n
  est <- data$est
  binary <- as.logical(as.character(data$binary)) %in% TRUE
  outcome <- if (is.null(data$outcome)) rep("", length(n)) else as.character(data$outcome)
  regression <- rep_len(regression %in% TRUE, length(n))
  min_n <- ifelse(regression, min_n_regression, min_n_mean)

  reason <- rep(NA_character_, length(n))
  reason <- add_reason(reason, is.na(est), REASON_NO_ESTIMATE)
  reason <- add_reason(reason, n < min_n, reason_few_obs(min_n))
  reason <- add_reason(reason, binary & est * n < min_count, reason_few_cases(min_count))
  reason <- add_reason(reason, binary & (1 - est) * n < min_count, reason_few_noncases(min_count))
  reason <- add_reason(reason, !binary & est == 0 & !grepl("_class_|_neighborhood_", outcome), REASON_EST_ZERO)
  if (length(school_postcodes) > 0) {
    reason <- add_reason(reason, grepl("_class_", outcome) & as.character(data$region_type) == "postcode4" &
                           as.character(data$region_id) %in% school_postcodes, REASON_FEW_SCHOOLS)
  }
  reason
}

# Apply the kansenkaart rules: reason_of_na with all reasons, est, lwr, upr and
# n set to NA in suppressed cells, n rounded to `round_n`.
kaart_suppress <- function(data, regression, min_n_mean = 11, min_n_regression = 12, min_count = 10,
                           school_postcodes = NULL, round_n = 10) {
  reason <- kaart_reasons(data, regression, min_n_mean = min_n_mean, min_n_regression = min_n_regression,
                          min_count = min_count, school_postcodes = school_postcodes)
  data <- suppress_cells(data, reason, c("est", "lwr", "upr", "n"))
  data$n <- as.integer(round(data$n / round_n) * round_n)
  data
}
