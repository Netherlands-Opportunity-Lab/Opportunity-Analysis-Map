# JGZ analysis and export -- shared helpers
#
# Sourced at the top of the R scripts in JGZ/Scripts (map tables, gradients,
# export). Loads JGZ/config.yml and defines the disclosure helpers.
#
# Compatibility: CBS Remote Access runs R 3.6.2 with dplyr 1.0.2. So: no `|>`
# pipe, no `\(x)` lambdas, no dplyr::join_by()/if_any()/pick() and no
# `relationship =` argument in joins.

suppressPackageStartupMessages(library(dplyr))

if (!file.exists("JGZ/config.yml")) {
  stop("JGZ/config.yml not found. Run the JGZ scripts from the repository root ",
       "(setwd() to the Opportunity-Analysis-Map folder first).")
}
loc <- config::get("file_locations", file = "JGZ/config.yml")
cfg <- config::get("settings",       file = "JGZ/config.yml")

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

# Organisation codes used in the JGZ files
ORG_CODES <- c(LimburgNoord = 6, ZONL = 7, Verian = 10, Org4 = 12)   # 4th organisation: keep anonymous in public files


# ---- Disclosure control (map tables, export) ------------------------------------

# Suppress a statistic when the cell has fewer than `min_cell` observations.
suppress_small_n <- function(value, n, min_cell = cfg$min_cell) {
  ifelse(n < min_cell, NA, value)
}

# Suppress a count (and its percentage) when the count itself or its
# complement (n - count) is below `min_cell`.
suppress_count <- function(value, count, n, min_cell = cfg$min_cell) {
  ifelse(is.na(count) | count < min_cell | (n - count) < min_cell, NA, value)
}
