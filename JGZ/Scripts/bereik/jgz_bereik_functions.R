# JGZ bereik -- functions (used by JGZ/Scripts/bereik/jgz_bereik.R, tested in
# tests/testthat/test-jgz-bereik.R)

# The catchment cohort in the shape kaart_estimate_outcome() expects: one column
# per area type (org, pc3, pc4, all), org as the organisation code of the
# catchment area, only organisations in export_orgs.
# org_catchment is the organisation code (6/7/10/12) in the R rebuild of the
# catchment sample (jgz_catchment_cohort.rds); Coen's Stata file numbered the
# areas 1-4. Both are accepted: values that are organisation codes are used as
# they are, values 1-4 are translated with `org_codes` (names = 1-4).
bereik_data <- function(catch, org_codes, export_orgs) {
  for (v in c("org_catchment", "postcode_4")) if (!v %in% names(catch)) stop(v, " not in the catchment cohort")
  oc <- unlist(org_codes)
  d <- as.data.frame(catch)
  raw <- as.numeric(haven::zap_labels(d$org_catchment))
  vals <- unique(raw[!is.na(raw)])
  if (all(vals %in% oc)) {
    d$org <- raw
  } else if (all(as.character(vals) %in% names(oc))) {
    d$org <- unname(oc[as.character(raw)])
  } else {
    stop("org_catchment has values that are neither organisation codes (", paste(oc, collapse = ", "),
         ") nor area numbers (", paste(names(oc), collapse = ", "), "): ", paste(sort(vals), collapse = ", "))
  }
  pc4 <- as.integer(haven::zap_labels(d$postcode_4))
  d$pc4 <- as.character(pc4)
  d$pc3 <- as.character(pc4 %/% 10)
  d$all <- "all"
  d <- d[!is.na(d$org) & d$org %in% export_orgs, , drop = FALSE]
  if (nrow(d) == 0) stop("No children of organisations ", paste(export_orgs, collapse = ", "),
                         " in the catchment cohort (org_catchment values: ", paste(sort(vals), collapse = ", "), ")")
  d$org <- as.character(d$org)
  d
}
