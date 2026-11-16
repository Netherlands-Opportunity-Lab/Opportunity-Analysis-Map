# JGZ bereik -- functions (used by JGZ/Scripts/bereik/jgz_bereik.R, tested in
# tests/testthat/test-jgz-bereik.R)

# The catchment cohort in the shape kaart_estimate_outcome() expects: one column
# per area type (org, pc3, pc4, all), org as the organisation code of the
# catchment area, only organisations in export_orgs.
bereik_data <- function(catch, org_codes, export_orgs) {
  for (v in c("org_catchment", "postcode_4")) if (!v %in% names(catch)) stop(v, " not in the catchment cohort")
  oc <- unlist(org_codes)
  d <- as.data.frame(catch)
  d$org <- unname(oc[as.character(haven::zap_labels(d$org_catchment))])
  pc4 <- as.integer(haven::zap_labels(d$postcode_4))
  d$pc4 <- as.character(pc4)
  d$pc3 <- as.character(pc4 %/% 10)
  d$all <- "all"
  d <- d[!is.na(d$org) & d$org %in% export_orgs, , drop = FALSE]
  d$org <- as.character(d$org)
  d
}
