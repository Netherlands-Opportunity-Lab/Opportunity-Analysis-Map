# JGZ bereik (JGZ/Scripts/bereik/jgz_bereik_functions.R). Toy data, no CBS data.
# (The JGZ kloof is in Opportunity-Analysis-Gap/JGZ since 17-11-2026.)
root <- file.path("..", "..")
suppressPackageStartupMessages(library(dplyr))
source(file.path(root, "JGZ", "Scripts", "kaart", "jgz_kaart_functions.R"))
source(file.path(root, "JGZ", "Scripts", "bereik", "jgz_bereik_functions.R"))
source_expectation_function(file.path(root, "src", "income_parents"))

test_that("bereik_data maps catchment areas to organisation codes and keeps export orgs", {
  catch <- data.frame(org_catchment = c(1, 2, 3, 4, 2), postcode_4 = c(5911, 8301, 7311, 6211, 8302),
                      m024_takeup = c(1, 0, 1, 1, NA))
  d <- bereik_data(catch, list(`1` = 6, `2` = 7, `3` = 10, `4` = 12), c(7, 10))
  expect_equal(d$org, c("7", "10", "7")); expect_equal(d$pc3, c("830", "731", "830"))
  res <- kaart_estimate_outcome(d[!is.na(d$m024_takeup), ], "m024_takeup", c("org", "pc4"),
                                kaart_groups("all", "all", "all", "all"))
  expect_equal(res$est[res$region_type == "org" & res$region_id == "7"], 0)
  expect_equal(res$n[res$region_type == "org" & res$region_id == "7"], 1L)
})

test_that("bereik_data also takes org_catchment as organisation codes, and stops when nothing is left", {
  oc <- list(`1` = 6, `2` = 7, `3` = 10, `4` = 12)
  catch <- data.frame(org_catchment = c(6, 7, 10, 12, 7), postcode_4 = c(5911, 8301, 7311, 6211, 8302),
                      m024_takeup = 1)
  expect_equal(bereik_data(catch, oc, c(7, 10))$org, c("7", "10", "7"))
  expect_error(bereik_data(catch, oc, c(99)), "No children")
  expect_error(bereik_data(transform(catch, org_catchment = c(6, 7, 10, 12, 55)), oc, 7), "neither")
})

test_that("JGZ/config.yml has the bereik settings", {
  cfg <- config::get("settings", file = file.path(root, "JGZ", "config.yml"))
  loc <- config::get("file_locations", file = file.path(root, "JGZ", "config.yml"))
  for (k in c("bereik_region_types", "bereik_gender_groups", "org_catchment_codes")) expect_false(is.null(cfg[[k]]), info = k)
  for (k in c("catchment_cohort_file", "bereik_results_folder")) expect_false(is.null(loc[[k]]), info = k)
  expect_equal(unname(unlist(cfg$org_catchment_codes)), c(6, 7, 10, 12))
  expect_null(cfg$kloof_min_n_mean)   # kloof settings live in the Gap repo
})
