# JGZ kloof (JGZ/Scripts/kloof/jgz_kloof_functions.R) and bereik
# (JGZ/Scripts/bereik/jgz_bereik_functions.R). Toy data, no CBS data.
root <- file.path("..", "..")
suppressPackageStartupMessages(library(dplyr))
source(file.path(root, "JGZ", "Scripts", "kaart", "jgz_kaart_functions.R"))
source(file.path(root, "JGZ", "Scripts", "kloof", "jgz_kloof_functions.R"))
source(file.path(root, "JGZ", "Scripts", "bereik", "jgz_bereik_functions.R"))
source_expectation_function(file.path(root, "src", "income_parents"))

s <- list(kloof_min_n_mean = 50, kloof_min_n_bins_5 = 250, kloof_min_n_bins_10 = 500,
          kloof_min_n_bins_20 = 1000, kloof_cbs_min_n_cell = 50, kloof_cbs_min_n_category = 10)

test_that("bin labels as Gap", {
  expect_equal(kloof_bin_labels(5), c("0% - 20%", "20% - 40%", "40% - 60%", "60% - 80%", "80% - 100%"))
  expect_equal(kloof_bin_labels(20)[c(1, 20)], c("0% - 5%", "95% - 100%"))
})

test_that("bins: ntile of the parental measure, approximated 10/5 bins from 20", {
  set.seed(2)
  x <- rlnorm(2000, 10); y <- as.numeric(x > median(x)) * 2 + rnorm(2000)
  b20 <- kloof_bins(y, x, 20)
  expect_equal(sum(b20$N), 2000); expect_true(all(b20$N == 100))
  expect_equal(b20$mean[1], mean(y[dplyr::ntile(x, 20) == 1]))
  b5 <- kloof_approx_bins(b20, 5)
  expect_equal(b5$N, rep(400L, 5)); expect_equal(b5$mean[1], mean(b20$mean[1:4]))
  # children without the parental measure are not in the bins
  expect_equal(sum(kloof_bins(y, replace(x, 1:10, NA), 20)$N), 1990)
})

test_that("CBS rules suppress the whole table", {
  tab <- data.frame(bin = 1:2, N = c(60L, 49L), mean = c(.5, .5), parent_income_wealth = 1, sd = 1)
  expect_true(all(is.na(kloof_check(tab, TRUE)$mean)))
  tab$N <- c(60L, 60L); tab$mean <- c(.5, .1)                       # 6 cases in bin 2
  expect_equal(unique(kloof_check(tab, TRUE)$reason_of_na), "N*mean or N*(1-mean) < 10")
  expect_equal(kloof_check(tab, FALSE)$mean, c(.5, .1))              # continuous: only N matters
})

test_that("thresholds decide which bins are made", {
  set.seed(3)
  f <- function(n) unique(kloof_subgroup_tables(rnorm(n), runif(n), FALSE, s)$bins)
  expect_null(kloof_subgroup_tables(rnorm(40), runif(40), FALSE, s))
  expect_equal(f(300), c("mean", "5"))
  expect_equal(f(600), c("mean", "10", "5"))
  expect_equal(f(1200), c("mean", "20", "10", "5"))
})

test_that("subgroups filter on organisation, sex, migration and household", {
  d <- data.frame(org = c("7", "7", "10", "10"), sex = c("Men", "Women", "Men", "Men"),
                  migration_background = c("Turkiye", "No Migration Background", "Morocco", "No Migration Background"),
                  has_migration = c(1, 0, 1, 0), type_household = "Two Parents", stringsAsFactors = FALSE)
  sg <- kloof_subgroups(c("pooled", "7"), c("Total", "Men"), c("Total", "With Migration Background", "Turkiye"), "Total")
  n <- sapply(seq_len(nrow(sg)), function(i) nrow(kloof_filter(d, sg[i, ])))
  expect_equal(n[sg$geography == "pooled" & sg$sex == "Total" & sg$migration_background == "Total"], 4)
  expect_equal(n[sg$geography == "pooled" & sg$sex == "Total" & sg$migration_background == "With Migration Background"], 2)
  expect_equal(n[sg$geography == "7" & sg$sex == "Men" & sg$migration_background == "Turkiye"], 1)
})

test_that("kloof_outcome labels rows and counts only children with a value", {
  set.seed(4)
  n <- 3000
  d <- data.frame(org = rep(c("7", "10"), length.out = n), sex = rep(c("Men", "Women"), each = n / 2),
                  migration_background = "No Migration Background", has_migration = 0,
                  type_household = "Two Parents", income_parents = rlnorm(n, 10),
                  y_m24_bmi_p90 = rbinom(n, 1, .3), stringsAsFactors = FALSE)
  sg <- kloof_subgroups(c("7", "10"), c("Total", "Men"), "Total", "Total")
  res <- kloof_outcome(d, "y_m24_bmi_p90", "income_parents", "income", sg, s)
  expect_true(all(res$binary)); expect_equal(unique(res$measure), "income")
  tot <- res[res$geography == "7" & res$sex == "Total" & res$bins == "mean", ]
  expect_equal(tot$N, sum(d$org == "7")); expect_equal(tot$mean, mean(d$y_m24_bmi_p90[d$org == "7"]))
  expect_setequal(unique(res$bins[res$geography == "7" & res$sex == "Total"]), c("mean", "20", "10", "5"))
})

test_that("bereik_data maps catchment areas to organisation codes and keeps export orgs", {
  catch <- data.frame(org_catchment = c(1, 2, 3, 4, 2), postcode_4 = c(5911, 8301, 7311, 6211, 8302),
                      y_m24_takeup = c(1, 0, 1, 1, NA))
  d <- bereik_data(catch, list(`1` = 6, `2` = 7, `3` = 10, `4` = 12), c(7, 10))
  expect_equal(d$org, c("7", "10", "7")); expect_equal(d$pc3, c("830", "731", "830"))
  res <- kaart_estimate_outcome(d[!is.na(d$y_m24_takeup), ], "y_m24_takeup", c("org", "pc4"),
                                kaart_groups("all", "all", "all", "all"))
  expect_equal(res$est[res$region_type == "org" & res$region_id == "7"], 0)
  expect_equal(res$n[res$region_type == "org" & res$region_id == "7"], 1L)
})

test_that("JGZ/config.yml has the kloof and bereik settings", {
  cfg <- config::get("settings", file = file.path(root, "JGZ", "config.yml"))
  loc <- config::get("file_locations", file = file.path(root, "JGZ", "config.yml"))
  for (k in c(names(s), "kloof_outcomes", "kloof_parental_measures", "kloof_sex", "kloof_migration_background",
              "kloof_type_household", "bereik_region_types", "bereik_gender_groups", "org_catchment_codes")) {
    expect_false(is.null(cfg[[k]]), info = k)
  }
  for (k in c("kloof_folder", "catchment_cohort_file", "bereik_results_folder")) expect_false(is.null(loc[[k]]), info = k)
  expect_equal(unname(unlist(cfg$org_catchment_codes)), c(6, 7, 10, 12))
})
