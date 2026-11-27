# JGZ kaart (JGZ/Scripts/kaart/jgz_kaart_functions.R). Toy cohort, no CBS data.
root <- file.path("..", "..")
suppressPackageStartupMessages(library(dplyr))
source(file.path(root, "JGZ", "Scripts", "kaart", "jgz_kaart_functions.R"))
source_expectation_function(file.path(root, "src", "income_parents"))

# toy jgz_cohort.rds: two ages with their own area columns
toy_cohort <- function(n = 400, seed = 1) {
  set.seed(seed)
  perc <- runif(n)
  data.frame(
    RINPERSOONS = "R", RINPERSOON = sprintf("k%04d", 1:n),
    income_parents_perc = perc,
    income_group = factor(case_when(perc >= .15 & perc <= .35 ~ "Low", perc >= .40 & perc <= .60 ~ "Mid",
                                    perc >= .65 & perc <= .85 ~ "High"), levels = c("Low", "Mid", "High")),
    income_group_tails = NA, sex = rep(c("Men", "Women"), length.out = n),
    migration_background = "No Migration Background", has_migration = 0, type_household = "Two Parents",
    m024_bmi = 16 + perc + rnorm(n, 0, .5),
    m024_bmi_p90 = as.integer(runif(n) < .3 - .2 * perc),
    pc4_m024 = rep(c(1011, 1012, 2021, 3031), length.out = n), pc3_m024 = rep(c(101, 101, 202, 303), length.out = n),
    org_m024 = rep(c(7, 7, 10, 6), length.out = n),
    m045_visus = c(NA, rbinom(n - 1, 1, .2)),
    pc4_m045 = 1011, pc3_m045 = 101, org_m045 = rep(c(7, 12), length.out = n),
    stringsAsFactors = FALSE)
}

test_that("outcome_age and the outcome list", {
  expect_equal(outcome_age(c("m024_bmi_p90", "m120_sdq", "m045_questions2_can")), c("m024", "m120", "m045"))
  co <- toy_cohort()
  expect_equal(kaart_outcome_list(co), c("m024_bmi", "m024_bmi_p90", "m045_visus"))
  expect_equal(kaart_outcome_list(co, list("m045_visus")), "m045_visus")
  expect_error(kaart_outcome_list(co, list("m099_x")), "not in the cohort file")
})

test_that("cohort files with the old names (y_m24_*) still work", {
  co <- toy_cohort()
  names(co) <- sub("^m0?([0-9]+)_", "y_m\\1_", names(co))
  names(co) <- sub("_m0?([0-9]+)$", "_m\\1", names(co))
  expect_equal(outcome_age("y_m24_bmi_p90"), "m24")
  expect_equal(kaart_outcome_list(co), c("y_m24_bmi", "y_m24_bmi_p90", "y_m45_visus"))
  d <- kaart_outcome_data(co, "y_m45_visus", export_orgs = 7)
  expect_equal(nrow(d), sum(!is.na(co$y_m45_visus) & co$org_m45 == 7))
})

test_that("kaart_outcome_data uses that age's areas, only export orgs and only children with a value", {
  co <- toy_cohort()
  d <- kaart_outcome_data(co, "m045_visus", export_orgs = c(7, 10))
  expect_true(all(d$org == "7"))                          # org 12 dropped
  expect_false(anyNA(d$m045_visus))                      # child 1 has no value
  expect_equal(nrow(d), sum(!is.na(co$m045_visus) & co$org_m045 == 7))
  d24 <- kaart_outcome_data(co, "m024_bmi", export_orgs = c(7, 10))
  expect_setequal(unique(d24$pc4), c("1011", "1012", "2021"))  # 3031 belongs to org 6
  expect_error(kaart_outcome_data(co[, setdiff(names(co), "pc3_m045")], "m045_visus", 7), "pc3_m045")
})

test_that("kaart_outcome_data takes the parental income of the outcome's age", {
  co <- toy_cohort()
  # cohort file with income per age: m024 and m045 get different percentiles
  co$income_parents_perc_m024 <- co$income_parents_perc
  co$income_group_m024 <- co$income_group
  co$income_group_tails_m024 <- co$income_group_tails
  co$income_parents_perc_m045 <- 1 - co$income_parents_perc
  co$income_parents_perc_m045[1:10] <- NA
  co$income_group_m045 <- factor(ifelse(co$income_parents_perc_m045 <= .35 & co$income_parents_perc_m045 >= .15, "Low", NA),
                                levels = c("Low", "Mid", "High"))
  co$income_group_tails_m045 <- NA
  co$income_parents_perc <- NULL; co$income_group <- NULL; co$income_group_tails <- NULL
  d45 <- kaart_outcome_data(co, "m045_visus", export_orgs = c(7, 10))
  ok <- !is.na(co$m045_visus) & co$org_m045 == 7 & !is.na(co$income_parents_perc_m045)
  expect_equal(nrow(d45), sum(ok))                        # no income at m045 -> left out
  expect_equal(d45$income_parents_perc, co$income_parents_perc_m045[ok])
  expect_false(any(grepl("_m045$|_m024$", setdiff(names(d45), c("m045_visus")))))
  d24 <- kaart_outcome_data(co, "m024_bmi", export_orgs = c(7, 10))
  expect_equal(d24$income_parents_perc, co$income_parents_perc_m024[co$org_m024 %in% c(7, 10)])
  # an age without an income column in a per-age file is an error
  co2 <- co; co2$income_parents_perc_m045 <- NULL
  expect_error(kaart_outcome_data(co2, "m045_visus", 7), "not for m045")
})

test_that("estimates: mean for 'all', regression prediction for income groups, exact n", {
  co <- toy_cohort()
  d <- kaart_outcome_data(co, "m024_bmi", export_orgs = c(7, 10))
  groups <- kaart_groups(c("all", "Low"), c("all", "Men"), "all", "all")
  res <- kaart_estimate_outcome(d, "m024_bmi", c("org", "pc4"), groups)
  expect_false(res$binary[1])
  r <- res[res$region_type == "pc4" & res$region_id == "1011" & res$income_group == "all" & res$gender_group == "all", ]
  expect_equal(r$est, mean(d$m024_bmi[d$pc4 == "1011"]))
  expect_equal(r$n, sum(d$pc4 == "1011"))
  dl <- d[d$org == "7" & d$income_group %in% "Low" & d$sex == "Men", ]
  fit <- lm(m024_bmi ~ income_parents_perc, data = dl)
  r2 <- res[res$region_type == "org" & res$region_id == "7" & res$income_group == "Low" & res$gender_group == "Men", ]
  expect_equal(r2$est, unname(predict(fit, data.frame(income_parents_perc = .25))))
  expect_equal(r2$n, nrow(dl))
  expect_equal(nrow(res), (2 + 3) * nrow(groups))         # 2 orgs + 3 PC4s
})

test_that("kaart_disclosure applies the national CBS rules in order", {
  res <- data.frame(outcome = "y", binary = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
                    income_group = c("all", "Low", "all", "all", "all", "all"),
                    n = c(10L, 11L, 100L, 50L, 50L, 100L), est = c(.5, .5, .05, 0, 16, .5),
                    lwr = 0, upr = 1, stringsAsFactors = FALSE)
  out <- kaart_disclosure(res)
  expect_equal(out$reason_of_na, c("1. fewer than 11 obs", "1. fewer than 12 obs", "2. est*n < 10",
                                   "4. est == 0", NA, NA))
  expect_true(all(is.na(out$est[1:4])) && all(is.na(out$n[1:4])))
  expect_equal(out$n[5:6], c(50L, 100L))
  expect_equal(kaart_disclosure(data.frame(binary = TRUE, income_group = "all", n = 100L, est = .95,
                                           lwr = 0, upr = 1))$reason_of_na, "3. (1-est)*n < 10")
  expect_equal(kaart_disclosure(data.frame(binary = FALSE, income_group = "all", n = 47L, est = 3,
                                           lwr = 0, upr = 1))$n, 50L)
})

test_that("JGZ/config.yml has the kaart settings and paths", {
  cfg <- config::get("settings", file = file.path(root, "JGZ", "config.yml"))
  loc <- config::get("file_locations", file = file.path(root, "JGZ", "config.yml"))
  for (k in c("kaart_outcomes", "kaart_region_types", "kaart_income_groups", "kaart_gender_groups",
              "kaart_migration_groups", "kaart_household_groups", "kaart_min_n_mean",
              "kaart_min_n_regression", "kaart_min_count", "kaart_round_n", "export_orgs")) {
    expect_false(is.null(cfg[[k]]), info = k)
  }
  expect_false(is.null(loc$cohort_file)); expect_false(is.null(loc$kaart_folder))
  expect_false(is.null(loc$catchment_cohort_file))
  expect_true(all(unlist(cfg$kaart_region_types) %in% c("org", "pc3", "pc4")))
})

test_that("organisation settings: export only permitted orgs, internal orgs never exported (20-11-2026)", {
  old <- setwd(root); on.exit(setwd(old))
  env <- new.env()
  sys.source("JGZ/Scripts/jgz_helpers.R", envir = env)
  expect_true(env$jgz_check_orgs(env$cfg))                         # the config in the repo passes
  expect_false(any(c(12) %in% unlist(env$cfg$export_orgs)))
  ok <- list(export_orgs = c(7, 10), permitted_orgs = c(6, 7, 10), internal_orgs = 12)
  expect_true(env$jgz_check_orgs(ok))
  expect_error(env$jgz_check_orgs(modifyList(ok, list(export_orgs = c(7, 12)))), "without permission: 12")
  expect_error(env$jgz_check_orgs(modifyList(ok, list(internal_orgs = 7))), "both export_orgs and internal_orgs")
  expect_error(env$jgz_check_orgs(modifyList(ok, list(permitted_orgs = NULL))), "permitted_orgs is not set")
})

test_that("kaart_run: pooled 'all' only over the organisations passed (export run never contains org 12)", {
  co <- toy_cohort()
  groups <- kaart_groups("all", "all", "all", "all")
  ex <- suppressMessages(kaart_run(co, "m045_visus", c(7, 10), c("all", "org"), groups))
  expect_setequal(ex$region_id[ex$region_type == "org"], "7")
  expect_equal(ex$n[ex$region_type == "all"], sum(!is.na(co$m045_visus) & co$org_m045 == 7))
  int <- suppressMessages(kaart_run(co, "m045_visus", 12, "org", groups))
  expect_setequal(int$region_id, "12")
})

# ---- Bereik as a kaart outcome (27-11-2026) -------------------------------------------------

# toy jgz_catchment_cohort.rds: children living in a catchment area, seen or
# not, with the same area and income columns as the JGZ cohort
toy_catchment <- function(n = 300, seed = 2) {
  set.seed(seed)
  perc <- runif(n)
  org <- rep(c(7, 10, 12), length.out = n)
  pc4 <- rep(c(7701, 8181, 1234), length.out = n)
  d <- data.frame(RINPERSOONS = "R", RINPERSOON = sprintf("c%04d", 1:n), org_catchment = org, postcode_4 = pc4,
                  sex = rep(c("Men", "Women"), length.out = n), migration_background = "No Migration Background",
                  has_migration = 0, type_household = "Two Parents",
                  m024_takeup = rbinom(n, 1, .6 + .3 * perc), m045_takeup_any = 1L, stringsAsFactors = FALSE)
  for (a in c("m024", "m045")) {
    d[[paste0("org_", a)]] <- org; d[[paste0("pc4_", a)]] <- pc4; d[[paste0("pc3_", a)]] <- pc4 %/% 10
    d[[paste0("income_parents_perc_", a)]] <- ifelse(seq_len(n) %% 10 == 0, NA, perc)
    d[[paste0("income_group_", a)]] <- factor(case_when(perc >= .15 & perc <= .35 ~ "Low", perc >= .40 & perc <= .60 ~ "Mid",
                                                        perc >= .65 & perc <= .85 ~ "High"), levels = c("Low", "Mid", "High"))
    d[[paste0("income_group_tails_", a)]] <- NA
  }
  d
}

test_that("the kaart reads both cohort files, each outcome from its own file", {
  loc <- config::get("file_locations", file = file.path(root, "JGZ", "config.yml"))
  expect_named(kaart_cohort_files(loc), c("jgz", "jgz_catchment"))
  expect_error(kaart_cohort_files(list(cohort_file = "x.rds")), "catchment_cohort_file")
  cohorts <- list(jgz = toy_cohort(), jgz_catchment = toy_catchment())
  oc <- kaart_outcomes_by_cohort(cohorts)
  expect_equal(oc$jgz, c("m024_bmi", "m024_bmi_p90", "m045_visus"))
  expect_equal(oc$jgz_catchment, c("m024_takeup", "m045_takeup_any"))
  oc2 <- kaart_outcomes_by_cohort(cohorts, list("m024_takeup", "m045_visus"))
  expect_equal(oc2, list(jgz = "m045_visus", jgz_catchment = "m024_takeup"))
  expect_error(kaart_outcomes_by_cohort(cohorts, list("m099_x")), "not in the cohort files")
  dup <- cohorts; dup$jgz$m024_takeup <- 1L
  expect_error(kaart_outcomes_by_cohort(dup), "more than one cohort file: m024_takeup")
})

test_that("take-up per area and income group: share among children living there with an income", {
  catch <- toy_catchment()
  cohorts <- list(jgz = toy_cohort(), jgz_catchment = catch)
  groups <- kaart_groups(c("all", "Low", "High"), "all", "all", "all")
  est <- suppressMessages(kaart_run_cohorts(cohorts, kaart_outcomes_by_cohort(cohorts), c(7, 10),
                                            c("org", "pc4"), groups))
  expect_equal(names(est)[1], "sample")
  expect_setequal(est$sample, c("jgz", "jgz_catchment"))
  tk <- est[est$sample == "jgz_catchment" & est$outcome == "m024_takeup", ]
  expect_setequal(tk$region_id[tk$region_type == "org"], c("7", "10"))         # never org 12
  r <- tk[tk$region_type == "org" & tk$region_id == "7" & tk$income_group == "all", ]
  ok <- catch$org_m024 == 7 & !is.na(catch$income_parents_perc_m024)
  expect_equal(r$est, mean(catch$m024_takeup[ok]))
  expect_equal(r$n, sum(ok))
  low <- tk[tk$region_type == "org" & tk$region_id == "7" & tk$income_group == "Low", ]
  dl <- catch[ok & catch$income_group_m024 %in% "Low", ]
  fit <- lm(m024_takeup ~ income_parents_perc_m024, data = dl)
  expect_equal(low$est, unname(predict(fit, data.frame(income_parents_perc_m024 = .25))))
  # take-up of ~100% is suppressed by the (1 - est) * n rule, as before
  res <- kaart_disclosure(est)
  any_all <- res[res$outcome == "m045_takeup_any", ]
  expect_true(all(grepl("^3\\.", any_all$reason_of_na[!is.na(any_all$reason_of_na) & any_all$income_group == "all"])))
})

test_that("a catchment cohort file without per-age area columns gives a clear error", {
  old <- toy_catchment()[, c("RINPERSOONS", "RINPERSOON", "org_catchment", "postcode_4", "m024_takeup")]
  expect_error(kaart_outcome_data(old, "m024_takeup", 7), "Rerun jgz_catchment")
})
