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
    y_m24_bmi = 16 + perc + rnorm(n, 0, .5),
    y_m24_bmi_p90 = as.integer(runif(n) < .3 - .2 * perc),
    pc4_m24 = rep(c(1011, 1012, 2021, 3031), length.out = n), pc3_m24 = rep(c(101, 101, 202, 303), length.out = n),
    org_m24 = rep(c(7, 7, 10, 6), length.out = n),
    y_m45_visus = c(NA, rbinom(n - 1, 1, .2)),
    pc4_m45 = 1011, pc3_m45 = 101, org_m45 = rep(c(7, 12), length.out = n),
    stringsAsFactors = FALSE)
}

test_that("outcome_age and the outcome list", {
  expect_equal(outcome_age(c("y_m24_bmi_p90", "y_m120_sdq", "y_m45_questions2_can")), c("m24", "m120", "m45"))
  co <- toy_cohort()
  expect_equal(kaart_outcome_list(co), c("y_m24_bmi", "y_m24_bmi_p90", "y_m45_visus"))
  expect_equal(kaart_outcome_list(co, list("y_m45_visus")), "y_m45_visus")
  expect_error(kaart_outcome_list(co, list("y_m99_x")), "not in the cohort file")
})

test_that("kaart_outcome_data uses that age's areas, only export orgs and only children with a value", {
  co <- toy_cohort()
  d <- kaart_outcome_data(co, "y_m45_visus", export_orgs = c(7, 10))
  expect_true(all(d$org == "7"))                          # org 12 dropped
  expect_false(anyNA(d$y_m45_visus))                      # child 1 has no value
  expect_equal(nrow(d), sum(!is.na(co$y_m45_visus) & co$org_m45 == 7))
  d24 <- kaart_outcome_data(co, "y_m24_bmi", export_orgs = c(7, 10))
  expect_setequal(unique(d24$pc4), c("1011", "1012", "2021"))  # 3031 belongs to org 6
  expect_error(kaart_outcome_data(co[, setdiff(names(co), "pc3_m45")], "y_m45_visus", 7), "pc3_m45")
})

test_that("kaart_outcome_data takes the parental income of the outcome's age", {
  co <- toy_cohort()
  # cohort file with income per age: m24 and m45 get different percentiles
  co$income_parents_perc_m24 <- co$income_parents_perc
  co$income_group_m24 <- co$income_group
  co$income_group_tails_m24 <- co$income_group_tails
  co$income_parents_perc_m45 <- 1 - co$income_parents_perc
  co$income_parents_perc_m45[1:10] <- NA
  co$income_group_m45 <- factor(ifelse(co$income_parents_perc_m45 <= .35 & co$income_parents_perc_m45 >= .15, "Low", NA),
                                levels = c("Low", "Mid", "High"))
  co$income_group_tails_m45 <- NA
  co$income_parents_perc <- NULL; co$income_group <- NULL; co$income_group_tails <- NULL
  d45 <- kaart_outcome_data(co, "y_m45_visus", export_orgs = c(7, 10))
  ok <- !is.na(co$y_m45_visus) & co$org_m45 == 7 & !is.na(co$income_parents_perc_m45)
  expect_equal(nrow(d45), sum(ok))                        # no income at m45 -> left out
  expect_equal(d45$income_parents_perc, co$income_parents_perc_m45[ok])
  expect_false(any(grepl("_m45$|_m24$", setdiff(names(d45), c("y_m45_visus")))))
  d24 <- kaart_outcome_data(co, "y_m24_bmi", export_orgs = c(7, 10))
  expect_equal(d24$income_parents_perc, co$income_parents_perc_m24[co$org_m24 %in% c(7, 10)])
  # an age without an income column in a per-age file is an error
  co2 <- co; co2$income_parents_perc_m45 <- NULL
  expect_error(kaart_outcome_data(co2, "y_m45_visus", 7), "not for m45")
})

test_that("estimates: mean for 'all', regression prediction for income groups, exact n", {
  co <- toy_cohort()
  d <- kaart_outcome_data(co, "y_m24_bmi", export_orgs = c(7, 10))
  groups <- kaart_groups(c("all", "Low"), c("all", "Men"), "all", "all")
  res <- kaart_estimate_outcome(d, "y_m24_bmi", c("org", "pc4"), groups)
  expect_false(res$binary[1])
  r <- res[res$region_type == "pc4" & res$region_id == "1011" & res$income_group == "all" & res$gender_group == "all", ]
  expect_equal(r$est, mean(d$y_m24_bmi[d$pc4 == "1011"]))
  expect_equal(r$n, sum(d$pc4 == "1011"))
  dl <- d[d$org == "7" & d$income_group %in% "Low" & d$sex == "Men", ]
  fit <- lm(y_m24_bmi ~ income_parents_perc, data = dl)
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
  expect_true(all(unlist(cfg$kaart_region_types) %in% c("org", "pc3", "pc4")))
})
