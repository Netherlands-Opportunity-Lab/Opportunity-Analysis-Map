root <- file.path("..", "..")

test_that("every R script parses", {
  scripts <- list.files(root, "\\.R$", recursive = TRUE, full.names = TRUE)
  scripts <- scripts[!grepl("renv/", scripts)]
  for (s in scripts) expect_error(parse(s, encoding = "UTF-8"), NA, info = s)
})

test_that("the OSSC job scripts point to scripts that exist", {
  for (job in list.files(root, "_job\\.sh$", full.names = TRUE)) {
    line <- grep("Rscript", readLines(job, warn = FALSE), value = TRUE)
    script <- sub(".*\\$PROJECT/(\\S+\\.R).*", "\\1", line)
    expect_true(file.exists(file.path(root, script)), info = job)
  }
})

test_that("every array job finds its expectation function", {
  for (ses in c("income", "wealth", "educ")) {
    hits <- list.files(file.path(root, "src", paste0(ses, "_parents")),
                       paste0("01_expectation_function_", ses, "\\.R$"))
    expect_length(hits, 1)
  }
})

test_that("the config has the paths the RA steps use", {
  cfg <- list.files(file.path(root, "config"), "config\\.yml$", full.names = TRUE)
  loc <- config::get("file_locations", file = cfg)
  for (k in c("cohort_folder", "input_folder", "pwr_file", "gwb_file", "corop_file")) {
    expect_false(is.null(loc[[k]]), info = k)
  }
})
