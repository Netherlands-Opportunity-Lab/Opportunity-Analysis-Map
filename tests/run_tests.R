# Run from the repository root: Rscript tests/run_tests.R  (no data needed)
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
