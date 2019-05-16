test_that("There are 4 columns in this dataframe", {
  testthat::expect_equal(ncol(fars_summarize_years(2013:2015)), 4)
  })
