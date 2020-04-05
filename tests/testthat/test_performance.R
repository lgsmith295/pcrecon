context("Performance statistics input and output")
library(pcreg)

test_that("colnames of observed dataframe are correct", {
expect_error(perf_stats(valid_est = data.frame(year = c(2002, 2003), fit = c(-2, 2)), observed = data.frame(Year = c(2000, 2001, 2002, 2003), means = rnorm(4)), valid_yrs = c(2000, 2001), calib_yrs = c(2002, 2003)))
})