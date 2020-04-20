

# library(pcreg)

test_that("Header messages are returned correctly", {
  em_file = system.file("extdata/crns/em000.crn", package = "pcreg")
  expect_error(read_crn_head(fname = em_file, err = "stop"), "em000.crn has fewer than 4 lines")
  expect_warning(read_crn_head(fname = em_file, err = "warn"), "em000.crn has fewer than 4 lines")
  expect_null(read_crn_head(fname = em_file, err = "log"))
})
