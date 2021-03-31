context("To test the functionality of fars_read function")

test_that("Whether fars_read() works correctly", {

  filename = make_filename(2015)
  path = system.file("extdata", filename, package = "farsdata")
  expect_equal(nrow(fars_read(path)), 32166)  ## check whether dataset has desired dimension (rows)
  expect_equal(ncol(fars_read(path)), 52)   ## check whether dataset has desired dimension (columns)

})
