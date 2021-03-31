context("To test the working of the make_filename function")

test_that("Whether make_filename gives the same output", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
