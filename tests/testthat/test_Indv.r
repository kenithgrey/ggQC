# XmR Test ----------------------------------------------------------------
context("Individual Limit and Range Functions (XmR)")

expect_equal(mR_points(-5:5), c(NA, rep(1,10)), tolerance = .01, scale = 1)

Wheeler49 <- c(39,41,41,41,43,44,41,42,40,41,44,40)
testthat::test_that("XmR Function Work", {
  expect_equal(mean(Wheeler49), 41.42, tolerance = .02, scale = 1)
  expect_equal(mR(Wheeler49), 1.73, tolerance = .01, scale = 1)
  expect_equal(mR_UCL(Wheeler49), 5.65, tolerance = .01, scale = 1)
  expect_equal(xBar_one_LCL(Wheeler49), 36.82, tolerance = .01, scale = 1)
  expect_equal(xBar_one_UCL(Wheeler49), 46.02, tolerance = .02, scale = 1)
})

context("XBarR Functions")
Wheeler43 <- read.csv(file = "Wheeler_USPC_43.csv", header=T)
testthat::test_that("XbarR Function Work", {
  expect_equal(xBar_Bar(data = Wheeler43, value = "values", grouping = "subgroup"), 4.763, tolerance = .001, scale = 1)
  expect_equal(xBar_rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 1.811, tolerance = .001, scale = 1)
  expect_equal(xBar_rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 7.715, tolerance = .001, scale = 1)
  expect_equal(rBar(data = Wheeler43, value = "values", grouping = "subgroup"), 4.05, tolerance = .001, scale = 1)
  expect_equal(rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 0, tolerance = .01, scale = 1)
  expect_equal(rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 9.24, tolerance = .01, scale = 1)

})




