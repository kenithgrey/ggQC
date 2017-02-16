context("Multi Dimentional Grouping Formulas")
Wheeler108 <- read.csv(file = "Wheeler_USPC_103.csv", header=T)
Wheeler104_test <- QC_Lines(Wheeler108, formula=value~Hour+PressCycle, method = "xBar.rBar")
Wheeler105_test <- QC_Lines(Wheeler108, formula=value~Hour+Cavity, method = "xBar.rBar")
Wheeler108_test <- plyr::ddply(Wheeler108,
                               .variables = c("Cavity"),
                               .fun = function(df) {
                                 QC_Lines(data = df, formula = value~Cavity+Hour)}  )

#write.csv(x = Wheeler104_test, file = "Wheeler104_test.csv")
#write.csv(x = Wheeler105_test, file = "Wheeler105_test.csv")
#write.csv(x = Wheeler108_test[,-1], file = "Wheeler108_test.csv")

Wheeler104_test <-read.csv(file="Wheeler104_test.csv")
Wheeler105_test <- read.csv(file="Wheeler105_test.csv")
Wheeler108_test <- read.csv(file="Wheeler108_test.csv")

Wheeler104_results <- read.csv(file="Wheeler104_results.csv")
Wheeler105_results <- read.csv(file="Wheeler105_results.csv")
Wheeler108_results <- read.csv(file="Wheeler108_results.csv")

testthat::test_that("Multi Dimentional Grouping Formulas Work", {
  expect_equal(Wheeler104_test, Wheeler104_results, tolerance = .001, scale = 1)
  expect_equal(Wheeler105_test, Wheeler105_results, tolerance = .001, scale = 1)
  expect_equal(Wheeler108_test, Wheeler108_results, tolerance = .001, scale = 1)
  #  expect_equal(xBar_rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 1.811, tolerance = .001, scale = 1)
  #  expect_equal(xBar_rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 7.715, tolerance = .001, scale = 1)
})
