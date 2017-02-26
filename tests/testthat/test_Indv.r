# Raw Data ----------------------------------------------------------------
Wheeler49 <- c(39,41,41,41,43,44,41,42,40,41,44,40)
Wheeler49_df <- data.frame(values = c(39,41,41,41,43,44,41,42,40,41,44,40),
                           gen = rep("Wheeler49", 12),
                           ID = 1:12)
WheelerEMP15_df <- data.frame(values = c(591, 600, 594, 601, 598, 594, 599, 597, 599,
                                         597, 602, 597, 593, 598, 599, 601, 600, 599,
                                         595, 598, 592, 601, 601, 598, 601, 603, 593,
                                         599, 601, 599),
                              gen = rep("WheelerEMP15", 30),
                              ID = 1:30)

df_wheelerXmR <- rbind(Wheeler49_df, WheelerEMP15_df)


# XmR Test ----------------------------------------------------------------
context("Individual Limit and Range Functions (XmR)")

expect_equal(mR_points(-5:5), c(NA, rep(1,10)), tolerance = .01, scale = 1)

testthat::test_that("XmR Function Work", {
  expect_equal(mean(Wheeler49), 41.42, tolerance = .02, scale = 1)
  expect_equal(mR(Wheeler49), 1.73, tolerance = .01, scale = 1)
  expect_equal(mR_UCL(Wheeler49), 5.65, tolerance = .01, scale = 1)
  expect_equal(xBar_one_LCL(Wheeler49), 36.82, tolerance = .01, scale = 1)
  expect_equal(xBar_one_UCL(Wheeler49), 46.02, tolerance = .02, scale = 1)
})


Wheeler49_Test <- as.data.frame.list(ylines_indv(y=Wheeler49_df$values, method="XmR")) # Good
WheelerEMP15_Test <- as.data.frame.list(ylines_indv(y=WheelerEMP15_df$values, method="XmR")) # Good
Wheeler49_and_WheelerEMP15_Test <- plyr::ddply(df_wheelerXmR, .variables = "gen", .fun=function(data){
  ylines_indv(data$values, method = "XmR")}) # Good


# XmR Group Limit and Range Function  -------------------------------------

# write.table(x = Wheeler49_Test, file = "Wheeler49_Result.csv", quote = F, sep = ",", row.names = F )
# write.table(x = WheelerEMP15_Test, file = "WheelerEMP15_Result.csv", quote = F, sep = ",", row.names = F)
# write.table(x = Wheeler49_and_WheelerEMP15_Test, file = "Wheeler49_and_WheelerEMP15_Result.csv", quote = F, sep = ",", row.names = F)

Wheeler49_Result <- read.csv("Wheeler49_Result.csv", header=T)
WheelerEMP15_Result <- read.csv("WheelerEMP15_Result.csv", header=T)
Wheeler49_and_WheelerEMP15_Result <- read.csv("Wheeler49_and_WheelerEMP15_Result.csv", header=T)

context("XmR ylines and grouping works")
testthat::test_that("XmR ylines and grouping works", {
  expect_equal(Wheeler49_Test, Wheeler49_Result , tolerance = .001, scale = 1)
  expect_equal(WheelerEMP15_Test, WheelerEMP15_Result , tolerance = .001, scale = 1)
  expect_equal(Wheeler49_and_WheelerEMP15_Result, Wheeler49_and_WheelerEMP15_Result , tolerance = .001, scale = 1)
})



# XBarR Individual Functions ----------------------------------------------
context("XBarR Functions")
Wheeler43 <- read.csv(file = "Wheeler_USPC_43.csv", header=T)
testthat::test_that("XbarR Function Work", {
  expect_equal(xBar_Bar(data = Wheeler43, value = "values", grouping = "subgroup"), 4.763, tolerance = .001, scale = 1)
  expect_equal(xBar_rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 1.811, tolerance = .001, scale = 1)
  expect_equal(xBar_rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 7.715, tolerance = .002, scale = 1)
  expect_equal(rBar(data = Wheeler43, value = "values", grouping = "subgroup"), 4.05, tolerance = .001, scale = 1)
  expect_equal(rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 0, tolerance = .01, scale = 1)
  expect_equal(rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 9.24, tolerance = .01, scale = 1)

})




