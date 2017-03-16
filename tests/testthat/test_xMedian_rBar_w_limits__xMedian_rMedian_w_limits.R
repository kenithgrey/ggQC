require(ggQC)
#require(tidyr)
# Desipersion Central Limit Tests -----------------------------------------

# Setup -------------------------------------------------------------------
Wheeler_232 <- read.table("wheeler232-233_long.csv", sep=",", header=T)
# Wheeler_232 <- gather(Wheeler_232, key = "subgroup", value = "value", -Subgroup)
# write.csv(Wheeler_232[,c(1,3)], file = "tests/testthat/wheeler232-233_long.csv", quote = F, row.names = F)
# Wheeler_232[,c(1,3)]


# Xmedian.Rbar Test Single Data Vector  using group and value --------------------------------------------
colnames(Wheeler_232)

Wheeler_232_V_G_rBarLCL <- rBar_LCL(Wheeler_232, value = "value", grouping = "Subgroup") # 0
Wheeler_232_V_G_rBar <-  rBar(Wheeler_232, value = "value", grouping = "Subgroup") # 8.85
Wheeler_232_V_G_rBarUCL <-  rBar_UCL(Wheeler_232, value = "value", grouping = "Subgroup") #18.7
testthat::test_that("Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(Wheeler_232_V_G_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_V_G_rBar, 8.85, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_V_G_rBarUCL, 18.7, tolerance = .1, scale = 1)
})


Wheeler_232_V_G_rMedianLCL <- rMedian_LCL(Wheeler_232, value = "value", grouping = "Subgroup") # 0
Wheeler_232_V_G_rMedian <-  rMedian(Wheeler_232, value = "value", grouping = "Subgroup") # 7.5
Wheeler_232_V_G_rMedianUCL <-  rMedian_UCL(Wheeler_232, value = "value", grouping = "Subgroup") #16.3
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_V_G_rMedianLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_V_G_rMedian, 7.5, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_V_G_rMedianUCL, 16.3, tolerance = .1, scale = 1)
})
QC_Lines(Wheeler_232, value = "value", grouping = "Subgroup", method="rMedian")


Wheeler_232_V_G_xMedian_Bar <- xMedian_Bar(Wheeler_232, value = "value", grouping = "Subgroup") #0.3
Wheeler_232_V_G_xMedian_Bar.rBar_LCL <- xMedian_rBar_LCL(Wheeler_232, value = "value", grouping = "Subgroup") #-5.8
Wheeler_232_V_G_xMedian_Bar.rBar_UCL <- xMedian_rBar_UCL(Wheeler_232, value = "value", grouping = "Subgroup") #6.4
QC_Lines(Wheeler_232, value = "value", grouping = "Subgroup", method="xMedian.rBar")

Wheeler_232_V_G_xMedian_Bar.rMedian_LCL <- xMedian_rMedian_LCL(Wheeler_232, value = "value", grouping = "Subgroup") #-5.04
Wheeler_232_V_G_xMedian_Bar.rMedian_UCL <- xMedian_rMedian_UCL(Wheeler_232, value = "value", grouping = "Subgroup") #5.6
QC_Lines(Wheeler_232, value = "value", grouping = "Subgroup", method="xMedian.rMedian")
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_V_G_xMedian_Bar, 0.3, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_V_G_xMedian_Bar.rBar_LCL, -5.8, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_V_G_xMedian_Bar.rBar_UCL, 6.4, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_V_G_xMedian_Bar.rMedian_LCL, -5.04, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_V_G_xMedian_Bar.rMedian_UCL, 5.6, tolerance = .1, scale = 1)
})



# Xmedian.Rbar Test Single Data Vector Using Formulas ----------------------------------------------------------

Wheeler_232_formula_rBarLCL <- rBar_LCL(Wheeler_232, formula=value~Subgroup) # 0
Wheeler_232_formula_rBar <-  rBar(Wheeler_232, formula=value~Subgroup) # 8.85
Wheeler_232_formula_rBarUCL <-  rBar_UCL(Wheeler_232, formula=value~Subgroup) #18.7
testthat::test_that("Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(Wheeler_232_formula_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rBar, 8.85, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rBarUCL, 18.7, tolerance = .1, scale = 1)
})

Wheeler_232_formula_rMedianLCL <- rMedian_LCL(Wheeler_232, formula=value~Subgroup) # 0
Wheeler_232_formula_rMedian <-  rMedian(Wheeler_232, formula=value~Subgroup) # 7.5
Wheeler_232_formula_rMedianUCL <-  rMedian_UCL(Wheeler_232, formula=value~Subgroup) #16.3
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_formula_rMedianLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rMedian, 7.5, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rMedianUCL, 16.3, tolerance = .1, scale = 1)
})

Wheeler_232_formula_xMedian_Bar <- xMedian_Bar(Wheeler_232, formula=value~Subgroup) #0.3
Wheeler_232_formula_xMedian_Bar.rBar_LCL <- xMedian_rBar_LCL(Wheeler_232, formula=value~Subgroup) #-5.8
Wheeler_232_formula_xMedian_Bar.rBar_UCL <- xMedian_rBar_UCL(Wheeler_232, formula=value~Subgroup) #6.4
QC_Lines(Wheeler_232, formula=value~Subgroup, method="xMedian.rBar")
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_formula_rMedianLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rMedian, 7.5, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_rMedianUCL, 16.3, tolerance = .1, scale = 1)
})

Wheeler_232_formula_xMedian_Bar.rMedian_LCL <- xMedian_rMedian_LCL(Wheeler_232, formula=value~Subgroup) #-5.04
Wheeler_232_formula_xMedian_Bar.rMedian_UCL <- xMedian_rMedian_UCL(Wheeler_232, formula=value~Subgroup) #5.6
QC_Lines(Wheeler_232, formula=value~Subgroup, method="xMedian.rMedian")
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_formula_xMedian_Bar, 0.3, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_xMedian_Bar.rBar_LCL, -5.8, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_formula_xMedian_Bar.rBar_UCL, 6.4, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_formula_xMedian_Bar.rMedian_LCL, -5.04, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_formula_xMedian_Bar.rMedian_UCL, 5.6, tolerance = .1, scale = 1)
})


# Missing Data ------------------------------------------------------------

# Xmedian.Rbar Test Single Data Vector  using group and value --------------------------------------------
#head(Wheeler_232_missing)
Wheeler_232_missing <- Wheeler_232
Wheeler_232_missing$value[1] <- NA


Wheeler_232_missing_V_G_rBarLCL <- rBar_LCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") # 0
Wheeler_232_missing_V_G_rBar <-  rBar(Wheeler_232_missing, value = "value", grouping = "Subgroup") # 8.80
Wheeler_232_missing_V_G_rBarUCL <-  rBar_UCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #20.1
testthat::test_that("Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(Wheeler_232_missing_V_G_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_rBar, 8.80, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_rBarUCL, 20.1, tolerance = .1, scale = 1)
})


Wheeler_232_missing_V_G_rMedianLCL <- rMedian_LCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") # 0
Wheeler_232_missing_V_G_rMedian <-  rMedian(Wheeler_232_missing, value = "value", grouping = "Subgroup") # 7.5
Wheeler_232_missing_V_G_rMedianUCL <-  rMedian_UCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #17.8
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_missing_V_G_rMedianLCL, 0, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_rMedian, 7.5, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_rMedianUCL, 17.8, tolerance = .1, scale = 1)
})


Wheeler_232_missing_V_G_xMedian_Bar <- xMedian_Bar(Wheeler_232_missing, value = "value", grouping = "Subgroup") #0.275
Wheeler_232_missing_V_G_xMedian_Bar.rBar_LCL <- xMedian_rBar_LCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #-6.7
Wheeler_232_missing_V_G_xMedian_Bar.rBar_UCL <- xMedian_rBar_UCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #7.2


Wheeler_232_missing_V_G_xMedian_Bar.rMedian_LCL <- xMedian_rMedian_LCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #-5.93
Wheeler_232_missing_V_G_xMedian_Bar.rMedian_UCL <- xMedian_rMedian_UCL(Wheeler_232_missing, value = "value", grouping = "Subgroup") #6.48
testthat::test_that("rMedian rMedian_UCL rMedian_LCL work using formula", {
  expect_equal(Wheeler_232_missing_V_G_xMedian_Bar, 0.275, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_xMedian_Bar.rBar_LCL, -6.7, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_xMedian_Bar.rBar_UCL, 7.2, tolerance = .1, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_xMedian_Bar.rMedian_LCL, -5.93, tolerance = .01, scale = 1)
  expect_equal(Wheeler_232_missing_V_G_xMedian_Bar.rMedian_UCL, 6.48, tolerance = .01, scale = 1)
})


# Xmedian.Rbar Test Single Data Vector Using Formulas ----------------------------------------------------------

Wheeler_232_formula_rBarLCL <- rBar_LCL(Wheeler_232_missing, formula=value~Subgroup) # 0
Wheeler_232_formula_rBar <-  rBar(Wheeler_232_missing, formula=value~Subgroup) # 8.80
Wheeler_232_formula_rBarUCL <-  rBar_UCL(Wheeler_232_missing, formula=value~Subgroup) #20.1

Wheeler_232_formula_rMedianLCL <- rMedian_LCL(Wheeler_232_missing, formula=value~Subgroup) # 0
Wheeler_232_formula_rMedian <-  rMedian(Wheeler_232_missing, formula=value~Subgroup) # 7.5
Wheeler_232_formula_rMedianUCL <-  rMedian_UCL(Wheeler_232_missing, formula=value~Subgroup) #17.81

Wheeler_232_formula_xMedian_Bar <- xMedian_Bar(Wheeler_232_missing, formula=value~Subgroup) #0.275
Wheeler_232_formula_xMedian_Bar.rBar_LCL <- xMedian_rBar_LCL(Wheeler_232_missing, formula=value~Subgroup) #-6.7
Wheeler_232_formula_xMedian_Bar.rBar_UCL <- xMedian_rBar_UCL(Wheeler_232_missing, formula=value~Subgroup) #7.3
QC_Lines(Wheeler_232_missing, formula=value~Subgroup, method="xMedian.rBar")

Wheeler_232_formula_xMedian_Bar.rMedian_LCL <- xMedian_rMedian_LCL(Wheeler_232_missing, formula=value~Subgroup) #-5.94
Wheeler_232_formula_xMedian_Bar.rMedian_UCL <- xMedian_rMedian_UCL(Wheeler_232_missing, formula=value~Subgroup) #6.49
QC_Lines(Wheeler_232_missing, formula=value~Subgroup, method="xMedian.rMedian")







# Changeing N does not cause changes in dispersion limits -----------------
W232_missing_formula_DF_rMedian <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="rMedian"))
W232_missing_formula_DF_rBar <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="rBar"))

W232_missing_formula_DF_rMedian_n10 <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="rMedian", n = 10))
W232_missing_formula_DF_rBar_n10 <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="rBar", n = 10))

testthat::test_that("missing value Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(W232_missing_formula_DF_rMedian, W232_missing_formula_DF_rMedian_n10, tolerance = .01, scale = 1)
  expect_equal(W232_missing_formula_DF_rBar, W232_missing_formula_DF_rBar_n10, tolerance = .01, scale = 1)
})


#Does not repond to n
#QC_Lines(Wheeler_43_missing, formula = values~subgroup, n=30, method = "rBar")
#Will calculate for Xbar but not Rbar_UCL

set.seed(5555)
nTest <- data.frame(v=rnorm(26, 0, 1), g=rep(letters[1:26], each=1))
testthat::test_that("warnings tripped for n = 1", {
  testthat::expect_warning(rBar_UCL(data = nTest, formula = v~g))
  testthat::expect_warning(rMedian_UCL(data = nTest, formula = v~g))
  testthat::expect_warning(QC_Lines(data = nTest, formula = v~g, n=20))
})


# Testing QC_ylines ----------------------------------------------------------
W232_missing_formula_DF_rMedian <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="xMedian.rMedian"))
W232_missing_formula_DF_rBar <- as.data.frame.list(QC_Lines(Wheeler_232_missing, value = "value", grouping = "Subgroup", method="xMedian.rBar"))
W232_missing_V_G_DF_rMedian <- as.data.frame.list(QC_Lines(Wheeler_232, value = "value", grouping = "Subgroup", method="xMedian.rMedian"))
W232_missing_V_G_DF_rBar <- as.data.frame.list(QC_Lines(Wheeler_232, value = "value", grouping = "Subgroup", method="xMedian.rBar"))


xMedian_rMedian_QC_ylines_test <- rbind(W232_missing_formula_DF_rMedian,
                             W232_missing_V_G_DF_rMedian)
#write.csv(x = xMedian_rMedian_QC_ylines_test, file = "tests/testthat/xMedian_rMedian_QC_ylines_results.csv", quote = F, row.names = F)
xMedian_rMedian_QC_ylines_results <- read.csv(file = "xMedian_rMedian_QC_ylines_results.csv", header=TRUE)

testthat::test_that("QC_ylines method = rBar works", {
  expect_equal(xMedian_rMedian_QC_ylines_test, xMedian_rMedian_QC_ylines_results, tolerance = .01, scale = 1)
})


xMedian_rBar_QC_ylines_test <- rbind(W232_missing_formula_DF_rBar,
                             W232_missing_V_G_DF_rBar)
#write.csv(x = xMedian_rBar_QC_ylines_test, file = "tests/testthat/xMedian_rBar_QC_ylines_results.csv", quote = F, row.names = F)
xMedian_rMedian_QC_ylines_results <- read.csv(file = "xMedian_rBar_QC_ylines_results.csv", header=TRUE)

testthat::test_that("QC_ylines method = rBar works", {
  expect_equal(xMedian_rBar_QC_ylines_test, xMedian_rMedian_QC_ylines_results, tolerance = .01, scale = 1)
})




