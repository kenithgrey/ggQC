# Setup ------------------------------------------------------------------
Wheeler_43 <- read.table("Wheeler_USPC_43.csv", sep=",", header=T)
Wheeler_43

# Xbar Test Single Data Vector --------------------------------------------
W43_V_G_xBarLCL <- xBar_rBar_LCL(Wheeler_43, value = "values", grouping = "subgroup") # 1.811
W43_V_G_xBar <-  xBar_Bar(Wheeler_43, value = "values", grouping = "subgroup") # 4.7625
W43_V_G_xBarUCL <-  xBar_rBar_UCL(Wheeler_43, value = "values", grouping = "subgroup") # 7.715

context("xBar -- xBar_rBar_LCL -- xBar_rBar_UCL")
testthat::test_that("xBar -- xBar_rBar_LCL -- xBar_rBar_UCL work using Value and Group", {
  expect_equal(W43_V_G_xBarLCL, 1.811, tolerance = .015, scale = 1)
  expect_equal(W43_V_G_xBar, 4.7625, tolerance = .015, scale = 1)
  expect_equal(W43_V_G_xBarUCL, 7.715, tolerance = .015, scale = 1)
})


W43_formula_xBarLCL <- xBar_rBar_LCL(Wheeler_43, formula=values~subgroup) # 1.811
W43_formula_xBar <-  xBar_Bar(Wheeler_43, formula=values~subgroup) # 4.7625
W43_formula_xBarUCL <-  xBar_rBar_UCL(Wheeler_43, formula=values~subgroup) # 7.715

context("xBar -- xBar_rBar_LCL -- xBar_rBar_UCL")
testthat::test_that("xBar -- xBar_rBar_LCL -- xBar_rBar_UCL work using formulas", {
  expect_equal(W43_formula_xBarLCL, 1.811, tolerance = .015, scale = 1)
  expect_equal(W43_formula_xBar, 4.7625, tolerance = .015, scale = 1)
  expect_equal(W43_formula_xBarUCL, 7.715, tolerance = .015, scale = 1)
})


# Rbar Test Single Data Vector with missing Data --------------------------------------------
# The aggregate function seems to be robust to the missing data
# The effect of missing data here is that it will lower n to the lowest
# average integer 3 instead of 4 opening up the limits.
Wheeler_43_missing <- Wheeler_43
Wheeler_43_missing$values[1] <- NA
#aggregate(values~subgroup, FUN = "length", data = Wheeler_43_missing)
#aggregate(values~subgroup, FUN = "QCrange", data = Wheeler_43_missing)

W43_missing_V_G_xBarLCL <-  xBar_rBar_LCL(Wheeler_43_missing, value = "values", grouping = "subgroup") # 0.817
W43_missing_V_G_xBar <-  xBar_Bar(Wheeler_43_missing, value = "values", grouping = "subgroup") # 4.808
W43_missing_V_G_xBarUCL <-  xBar_rBar_UCL(Wheeler_43_missing, value = "values", grouping = "subgroup") # 8.799
testthat::test_that("missing value xBar -- xBar_rBar_LCL -- xBar_rBar_UCL work value and grouping", {
  expect_equal(W43_missing_V_G_xBarLCL, 0.817, tolerance = .015, scale = 1)
  expect_equal(W43_missing_V_G_xBar, 4.808, tolerance = .015, scale = 1)
  expect_equal(W43_missing_V_G_xBarUCL, 8.799, tolerance = .015, scale = 1)
})

W43_missing_formula_xBarLCL <-  xBar_rBar_LCL(Wheeler_43_missing, formula=values~subgroup) # 0.817
W43_missing_formula_xBar <-  xBar_Bar(Wheeler_43_missing, formula=values~subgroup) # 4.808
W43_missing_formula_xBarUCL <-  xBar_rBar_UCL(Wheeler_43_missing, formula=values~subgroup) # 8.799
testthat::test_that("missing value xBar -- xBar_rBar_LCL -- xBar_rBar_UCL work using formulas", {
  expect_equal(W43_missing_formula_xBarLCL, 0.817, tolerance = .015, scale = 1)
  expect_equal(W43_missing_formula_xBar, 4.808, tolerance = .015, scale = 1)
  expect_equal(W43_missing_formula_xBarUCL, 8.799, tolerance = .015, scale = 1)
})

# Changing n has an effect ------------------------------------------------
# Changing n can change the value of Xbar limits but not Rbar's limits .
W43_formula_DF <- as.data.frame.list(QC_Lines(Wheeler_43, formula = values~subgroup, method = "xBar.rBar")) #Same As Above
xBar_n_changes <-  as.data.frame.list(QC_Lines(Wheeler_43, formula = values~subgroup, n=10, method = "xBar.rBar"))
upperDiff <- (xBar_n_changes$xBar_rBar_UCL - W43_formula_DF$xBar_rBar_UCL)
lowerDiff <- (xBar_n_changes$xBar_rBar_LCL - W43_formula_DF$xBar_rBar_LCL)

testthat::test_that("Changing n can change the value of Xbar limits but not Rbar's limits", {
  expect_equal(xBar_n_changes$rBar_UCL, W43_formula_DF$rBar_UCL, tolerance = .01, scale = 1)
  expect_equal(upperDiff, -1.0846, tolerance = .01, scale = 1)
  expect_equal(lowerDiff, 1.0846, tolerance = .01, scale = 1)
})


# Warnings flagged for n greater than contant table -----------------------
#Does not repond to n
#QC_Lines(Wheeler_43_missing, formula = values~subgroup, n=30, method = "rBar")
#Will calculate for Xbar but not Rbar_UCL
set.seed(5555)
nTest <- data.frame(v=rnorm(26, 0, 1), g=rep(letters[1:26], each=1))
testthat::test_that("warnings tripped for n = 1", {
  testthat::expect_warning(xBar_rBar_LCL(data = nTest, formula = v~g))
  testthat::expect_warning(xBar_rBar_UCL(data = nTest, formula = v~g))
  testthat::expect_warning(QC_Lines(data = nTest, formula = v~g, n=20))
})


# Multi-dimentional workup ------------------------------------------------

context("Multi Dimentional Grouping Formulas")
Wheeler108 <- read.csv(file = "Wheeler_USPC_103.csv", header=T)
#Wheeler108 <- read.csv(file = "tests/testthat/Wheeler_USPC_103.csv", header=T)
Wheeler104_test <- as.data.frame.list(QC_Lines(Wheeler108, formula=value~Hour+PressCycle, method = "xBar.rBar"))
  # n = 4
  # XBarBAr = 9.74
  # rBar = 7.90
  # rLCL = 0
  # rUCL = 18.03
  # xBar_LCL = 3.98
  # xBar_UCL = 15.50

Wheeler105_test <- as.data.frame.list(QC_Lines(Wheeler108, formula=value~Hour+Cavity, method = "xBar.rBar"))
  # n = 5
  # XBarBAr = 9.74
  # rBar = 3.51
  # rLCL = 0
  # rUCL = 7.42
  # xBar_LCL = 7.71
  # xBar_UCL = 11.77

Wheeler108_test <- plyr::ddply(Wheeler108,
                               .variables = c("Cavity"),
                               .fun = function(df) {
                                 QC_Lines(data = df, formula = value~Cavity+Hour)}  )
# +=========+========+======+=====+=====+
# |Cavity   | I      |II    |III  | IV  |
# |n        | 5      |5     |5    | 5   |
# |XBar_BAr | 14.65  |8.97  |7.47 |7.86 |
# |rBar     | 3.30   |3.45  |3.95 |3.35 |
# |rLCL     | 0      |0     |0    |0    |
# |rUCL     | 6.98   |7.29  |8.35 |7.08 |
# |xBar_LCL | 12.75  |6.98  |5.19 |5.93 |
# |xBar_UCL | 16.55  |10.96 |9.75 |9.79 |
# |Matches  | YES    |YES   |YES  |YES  |
# +=========+========+======+=====+=====+

# write.csv(x = Wheeler104_test, file = "Wheeler104_results.csv")
# write.csv(x = Wheeler105_test, file = "Wheeler105_results.csv")
# write.csv(x = Wheeler108_test[,-1], file = "Wheeler108_results.csv")

#Wheeler104_test <-read.csv(file="Wheeler104_test.csv")
#Wheeler105_test <- read.csv(file="Wheeler105_test.csv")
#Wheeler108_test <- read.csv(file="Wheeler108_test.csv")

Wheeler104_results <- read.csv(file="Wheeler104_results.csv")
Wheeler105_results <- read.csv(file="Wheeler105_results.csv")
Wheeler108_results <- read.csv(file="Wheeler108_results.csv")

testthat::test_that("Multi Dimentional Grouping Formulas Work", {
  expect_equal(Wheeler104_test, Wheeler104_results[-1], tolerance = .0015, scale = 1)
  expect_equal(Wheeler105_test, Wheeler105_results[-1], tolerance = .001, scale = 1)
  expect_equal(Wheeler108_test[-1], Wheeler108_results[-1], tolerance = .001, scale = 1)
  #  expect_equal(xBar_rBar_LCL(data = Wheeler43, value = "values", grouping = "subgroup"), 1.811, tolerance = .001, scale = 1)
  #  expect_equal(xBar_rBar_UCL(data = Wheeler43, value = "values", grouping = "subgroup"), 7.715, tolerance = .001, scale = 1)
})
