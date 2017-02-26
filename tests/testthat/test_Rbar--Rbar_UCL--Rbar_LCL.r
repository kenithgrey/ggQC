require(rQC)
# Desipersion Central Limit Tests -----------------------------------------


# Setup -------------------------------------------------------------------
Wheeler_43 <- read.table("Wheeler_USPC_43.csv", sep=",", header=T)
Wheeler_43


# Rbar Test Single Data Vector --------------------------------------------
# colnames(Wheeler_43)
W43_V_G_rBarLCL <- rBar_LCL(Wheeler_43, value = "values", grouping = "subgroup") # 0
W43_V_G_rBar <-  rBar(Wheeler_43, value = "values", grouping = "subgroup") # 4.05
W43_V_G_rBarUCL <-  rBar_UCL(Wheeler_43, value = "values", grouping = "subgroup") # 9.24

context("rBar -- rBar_UCL -- rBar_LCL")
testthat::test_that("Rbar Rbar_UCL rBar_LCL work using Value and Group", {
  expect_equal(W43_V_G_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(W43_V_G_rBar, 4.05, tolerance = .01, scale = 1)
  expect_equal(W43_V_G_rBarUCL, 9.24, tolerance = .01, scale = 1)
})


W43_formula_rBarLCL <-  rBar_LCL(Wheeler_43, formula = values~subgroup) # 0
W43_formula_rBar <-  rBar(Wheeler_43, formula = values~subgroup) # 4.05
W43_formula_rBarUCL <- rBar_UCL(Wheeler_43, formula = values~subgroup) # 9.24

testthat::test_that("Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(W43_formula_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(W43_formula_rBar, 4.05, tolerance = .01, scale = 1)
  expect_equal(W43_formula_rBarUCL, 9.24, tolerance = .01, scale = 1)
})

# Rbar Test Single Data Vector --------------------------------------------
  # The aggregate function seems to be robust to the missing data
  # The effect of missing data here is that it will lower n to the lowest
  # average integer 3 instead of 4 opening up the limits.
Wheeler_43_missing <- Wheeler_43
Wheeler_43_missing$values[1] <- NA
  #aggregate(values~subgroup, FUN = "length", data = Wheeler_43_missing)
  #aggregate(values~subgroup, FUN = "QCrange", data = Wheeler_43_missing)

W43_missing_V_G_rBarLCL <-  rBar_LCL(Wheeler_43_missing, value = "values", grouping = "subgroup") # 0
W43_missing_V_G_rBar <-  rBar(Wheeler_43_missing, value = "values", grouping = "subgroup") # 3.9
W43_missing_V_G_rBarUCL <-  rBar_UCL(Wheeler_43_missing, value = "values", grouping = "subgroup") # 10.046
testthat::test_that("missing value Rbar Rbar_UCL rBar_LCL work value and grouping", {
  expect_equal(W43_missing_V_G_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(W43_missing_V_G_rBar, 3.9, tolerance = .01, scale = 1)
  expect_equal(W43_missing_V_G_rBarUCL, 10.046, tolerance = .01, scale = 1)
})

W43_missing_formula_rBarLCL <-  rBar_LCL(Wheeler_43_missing, formula = values~subgroup) # 0
W43_missing_formula_rBar <-  rBar(Wheeler_43_missing, formula = values~subgroup) # 3.9
W43_missing_formula_rBarUCL <-  rBar_UCL(Wheeler_43_missing, formula = values~subgroup) # 10.046
testthat::test_that("missing value Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(W43_missing_formula_rBarLCL, 0, tolerance = .01, scale = 1)
  expect_equal(W43_missing_formula_rBar, 3.9, tolerance = .01, scale = 1)
  expect_equal(W43_missing_formula_rBarUCL, 10.046, tolerance = .01, scale = 1)
})

# Changing N is not allowed the change the value of Rbar and Rbar_(UL)CL.
W43_missing_formula_DF <- as.data.frame.list(QC_Lines(Wheeler_43_missing, formula = values~subgroup, method = "rBar")) #Same As Above
rBar_n_innert <-  as.data.frame.list(QC_Lines(Wheeler_43_missing, formula = values~subgroup, n=10, method = "rBar"))
testthat::test_that("missing value Rbar Rbar_UCL rBar_LCL work using formula", {
  expect_equal(rBar_n_innert, W43_missing_formula_DF, tolerance = .01, scale = 1)
})

#Does not repond to n
  #QC_Lines(Wheeler_43_missing, formula = values~subgroup, n=30, method = "rBar")
  #Will calculate for Xbar but not Rbar_UCL

set.seed(5555)
nTest <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B"), each=30))
testthat::test_that("warnings tripped for n < 20", {
testthat::expect_warning(rBar_UCL(data = nTest, formula = v~g))
testthat::expect_warning(QC_Lines(data = nTest, formula = v~g, n=20))
})

# Testing QC_ylines ----------------------------------------------------------

W43_V_G_DF <- as.data.frame.list(QC_Lines(Wheeler_43, value = "values", grouping = "subgroup", method = "rBar")) #Same As Above
W43_formula_DF <- as.data.frame.list(QC_Lines(Wheeler_43, formula = values~subgroup, method = "rBar")) #Same As Above
W43_missing_V_G_DF <- as.data.frame.list(QC_Lines(Wheeler_43_missing, value = "values", grouping = "subgroup", method = "rBar")) #Same As Above
W43_missing_formula_DF <- as.data.frame.list(QC_Lines(Wheeler_43_missing, formula = values~subgroup, method = "rBar")) #Same As Above

Rbar_QC_ylines_test <- rbind(W43_V_G_DF, W43_formula_DF, W43_missing_V_G_DF, W43_missing_formula_DF)
#write.csv(x = Rbar_QC_ylines_results, file = "tests/testthat/Rbar_QC_ylines_results.csv", quote = F, row.names = F)
Rbar_QC_ylines_results <- read.csv(file = "Rbar_QC_ylines_results.csv", header=TRUE)

testthat::test_that("QC_ylines method = rBar works", {
  expect_equal(Rbar_QC_ylines_test, Rbar_QC_ylines_results, tolerance = .01, scale = 1)
})
