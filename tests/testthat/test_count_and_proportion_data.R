# c-chart Checks ----------------------------------------------------------
set.seed(5555)
y <- rpois(30, 9)

context("c-chart functions")

testthat::test_that("c-chart functions", {
  expect_equal(cBar_UCL(y), 16.99, tolerance = .02, scale = 1)
  expect_equal(cBar_LCL(y), 0, tolerance = .02, scale = 1)
})


# np-chart checks ---------------------------------------------------------
set.seed(5555)
p <- rbinom(n = 100, size = 30, prob = .2)

set.seed(5555)
bin_data <- data.frame(
  trial=1:30,
  NNC = rbinom(30, 30, prob = .50),
  N = rep(15, 30))

context("np-chart functions")
testthat::test_that("np-chart functions", {
  expect_equal(npBar_UCL(p, n=30), 12.74, tolerance = .02, scale = 1)
  expect_warning(npBar_UCL(p, n=10))
  expect_equal(npBar_LCL(p, n=30), 0, tolerance = .02, scale = 1)
  expect_equal(as.vector(
               ylines_indv(p, n=30, method = "np")),
               c(0,6.1,12.7), tolerance = .042, scale = 1)
  })



# p-charts checks ----------------------------------------------------------------
wheeler264 <- read.table(file = "wheeler_USPC_264.csv", header=T, sep=",")
# wheeler264_test <- ylines_indv(y = wheeler264$Proportion_Incomplete,
#                                n = wheeler264$Num_Items_in_Set,
#                                method = "p")
wheeler264_results <- read.table("wheeler264_test.csv", header = T, sep=",")[,-1]
#write.csv(x = wheeler264_test, file = "tests/testthat/wheeler264_test.csv")

#colnames(wheeler264) <- c("Day", "Date", "Num_Incomplete_Items", "Num_Items_in_Set",
#                          "Proportion_Incomplete")
context("p-chart functions")
testthat::test_that("p-chart functions", {
  expect_equal(
    pBar_UCL(
      wheeler264$Proportion_Incomplete,
             n = wheeler264$Num_Items_in_Set
      ),
    wheeler264_results$pBar_UCL,
    tolerance = .02,
    scale = 1
  )
expect_equal(
    pBar(
      wheeler264$Proportion_Incomplete,
      n = wheeler264$Num_Items_in_Set
    ),
    wheeler264_results$pBar,
    tolerance = .02,
    scale = 1
  )
expect_equal(
    pBar_LCL(
      y = wheeler264$Proportion_Incomplete,
      n = wheeler264$Num_Items_in_Set
    ),
    wheeler264_results$pBar_LCL,
    tolerance = .02,
    scale = 1
  )
expect_equal(ylines_indv(y = wheeler264$Proportion_Incomplete,
                n = wheeler264$Num_Items_in_Set,
                method = "p"),
    wheeler264_results, tolerance = .042, scale = 1)
})






# u-chart Checks ----------------------------------------------------------
wheeler276 <- read.table(file = "wheeler_USPC_276.csv", header=TRUE, sep=",")
wheeler276_test <- ylines_indv(y = wheeler276$Rate, wheeler276$No_of_Radiators, method = "u")
#write.csv(wheeler276_test, "tests/testthat/wheeler276_test.csv", quote = FALSE)
wheeler276_results <- read.csv("wheeler276_test.csv", header=TRUE)[,-1]

context("u-chart functions")
testthat::test_that("u-chart functions", {
  expect_equal(wheeler276_test, wheeler276_results,
               tolerance = .01,
               scale = 1)})


