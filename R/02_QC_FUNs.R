##############################
# Copyright 2017 Kenith Grey #
##############################

# Copyright Notice --------------------------------------------------------
# This file is part of ggQC.
#
# ggQC is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ggQC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ggQC.  If not, see <http://www.gnu.org/licenses/>.

# General Funs ------------------------------------------------------------
sigma_est_symetric <-
  function(y,
           n = n,
           na.rm = na.rm,
           center,
           threesigma) {
    return(abs(center(y, na.rm = na.rm, n = n) - threesigma(y, na.rm = na.rm, n =
                                                              n)) / 3)
  }

sigma_est_symetric_grouping <-
  function(data = data,
           value = value,
           grouping = grouping,
           formula = formula,
           n = n,
           center,
           threesigma) {
    return(abs(
      center(
        data = data, value = value,
        grouping = grouping,formula = formula,
        n = n) - threesigma(
        data = data, value = value,
        grouping = grouping, formula = formula,
        n = n)
             ) / 3)
  }


ZERO <- function(...){0}

#' @export
#' @title Range: Max Min Difference
#' @description Given a set of numbers, function calculates the difference between the maximum and minimum value.
#' @param y : vector of values
#' @return a number.
#' @examples
#' y <- seq(-5:5)
#' QCrange(y)
QCrange <- function(y){max(y) - min(y)}

#' @export
#' @title One Point Moving Range of Vector
#' @description Calculates a one-point moving range vector given an input vector of values.
#'  Output often used to produce mR-chart.
#' @param y : vector of values
#' @return Vector of one-point moving range.
#' @examples
#' y <- seq(-5:5)
#' mR_points(y)
mR_points<- function(y){c(NA, abs(diff(y)))}

mR_points_gg <- dispersionFUN(mean, mR_points)


# Count Data c-chart --------------------------------------------------------------
#' @export
#' @title Upper Control Limit: Count Data (c-chart)
#' @description Calculates upper control limit (UCL) for count data acquired
#' over the same-sized area of opportunity.
#' @param y Vector of count data. Each observation having the
#'  same-area of opportunity.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' @return A number; 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' y <- rpois(30, 9)
#' cBar_UCL(y)
#'
cBar_UCL <- function(y, na.rm=FALSE, ...){mean(y, na.rm=na.rm)+3*sqrt(mean(y, na.rm=na.rm))}

#' @export
#' @title Lower Control Limit: Count Data (c-chart)
#' @description Calculates lower control limit (LCL) for count data acquired
#' over the same-sized area of opportunity. Negative values are reported as 0.
#' @inheritParams cBar_UCL
#' @return A number; 3-sigma lower control limit (LCL). Function returns 0 for negative values.
#' @examples
#' set.seed(5555)
#' y <- rpois(30, 9)
#' cBar_LCL(y)
#'
cBar_LCL <- function(y, na.rm=FALSE, ...){
  LCL <- mean(y, na.rm=na.rm)-3*sqrt(mean(y, na.rm=na.rm))
  LCL[LCL < 0] <- 0
  return(LCL)
}


# Binomial np-chart -----------------------------------------------------------
binCheck_pChart <- function(p, n, na.rm=FALSE){
  sum(as.integer(p*n > n), na.rm=na.rm) == 0
  }
npChartErrorMessage <- "Items of Opportunity 'n' < Item Nonconforming check value of 'n'."
#' @export
#' @title Upper Control Limit: Binomial Data (np-chart)
#' @description Calculates upper control limit (UCL) for
#' binomial count data acquired over the same-sized area of
#' opportunity.
#' @param y Vector of binomial count data (not proportions). Each observation
#' having the same-area of opportunity.
#' @param n A number representing the area of opportunity.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' @return A number; 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar_UCL(y = p, n = 30)
#'

npBar_UCL <- function(y, n, na.rm=FALSE, ...){
  y <- y/n
  if(binCheck_pChart(y, n, na.rm=na.rm)){
    n*mean(y, na.rm=na.rm)+3*sqrt(n*mean(y, na.rm=na.rm)*(1-mean(y, na.rm=na.rm)))
  }else{
    warning(npChartErrorMessage)
  }
}

#' @export
#' @title Mean Value: Binomial Data (np-chart)
#' @description Calculates the mean value for
#' binomial count data acquired over the same-sized area of
#' opportunity.
#' @inheritParams npBar_UCL
#' @return A number; mean value
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar(y = p, n = 30)
#'
npBar <- function(y, n, na.rm=FALSE, ...){
  y <- y/n
  n*mean(y, na.rm=na.rm)
}

#' @export
#' @title Lower Control Limit: Binomial Data (np-chart)
#' @description Calculates lower control limit (LCL) for
#' binomial count data acquired over the same-sized area of
#' opportunity.
#' @inheritParams npBar_UCL
#' @return A number; 3-sigma upper control limit (LCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar_LCL(y = p, n = 30)
#'

npBar_LCL <- function(y, n, na.rm=FALSE, ...){
  y <- y/n
  if(binCheck_pChart(y, n, na.rm=na.rm)){
    LCL <- n*mean(y,na.rm=na.rm)-3*sqrt(n*mean(y, na.rm=na.rm)*(1-mean(y, na.rm=na.rm)))
    LCL[LCL < 0] <- 0
    return(LCL)
  }else{
    warning(npChartErrorMessage)
  }
}

# Binomial p-chart --------------------------------------------------------
#' @export
#' @title Upper Control Limit: Binomial Data (p-chart)
#' @description Calculates point-wise upper control limit (UCL)
#' for binomial proportion data acquired over a variable area of
#' opportunity.
#' @param y Vector of binomial proportion data (not counts). Observations
#' may have a different area of opportunity, n.
#' @param n A vector representing the area of opportunity.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' @return A vector; point-wise 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' n <- rpois(100, 100)
#' pBar_UCL(y = p/n, n = n)
#'

pBar_UCL <- function(y, n, na.rm=FALSE, ...){
  PBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    PBAR_DF <- stats::na.omit(PBAR_DF)
  }else if (!sum(is.na(PBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }


    pbar <- pBar2(PBAR_DF$y,PBAR_DF$n, na.rm=na.rm)
    UCL <- pbar+(3*sqrt( pbar*(1-pbar) / PBAR_DF$n ))

  return(UCL)
}

#' @export
#' @title Mean Proportion: Binomial Data (p-chart)
#' @description Calculates overall mean proportion
#' for binomial proportion data acquired over a variable area of
#' opportunity.
#' @inheritParams pBar_UCL
#' @return A vector of mean proportion, length equal to length of
#' parameter y.
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' n <- rpois(100, 100)
#' pBar(y = p/n, n = n)
#'

pBar <- function(y, n, na.rm=FALSE, ...){
  PBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    PBAR_DF <- stats::na.omit(PBAR_DF)
  }else if (!sum(is.na(PBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }

  pbar <- sum(PBAR_DF$n * PBAR_DF$y, na.rm = na.rm)/sum(PBAR_DF$n, na.rm = na.rm)
  return(rep(pbar, nrow(PBAR_DF)))
}

pBar2 <- function(y, n, na.rm=FALSE, ...){
  PBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    PBAR_DF <- stats::na.omit(PBAR_DF)
  }else if (!sum(is.na(PBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }

  pbar <- sum(PBAR_DF$n * PBAR_DF$y, na.rm=na.rm)/sum(PBAR_DF$n, na.rm=na.rm)
  pbar
}


#' @export
#' @title Lower Control Limit: Binomial Data (p-chart)
#' @description Calculates point-wise lower control limit (LCL)
#' for binomial proportion data acquired over a variable area of
#' opportunity.
#' @inheritParams pBar_UCL
#' @return A vector; point-wise 3-sigma lower control limit (LCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' n <- rpois(100, 100)
#' pBar_LCL(y = p/n, n = n)
#'
pBar_LCL <- function(y, n, na.rm=FALSE, ...){
  PBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    PBAR_DF <- stats::na.omit(PBAR_DF)
  }else if (!sum(is.na(PBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }

  pbar <- pBar2(PBAR_DF$y,PBAR_DF$n, na.rm=na.rm)
  LCL <- pbar-(3*sqrt( pbar*(1-pbar) / PBAR_DF$n ))
  LCL[LCL < 0] <- 0

  return(LCL)
}

# Binomial u-chart --------------------------------------------------------
#' @export
#' @title Upper Control Limit: Count Data (u-chart)
#' @description Calculates point-wise upper control limit (UCL)
#' for count data acquired over a variable area of
#' opportunity.
#' @param y Vector of counts per unit opportunity (rate). Observations
#' may have a different area of opportunity, n.
#' @param n A vector representing the area of opportunity.
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' @return A vector; point-wise 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' counts <- rpois(100, 25)
#' n <- rpois(100, 15)
#' uBar_UCL(y = counts / n, n = n)
#'
uBar_UCL <- function(y, n, na.rm=FALSE, ...){
  UBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    UBAR_DF <- stats::na.omit(UBAR_DF)
  }else if (!sum(is.na(UBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }
  UCL <- uBar(UBAR_DF$y, UBAR_DF$n, na.rm = na.rm) + 3*sqrt( uBar(UBAR_DF$y, UBAR_DF$n, na.rm = na.rm) / UBAR_DF$n )
  return(UCL)
}

#' @export
#' @title Mean Rate: Count Data (u-chart)
#' @description Calculates overall mean rate
#' for count data acquired over a variable area of
#' opportunity.
#' @inheritParams uBar_UCL
#' @return A vector of mean rate, length equal to length of
#' parameter y.
#' @examples
#' set.seed(5555)
#' counts <- rpois(100, 25)
#' n <- rpois(100, 15)
#' uBar(y = counts / n, n = n)
#'
uBar <- function(y, n, na.rm = FALSE, ...){
  UBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    UBAR_DF <- stats::na.omit(UBAR_DF)
  }else if (!sum(is.na(UBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }
  ubar <- sum(UBAR_DF$n * UBAR_DF$y)/sum(UBAR_DF$n)
  rep(ubar, nrow(UBAR_DF))
}

#' @export
#' @title Lower Control Limit: Count Data (u-chart)
#' @description Calculates point-wise lower control limit (LCL)
#' for count data acquired over a variable area of
#' opportunity.
#' @inheritParams uBar_UCL
#' @return A vector; point-wise 3-sigma lower control limit (LCL)
#' @examples
#' set.seed(5555)
#' counts <- rpois(100, 25)
#' n <- rpois(100, 15)
#' uBar_LCL(y = counts / n, n = n)
#
uBar_LCL <- function(y, n, na.rm=FALSE, ...){
  UBAR_DF <- data.frame(y = y, n = n)
  if(na.rm == TRUE){
    UBAR_DF <- stats::na.omit(UBAR_DF)
  }else if (!sum(is.na(UBAR_DF)) == 0) {
    warning("NAs in args 'y' or 'n', resolve or use na.rm = FALSE")
    return(NULL)
  }
  LCL <- uBar(UBAR_DF$y, UBAR_DF$n, na.rm = na.rm) - 3*sqrt( uBar(UBAR_DF$y, UBAR_DF$n, na.rm = na.rm) / UBAR_DF$n )
  LCL[LCL < 0] <- 0
  return(LCL)
}


# Xbar.One Functions ------------------------------------------------------

#' @export
#' @title Mean One-Point Moving Range
#' @description Calculates the mean one-point moving range used when constructing a moving-range chart.
#' @param y Vector of values
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @param ... further arguments passed to or from other methods.
#' @return A number; mean one-point moving range.
#' @examples
#' set.seed(5555)
#' values <- rnorm(n = 100, mean = 25, sd = 1)
#' mR(values)
mR <- function(y, na.rm = TRUE, ...) {mean(abs(diff(y)), na.rm = na.rm)}

#' @export
#' @title Mean One-Point Moving Range Upper Control Limit (UCL)
#' @description Calculates the mean one-point moving range UCL used when constructing a moving-range chart.
#' @inheritParams mR
#' @return A number; mean one-point moving range UCL.
#' @examples
#' set.seed(5555)
#' values <- rnorm(n = 100, mean = 25, sd = 1)
#' mR_UCL(values)
mR_UCL <- function(y, na.rm = FALSE, ...) {mR(y, na.rm = na.rm, ...)*3.268}

#' @export
#' @title xBar_One Upper Control Limit (UCL)
#' @description Calculates the xBar_One UCL used when constructing a xBar-One chart.
#' @inheritParams mR
#' @return A number; xBar_One Upper Control Limit (UCL)
#' @examples
#' set.seed(5555)
#' values <- rnorm(n = 100, mean = 25, sd = 1)
#' xBar_one_UCL(values)
xBar_one_UCL <- function(y, na.rm = FALSE, ...) {mean(y, na.rm = na.rm, ...) + 2.66 * mR(y, na.rm = na.rm, ...)}

#' @export
#' @title xBar_One Lower Control Limit (LCL)
#' @description Calculates the xBar_One LCL used when constructing a xBar-One chart.
#' @inheritParams mR
#' @return A number; xBar_One Lower Control Limit (LCL)
#' @examples
#' set.seed(5555)
#' values <- rnorm(n = 100, mean = 25, sd = 1)
#' xBar_one_LCL(values)
xBar_one_LCL <- function(y, na.rm = FALSE, ...) {mean(y, na.rm = na.rm, ...) - 2.66 * mR(y, na.rm = na.rm, ...)}

# Dispersion Central Limit Functions ----------------------------------------------------
#' @export
#' @title Mean Subgroup Range
#' @description Calculates the mean subgroup range used when constructing a XbarR chart.
#' @param data data frame to be processed
#' @param value numeric vector in a data frame with values of interest.
#' @param grouping single factor/variable to split the data frame "values" by.
#' @param formula a formula, such as y ~ x1 + x2, where the y variable is
#'   numeric data to be split into groups according to the grouping x
#'   factors/variables
#' @param ... further arguments passed to or from other methods.
#' @return A number; mean subgroup range.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rBar(data = df, formula = v~g)
rBar <- dispersionFUN(QCrange, mean)

#' @export
#' @title Median of Subgroup Ranges
#' @description Calculates the median of subgroup ranges, used when constructing xBar_rMedian charts.
#' @inheritParams rBar
#' @return A number; median subgroup range.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rMedian(data = df, formula = v~g)
rMedian <- dispersionFUN(QCrange, stats::median)

#' @export
#' @title Mean Subgroup Standard Deviation
#' @description Calculates the mean subgroup standard deviation used when constructing a XbarS chart.
#' @inheritParams rBar
#' @return A number; mean subgroup standard deviation.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' sBar(data = df, formula = v~g)
sBar <- dispersionFUN(sd, mean)

# Dispersion Limit Functions ----------------------------------------------------
# +---------------+---------------+------------------+
# |               | lower_limit   | upper_limit      |
# +===============+===============+==================+
# | rBar          | D3            | D4               |
# +---------------+---------------+------------------+
# | rMedian       | D5            | D6               |
# +---------------+---------------+------------------+
# | sBar          | B3            | B4               |
# +---------------+---------------+------------------+

#' @export
#' @title Mean Subgroup Range Upper Control Limit (UCL)
#' @description Calculates the mean subgroup range upper control limit
#'  (UCL) used when constructing a XbarR chart.
#' @inheritParams rBar
#' @return A number; mean subgroup range upper control limit (UCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rBar_UCL(data = df, formula = v~g)
rBar_UCL <- DispersionLimitFun(rBar, "+")

#' @export
#' @title Mean Subgroup Range Lower Control Limit (LCL)
#' @description Calculates the mean subgroup range Lower control limit
#'  (UCL) used when constructing a XbarR chart.
#' @inheritParams rBar
#' @return A number; mean subgroup range lower control limit (LCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rBar_LCL(data = df, formula = v~g)
rBar_LCL <- DispersionLimitFun(rBar, "-")

#' @export
#' @title Median of Subgroup Ranges Upper Control Limit (UCL)
#' @description Calculates the median of subgroup range upper control limit
#'  (UCL) used when constructing a xBar_rMedian chart.
#' @inheritParams rBar
#' @return A number; median of subgroup range upper control limit (UCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rMedian_UCL(data = df, formula = v~g)
rMedian_UCL <- DispersionLimitFun(rMedian, "+")

#' @export
#' @title Median of Subgroup Ranges Lower Control Limit (LCL)
#' @description Calculates the median of subgroup range Lower control limit
#'  (LCL) used when constructing a xBar_rMedian chart.
#' @inheritParams rBar
#' @return A number; median of subgroup range lower control limit (LCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' rMedian_LCL(data = df, formula = v~g)
rMedian_LCL <- DispersionLimitFun(rMedian, "-")

#' @export
#' @title Mean Subgroup Standard Deviation Upper Control Limit (UCL)
#' @description Calculates the mean subgroup standard deviation upper control limit
#'  (UCL) used when constructing a XbarS chart.
#' @inheritParams rBar
#' @return A number; mean subgroup standard deviation upper control limit (UCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' sBar_UCL(data = df, formula = v~g)
sBar_UCL <- DispersionLimitFun(sBar, "+")

#' @export
#' @title Mean Subgroup Standard Deviation Lower Control Limit (LCL)
#' @description Calculates the mean subgroup standard deviation Lower control limit
#'  (UCL) used when constructing a XbarS chart.
#' @inheritParams rBar
#' @return A number; mean subgroup standard deviation lower control limit (LCL).
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' sBar_LCL(data = df, formula = v~g)
sBar_LCL <- DispersionLimitFun(sBar, "-")


# Central Limit Functions -----------------------------------------------
#' @export
#' @title Mean of Subgroup Means
#' @description Calculates the mean subgroup means used when constructing a xBar-R or xBar-S charts.
#' @inheritParams rBar
#' @return A number; mean of subgroup means.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_Bar(data = df, formula = v~g)
xBar_Bar <- XCentral_LimitFUN(mean)

#See Wheeler USPC 232
#' @export
#' @title Mean of Subgroup Medians
#' @description Calculates the mean of subgroup medians used when constructing a xMedian-R charts.
#' @inheritParams rBar
#' @return A number; mean of subgroup medians.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xMedian_Bar(data = df, formula = v~g)
xMedian_Bar <- XCentral_LimitFUN(stats::median)

# X-Limit Functions (+/-) ------------------------------
# +---------------+---------------+------------------+
# |               | mean(mean(x)) |mean(median(x))   |
# +===============+===============+==================+
# | rBar          | d2            | b2               |
# +---------------+---------------+------------------+
# | rMedian       | d4            | b4               |
# +---------------+---------------+------------------+
# | sBar          | c4            |                  |
# +---------------+---------------+------------------+
#' @export
#' @title Mean of Subgroup Means Upper Control Limit (UCL)
#' @description Calculates the mean of subgroup means upper control limit used when constructing a xBar-R charts.
#' @inheritParams rBar
#' @param n a number indicating a hypothetical subgroup size other than, function determined subgroup n
#' determined by the floor length of subgroup values.
#' @param natural logical, if TRUE calculate limits for individuals (n=1) else calculate for
#' n determined by the floor length of subgroup values
#' @return A number; mean of subgroup means upper control limit.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_rBar_UCL(data = df, formula = v~g)
xBar_rBar_UCL <- xLimitFun(mean, rBar, "+")

#' @export
#' @title Mean of Subgroup Means Lower Control Limit (LCL)
#' @description Calculates the mean of subgroup means lower control limit used when constructing a xBar-R charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means lower control limit.
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_rBar_LCL(data = df, formula = v~g)
xBar_rBar_LCL <- xLimitFun(mean, rBar, "-")

#' @export
#' @title Mean of Subgroup Means Upper Control Limit (UCL) based on Median Range
#' @description Calculates the mean of subgroup means upper control limit based on the
#' median range. The result is used when constructing a xBar-rMedian charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means Upper Control Limit (UCL) based on Median Range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_rMedian_UCL(data = df, formula = v~g)
xBar_rMedian_UCL <- xLimitFun(mean, rMedian, "+")

#' @export
#' @title Mean of Subgroup Means Lower Control Limit (LCL) based on Median Range
#' @description Calculates the mean of subgroup means lower control limit based on the
#' median range. The result is used when constructing a xBar-rMedian charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means Lower Control Limit (LCL) based on Median Range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_rMedian_LCL(data = df, formula = v~g)
xBar_rMedian_LCL <- xLimitFun(mean, rMedian, "-")

#' @export
#' @title Mean of Subgroup Means Upper Control Limit (UCL) based on Standard Deviation
#' @description Calculates the mean of subgroup means upper control limit based on the
#' standard deviation. The result is used when constructing a xBar-S charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means Upper Control Limit (UCL) based on standard deviation
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_sBar_UCL(data = df, formula = v~g)
xBar_sBar_UCL <- xLimitFun(mean, sBar, "+")

#' @export
#' @title Mean of Subgroup Means Lower Control Limit (LCL) based on Standard Deviation
#' @description Calculates the mean of subgroup means lower control limit based on the
#' standard deviation. The result is used when constructing a xBar-S charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means Lower Control Limit (LCL) based on standard deviation
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_sBar_LCL(data = df, formula = v~g)
xBar_sBar_LCL <- xLimitFun(mean, sBar, "-")

#' @export
#' @title Mean of Subgroup Medians Upper Control Limit (UCL) based on mean Range
#' @description Calculates the mean of subgroup medians upper control limit based on the
#' mean subgroup range. The result is used when constructing a xMedian-R charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup means Upper Control Limit (UCL) based on Median Range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xMedian_rBar_UCL(data = df, formula = v~g)
xMedian_rBar_UCL <- xLimitFun(stats::median, rBar, "+")

#' @export
#' @title Mean of Subgroup Medians Lower Control Limit (LCL) based on Mean Range
#' @description Calculates the mean of subgroup medians lower control limit based on the
#' mean range. The result is used when constructing a xMedian-R charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup medians Lower Control Limit (LCL) based on mean range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xBar_rMedian_LCL(data = df, formula = v~g)
xMedian_rBar_LCL <- xLimitFun(stats::median, rBar, "-")

#' @export
#' @title Mean of Subgroup Medians Upper Control Limit (UCL) based on Median Range
#' @description Calculates the mean of subgroup medians upper control limit based on the
#' median subgroup range. The result is used when constructing a xMedian-rMedian charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup median upper Control Limit (UCL) based on Median Range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xMedian_rMedian_UCL(data = df, formula = v~g)
xMedian_rMedian_UCL <- xLimitFun(stats::median, rMedian, "+")

#' @export
#' @title Mean of Subgroup Medians Lower Control Limit (LCL) based on Median Range
#' @description Calculates the mean of subgroup medians lower control limit based on the
#' median subgroup range. The result is used when constructing a xMedian-rMedian charts.
#' @inheritParams xBar_rBar_UCL
#' @return A number; mean of subgroup median Lower Control Limit (LCL) based on Median Range
#' @examples
#' set.seed(5555)
#' df <- data.frame(v=rnorm(60, 0, 1), g=rep(c("A","B","C","D","E"), each=12))
#' xMedian_rMedian_LCL(data = df, formula = v~g)
xMedian_rMedian_LCL <- xLimitFun(stats::median, rMedian, "-")




