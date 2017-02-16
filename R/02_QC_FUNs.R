# General Funs ------------------------------------------------------------
ZERO <- function(...){0}
QCrange <- function(y){max(y) - min(y)}
#' @export
#' @title One Point Moving Range
#' @description Calculates a one-point moving range vector given an input vector of values.
#'  Output often used to produce mR-chart.
#' @param y : vector of values
#' @return Vector of one point moving range.
#' @examples
#' y <- seq(-5:5)
#' mR_points(y)
#'
mR_points<- function(y){c(NA, abs(diff(y)))}

mR_points_gg <- dispersionFUN(mean, mR_points)


# Count Data --------------------------------------------------------------
#' @export
#' @title Upper Control Limit: Count Data (c-chart)
#' @description Calculates upper control limit (UCL) for count data aquired
#' over the same-sized area of opportunity.
#' @param y Vector of count data. Each observation having the
#'  same-area of opportinity.
#' @param ... further arguments passed to or from other methods.
#' @return A number; 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' y <- rpois(30, 9)
#' cBar_UCL(y)
#'

cBar_UCL <- function(y, ...){mean(y)+3*sqrt(mean(y))}
#' @export
#' @title Lower Control Limit: Count Data (c-chart)
#' @description Calculates lower control limit (LCL) for count data aquired
#' over the same-sized area of opportunity. Negative values are reported as 0.
#' @inheritParams cBar_UCL
#' @return A number; 3-sigma lower control limit (LCL). Function returns 0 for negative values.
#' @examples
#' set.seed(5555)
#' y <- rpois(30, 9)
#' cBar_LCL(y)
#'
cBar_LCL <- function(y, ...){
  LCL <- mean(y)-3*sqrt(mean(y))
  LCL[LCL < 0] <- 0
  return(LCL)
  }


# Binomial np-chart -----------------------------------------------------------
binCheck_pChart <- function(p, n){sum(as.integer(p*n > n)) == 0}
npChartErrorMessage <- "Items of Opportunity 'n' < Item Nonconforming check value of 'n'."
#' @export
#' @title Upper Control Limit: Binomial Data (np-chart)
#' @description Calculates upper control limit (UCL) for
#' binomial count data aquired over the same-sized area of
#' opportunity.
#' @param y Vector of binomial count data (not proportions). Each observation
#' having the same-area of opportinity.
#' @param n A number representing the area of opportunity.
#' @param ... further arguments passed to or from other methods.
#' @return A number; 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar_UCL(y = p, n = 30)
#'

npBar_UCL <- function(y, n, ...){
  y <- y/n
  if(binCheck_pChart(y, n)){
    n*mean(y)+3*sqrt(n*mean(y)*(1-mean(y)))
  }else{
    warning(npChartErrorMessage)
  }
}

#' @export
#' @title Mean Value: Binomial Data (np-chart)
#' @description Calculates the mean value for
#' binomial count data aquired over the same-sized area of
#' opportunity.
#' @inheritParams npBar_UCL
#' @return A number; mean value
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar(y = p, n = 30)
#'
npBar <- function(y, n,...){
  y <- y/n
  n*mean(y)
}

#' @export
#' @title Lower Control Limit: Binomial Data (np-chart)
#' @description Calculates lower control limit (LCL) for
#' binomial count data aquired over the same-sized area of
#' opportunity.
#' @inheritParams npBar_UCL
#' @return A number; 3-sigma upper control limit (LCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' npBar_LCL(y = p, n = 30)
#'

npBar_LCL <- function(y, n, ...){
  y <- y/n
  if(binCheck_pChart(y, n)){
    LCL <- n*mean(y)-3*sqrt(n*mean(y)*(1-mean(y)))
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
#' for binomial proportion data aquired over a variable area of
#' opportunity.
#' @param y Vector of binomial proportion data (not counts). Observations
#' may have a different area of opportinity, n.
#' @param n A vector representing the area of opportunity.
#' @param ... further arguments passed to or from other methods.
#' @return A vector; point-wise 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' n <- rpois(100, 100)
#' pBar_UCL(y = p/n, n = n)
#'

pBar_UCL <- function(y, n, ...){
    pbar <- pBar2(y,n)
    pbar+(3*sqrt( pbar*(1-pbar) / n ))
  }

#' @export
#' @title Mean Propotion: Binomial Data (p-chart)
#' @description Calculates overall mean porportion
#' for binomial proportion data aquired over a variable area of
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

pBar <- function(y, n,...){
  pbar <- sum(n * y)/sum(n)
  rep(pbar, length(n))
}

pBar2 <- function(y, n,...){
  pbar <- sum(n * y)/sum(n)
  pbar
}


#' @export
#' @title Lower Control Limit: Binomial Data (p-chart)
#' @description Calculates point-wise lower control limit (LCL)
#' for binomial proportion data aquired over a variable area of
#' opportunity.
#' @inheritParams pBar_UCL
#' @return A vector; point-wise 3-sigma lower control limit (LCL)
#' @examples
#' set.seed(5555)
#' p <- rbinom(n = 100, size = 30, prob = .2)
#' n <- rpois(100, 100)
#' pBar_LCL(y = p/n, n = n)
#'
pBar_LCL <- function(y, n, ...){
  pbar <- pBar2(y,n)
  LCL <- pbar-(3*sqrt( pbar*(1-pbar) / n ))
  LCL[LCL < 0] <- 0
  return(LCL)
  }

# Binomial u-chart --------------------------------------------------------
#' @export
#' @title Upper Control Limit: Count Data (u-chart)
#' @description Calculates point-wise upper control limit (UCL)
#' for count data aquired over a variable area of
#' opportunity.
#' @param y Vector of count data. Observations
#' may have a different area of opportinity, n.
#' @param n A vector representing the area of opportunity.
#' @param ... further arguments passed to or from other methods.
#' @return A vector; point-wise 3-sigma upper control limit (UCL)
#' @examples
#' set.seed(5555)
#' counts <- rpois(100, 25)
#' n <- rpois(100, 15)
#' uBar_UCL(y = counts, n = n)
#'
uBar_UCL <- function(y, n, ...){(mean(y) + 3*sqrt( mean(y) / n ))}

#' @export
#' @title Lower Control Limit: Count Data (u-chart)
#' @description Calculates point-wise lower control limit (LCL)
#' for count data aquired over a variable area of
#' opportunity.
#' @inheritParams uBar_UCL
#' @return A vector; point-wise 3-sigma lpper control limit (LCL)
#' @examples
#' set.seed(5555)
#' counts <- rpois(100, 25)
#' n <- rpois(100, 15)
#' uBar_LCL(y = counts, n = n)
#
uBar_LCL <- function(y, n, ...){
  LCL <- mean(y) -(3*sqrt( mean(y) / n ))
  LCL[LCL < 0] <- 0
  return(LCL)
  }


# Xbar.One Functions ------------------------------------------------------
#' @export
mR <- function(y, ...) {mean(abs(diff(y)))}
#' @export
mR_UCL <- function(y, ...) {mR(y)*3.268}
#' @export
xBar_one_UCL <- function(y, ...) {mean(y) + 2.66 * mR(y)}
#' @export
xBar_one_LCL <- function(y, ...) {mean(y) - 2.66 * mR(y)}

# Dispersion Central Limit Functions ----------------------------------------------------
#' @export
rBar <- dispersionFUN(QCrange, mean)
#' @export
rMedian <- dispersionFUN(QCrange, stats::median)
#' @export
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
rBar_UCL <- DispersionLimitFun(rBar, "+")
#' @export
rBar_LCL <- DispersionLimitFun(rBar, "-")
#' @export
rMedian_UCL <- DispersionLimitFun(rMedian, "+")
#' @export
rMedian_LCL <- DispersionLimitFun(rMedian, "-")
#' @export
sBar_UCL <- DispersionLimitFun(sBar, "+")
#' @export
sBar_LCL <- DispersionLimitFun(sBar, "-")


# Central Limit Functions -----------------------------------------------
#' @export
xBar_Bar <- XCentral_LimitFUN(mean)
#' @export
xMedian_Bar <- XCentral_LimitFUN(stats::median)

# X-Limit Functions (+/-) ------------------------------
# +---------------+---------------+------------------+
# |               | mean(x)       | median(x)        |
# +===============+===============+==================+
# | rBar          | d2            | b2               |
# +---------------+---------------+------------------+
# | rMedian       | d4            | b4               |
# +---------------+---------------+------------------+
# | sBar          | c4            |                  |
# +---------------+---------------+------------------+
#' @export
xBar_rBar_UCL <- xLimitFun(mean, rBar, "+")
#' @export
xBar_rBar_LCL <- xLimitFun(mean, rBar, "-")
#' @export
xBar_rMedian_UCL <- xLimitFun(mean, rMedian, "+")
#' @export
xBar_rMedian_LCL <- xLimitFun(mean, rMedian, "-")
#' @export
xBar_sBar_UCL <- xLimitFun(mean, sBar, "+")
#' @export
xBar_sBar_LCL <- xLimitFun(mean, sBar, "-")
#' @export
xMedian_rBar_UCL <- xLimitFun(stats::median, rBar, "+")
#' @export
xMedian_rBar_LCL <- xLimitFun(stats::median, rBar, "-")
#' @export
xMedian_rMedian_UCL <- xLimitFun(stats::median, rMedian, "+")
#' @export
xMedian_rMedian_LCL <- xLimitFun(stats::median, rMedian, "-")




