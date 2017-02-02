# Xbar.One Functions ------------------------------------------------------
mR <- function(y) {mean(abs(diff(y)))}
xBar_one_UCL <- function(y) {mean(y) + 2.66 * mR(y)}
xBar_one_LCL <- function(y) {mean(y) - 2.66 * mR(y)}

# Dispersion Functions ----------------------------------------------------
rBar <- dispersionFUN(function(x){max(x)-min(x)}, mean)
rMedian <- dispersionFUN(function(x){max(x)-min(x)}, median)
sBar <- dispersionFUN(sd, mean)

# Central Limit Functions -----------------------------------------------
xBar_Bar <- XCentral_LimitFUN(mean)
xMedian_Bar <- XCentral_LimitFUN(median)

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

xBar_rBar_UCL <- xLimitFun(mean, rBar, "+")
xBar_rBar_LCL <- xLimitFun(mean, rBar, "-")
xBar_rMedian_UCL <- xLimitFun(mean, rMedian, "+")
xBar_rMedian_LCL <- xLimitFun(mean, rMedian, "-")
xBar_sBar_UCL <- xLimitFun(mean, sBar, "+")
xBar_sBar_LCL <- xLimitFun(mean, sBar, "-")
xMedian_rBar_UCL <- xLimitFun(median, rBar, "+")
xMedian_rBar_LCL <- xLimitFun(median, rBar, "-")
xMedian_rMedian_UCL <- xLimitFun(median, rMedian, "+")
xMedian_rMedian_LCL <- xLimitFun(median, rMedian, "-")


# N needed by Functions ---------------------------------------------------
NFUN <- function(data, value, grouping, ...){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  N <- floor(mean(aggregate(f1, FUN="length", data = data)[,2]))
  N
}


