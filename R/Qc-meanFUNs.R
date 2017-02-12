# General Funs ------------------------------------------------------------
ZERO <- function(...){0}
QCrange <- function(y){max(y) - min(y)}
mR_points<- function(y){c(NA, abs(diff(y)))}
mR_points_gg <- dispersionFUN(mean, mR_points)


# Count Data --------------------------------------------------------------
cBar_UCL <- function(y, ...){mean(y)+3*sqrt(mean(y))}
cBar_LCL <- function(y, ...){mean(y)-3*sqrt(mean(y))}


# Binomial np-chart -----------------------------------------------------------
binCheck_pChart <- function(p, n){sum(as.integer(p*n > n)) == 0}
npBar_UCL <- function(y, n, ...){
  if(binCheck_pChart(y, n)){
    n*pBar(y)+3*sqrt(n*pBar(y)*(1-pBar(y)))
  }else{
    print("Error: Items of Opportunity 'n' < Item Nonconforming/n check value of 'n'.")
  }
}
#pBar <- function(y, ...){mean(y)}
npBar <- function(y, n){n*pBar(y)}
npBar_LCL <- function(y, n, ...){
  if(binCheck_pChart(y, n)){
    n*pBar(y)-3*sqrt(n*pBar(y)*(1-pBar(y)))
  }else{
    print("Error: Items of Opportunity 'n' < Item Nonconforming/n check value of 'n'.")
  }
}

# Binomial p-chart --------------------------------------------------------
pBar_UCL <- function(y, n, ...){(3*sqrt( mean(y)*(1-mean(y)) / n ))}
pBar <- function(y, ...){mean(y)}
pBar_LCL <- function(y, n, ...){-(3*sqrt( mean(y)*(1-mean(y)) / n ))}

# Binomial u-chart --------------------------------------------------------
uBar_UCL <- function(y, n, ...){(uBar(y) + 3*sqrt( uBar(y) / n ))}
uBar <- function(y, ...){mean(y)}
uBar_LCL <- function(y, n, ...){
  LCL <- uBar(y) -(3*sqrt( uBar(y) / n ))
  LCL[LCL < 0] <- 0
  #print(LCL)
  #if(LCL < 0) LCL <- 0
  return(LCL)
  }


# Xbar.One Functions ------------------------------------------------------
mR <- function(y, ...) {mean(abs(diff(y)))}
mR_UCL <- function(y, ...) {mR(y)*3.268}
xBar_one_UCL <- function(y, ...) {mean(y) + 2.66 * mR(y)}
xBar_one_LCL <- function(y, ...) {mean(y) - 2.66 * mR(y)}

# Dispersion Central Limit Functions ----------------------------------------------------
rBar <- dispersionFUN(QCrange, mean)
rMedian <- dispersionFUN(QCrange, median)
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

rBar_UCL <- DispersionLimitFun(rBar, "+")
rBar_LCL <- DispersionLimitFun(rBar, "-")
rMedian_UCL <- DispersionLimitFun(rMedian, "+")
rMedian_LCL <- DispersionLimitFun(rMedian, "-")
sBar_UCL <- DispersionLimitFun(sBar, "+")
sBar_LCL <- DispersionLimitFun(sBar, "-")


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




