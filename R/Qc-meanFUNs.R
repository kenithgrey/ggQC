
# QC Constants ------------------------------------------------------------
qcK <- read.csv(file = "Stat/Constants_Table.csv", header=T)


# Xbar.One Function List --------------------------------------------------
# mR <- function(y)
# xBar_one_UCL <- function(y)
# xBar_one_LCL <- function(y)


# Xbar.One Functions ------------------------------------------------------
mR <- function(y) {
  mean(abs(diff(y)))
}

xBar_one_UCL <- function(y) {
  mean(y) + qcK[qcK$n == 2, "E2"] * mR(y)
}

xBar_one_LCL <- function(y) {
  mean(y) - qcK[qcK$n == 2, "E2"] * mR(y)
}

# Xbar Rbar Function List -------------------------------------------------
# rBar <- function(data, value, grouping)
# xBar_UCL <- function(data, value, grouping, n=2, natural = F)
# xBar_Bar <- function(data, value, grouping, n=2, natural = F)
# xBar_LCL <- function(data, value, grouping, n=2, natural = F)

# Xbar Rbar Functions  -----------------------------------------------------
rBar <- function(data, value, grouping, ...){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  agg <- aggregate(f1, function(x){max(x)-min(x)},data = data)
  mean(agg[,2])
}

#rBar(data = df_all, value = "DIN", grouping = "subgroup5")

d2_N <- function(data, value, grouping, ...){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  N <- floor(mean(aggregate(f1, FUN="length", data = data)[,2]))
  N
}

xBar_UCL <- function(data, value, grouping, n=NULL, natural = F){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  agg <- aggregate(f1,
                   FUN=mean,
                   data = data)
  N <- floor(mean(aggregate(f1, FUN="length", data = data)[,2]))

  if(is.null(n)){n <- N}
  else if(natural == T){n <- 1}
  else{n=n}

  BCF <- 3/(qcK[qcK$n == N,"d2"]*sqrt(n)) #if n=1 than == E2
  mean(agg[,2])+rBar(data=data, value=value, grouping=grouping)*BCF

}

#xBar_UCL(data = df_all, value = "DIN", grouping = "subgroup5")

xBar_Bar <- function(data, value, grouping, n=2, natural = F){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  agg <- aggregate(f1,
                   FUN=mean,
                   data = data)
  mean(agg[,2])
}


xBar_LCL <- function(data, value, grouping, n=2, natural = F){
  f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  agg <- aggregate(f1,
                   FUN=mean,
                   data = data)
  N <- floor(mean(aggregate(f1, FUN="length", data = data)[,2]))
  if(is.null(n)){n <- N}
  else if(natural == T){n <- 1}
  else{n=n}

  BCF <- 3/(qcK[qcK$n == N,"d2"]*sqrt(n))
  mean(agg[,2])-rBar(data=data, value=value, grouping=grouping)*BCF

}

# 2nd Order Functions List ------------------------------------------------------
# ylines_indv <- function(y)
# ylines_XbarR <- function(data=NULL, value=NULL, grouping=NULL, n=NULL)

# 2nd Order Functions ------------------------------------------------------
ylines_indv <- function(y){
  QC_indv_functions <- list(mR = mR,
                            xBar_one_LCL = xBar_one_LCL,
                            mean = mean,
                            xBar_one_UCL = xBar_one_UCL)

  unlist(lapply(QC_indv_functions,
                             FUN = function(f){f(y)}))
}

ylines_XbarR <- function(data=NULL, value=NULL, grouping=NULL, n=NULL){
  QC_XbarR_functions <- list(rBar = rBar,
                             d2_N = d2_N,
                             xBar_LCL = xBar_LCL,
                             xBar_Bar = xBar_Bar,
                             xBar_UCL = xBar_UCL)

  unlist(lapply(QC_XbarR_functions,
          FUN = function(f){f(data=data, value=value, grouping=grouping, n=n)}))
}

# Test Data ---------------------------------------------------------------
# ylines_indv(rn_data$y)
# ylines_XbarR(rn_data, "y", "grouping", n = 10)
# mean(aggregate(y~grouping, FUN=mean, data = rn_data)[,2])
# rBar(data = rn_data, value = "y", grouping = "grouping", n=NULL)
# UCL10<-xBar_UCL(data = rn_data, value = "y", grouping = "grouping", n=10 )
# LCL10<-xBar_LCL(data = rn_data, value = "y", grouping = "grouping", n=10 )
# mmX <- xBar_Bar(data = rn_data, value = "y", grouping = "grouping", n=10 )
# mR(rn_data)
# mean(rn_data)
#
#
# set.seed(1)
#rn_data <- data.frame(y = rnorm(100, 0, 1),
#                       grouping = rep(letters[1:10], each = 10))
#
# plot(rn_data$y, ylim=c(-4,4))
# abline(h=ylines_indv[-1], col="blue")
# abline(h=ylines_XbarR[-1], col="red")
# points(aggregate(y~grouping, data=rn_data, FUN=mean)[,2], col="red", pch=20)

# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

