# 2nd Order Functions ------------------------------------------------------
  # Report Lines for XmR chart
ylines_indv <- function(y){
  QC_indv_functions <- list(mR = mR,
                            xBar_one_LCL = xBar_one_LCL,
                            mean = mean,
                            xBar_one_UCL = xBar_one_UCL)

  unlist(lapply(QC_indv_functions,
                FUN = function(f){f(y)}))
}

#Report lines for subgroup plots
QC_Lines <- function(data=NULL, value=NULL, grouping=NULL, n=NULL, method="xBar.rBar"){
  switch(method,
         "xBar.rBar" = {
           Lines <- list(rBar = rBar, d2_N = NFUN,
                         xBar_rBar_LCL = xBar_rBar_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_rBar_UCL = xBar_rBar_UCL)},
         "xBar.rMedian" = {
           Lines <- list(rMedian = rMedian, d4_N = NFUN,
                         xBar_rMedian_LCL = xBar_rMedian_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_rMedian_UCL = xBar_rMedian_UCL)},
         "xBar.sBar" = {
           Lines <- list(sBar = sBar, c4_N = NFUN,
                         xBar_sBar_LCL = xBar_sBar_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_sBar_UCL = xBar_sBar_UCL)},
         "xMedian.rBar" = {
           Lines <- list(rBar = rBar, b2_N = NFUN,
                         xMedian_rBar_LCL = xMedian_rBar_LCL,
                         xMedian_Bar = xMedian_Bar,
                         xMedian_rBar_UCL = xMedian_rBar_UCL)},
         "xMedian.rMedian" = {
           Lines <- list(rMedian = rMedian, b4_N = NFUN,
                         xMedian_rMedian_LCL = xMedian_rMedian_LCL,
                         xMedian_Bar = xMedian_Bar,
                         xMedian_rMedian_UCL = xMedian_rMedian_UCL)}
  )

  unlist(lapply(Lines,
                FUN = function(f){f(data=data, value=value, grouping=grouping, n=n)}))

}
