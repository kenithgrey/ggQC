# 2nd Order Functions ------------------------------------------------------
  # Report Lines for XmR chart


#' @export
ylines_indv <- function(y, n=1, method = "XmR", na.rm = FALSE){
  switch(method,
         "mR" = {
          QC_indv_functions <- list(
                mR_LCL = ZERO,
                mR = mR, mR_UCL = mR_UCL,
                xBar_one_LCL = xBar_one_LCL,
                mean = mean,
                xBar_one_UCL = xBar_one_UCL)
                },

         "XmR" = {
           QC_indv_functions <- list(
                xBar_one_LCL = xBar_one_LCL,
                mean = mean,
                xBar_one_UCL = xBar_one_UCL,
                mR_LCL = ZERO,mR = mR, mR_UCL = mR_UCL)
                 },
         "c" = {
           QC_indv_functions <- list(
                cBar_LCL = cBar_LCL,
                cBar = mean,
                cBar_UCL = cBar_UCL)
               },
         "np" = {
           QC_indv_functions <- list(
             npBar_LCL = npBar_LCL,
             npBar = npBar,
             npBar_UCL = npBar_UCL)
         },
         "p" = {
            p_chart_data <- data.frame(
                       pBar_LCL = pBar_LCL(y, n),
                       pBar = pBar(y, n),
                       pBar_UCL = pBar_UCL(y, n)
                       )
             return(p_chart_data)
         },
         "u" = {
           u_chart_data <- data.frame(
             uBar_LCL = uBar_LCL(y, n),
             uBar = uBar(y, n),
             uBar_UCL = uBar_UCL(y, n)
           )
           return(u_chart_data)
         }
        )

         #print(n)
           as.data.frame.list(
           unlist(lapply(QC_indv_functions,
                FUN = function(f){f(y, n=n, na.rm = na.rm)}))
           )
}

#Report lines for subgroup plots
#' @export
QC_Lines <- function(data=NULL, value=NULL, grouping=NULL, formula=NULL, n=NULL, method="xBar.rBar"){
  switch(method,
         "xBar.rBar" = {
           Lines <- list(rBar_LCL = rBar_LCL , rBar = rBar, rBar_UCL = rBar_UCL,
                         d2_N = NFUN,
                         xBar_rBar_LCL = xBar_rBar_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_rBar_UCL = xBar_rBar_UCL)},
         "xBar.rMedian" = {
           Lines <- list(rMedian_LCL = rMedian_LCL, rMedian = rMedian,
                         rMedian_UCL = rMedian_UCL, d4_N = NFUN,
                         xBar_rMedian_LCL = xBar_rMedian_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_rMedian_UCL = xBar_rMedian_UCL)},
         "xBar.sBar" = {
           Lines <- list(sBar_LCL = sBar_LCL,
                         sBar = sBar,
                         sBar_UCL = sBar_UCL, c4_N = NFUN,
                         xBar_sBar_LCL = xBar_sBar_LCL,
                         xBar_Bar = xBar_Bar,
                         xBar_sBar_UCL = xBar_sBar_UCL)},
         "xMedian.rBar" = {
           Lines <- list(rBar_LCL = rBar_LCL , rBar = rBar, rBar_UCL = rBar_UCL,
                         b2_N = NFUN,
                         xMedian_rBar_LCL = xMedian_rBar_LCL,
                         xMedian_Bar = xMedian_Bar,
                         xMedian_rBar_UCL = xMedian_rBar_UCL)},
         "xMedian.rMedian" = {
           Lines <- list(rMedian_LCL = rMedian_LCL, rMedian = rMedian,
                         rMedian_UCL = rMedian_UCL, b4_N = NFUN,
                         xMedian_rMedian_LCL = xMedian_rMedian_LCL,
                         xMedian_Bar = xMedian_Bar,
                         xMedian_rMedian_UCL = xMedian_rMedian_UCL)},
         "rBar" = {
           Lines <- list(N = NFUN, N = NFUN,
                         xBar_Bar = xBar_Bar,
                         N = NFUN,
                         rBar_LCL = rBar_LCL,
                         rBar = rBar,
                         rBar_UCL = rBar_UCL)},
         "rMedian" = {
           Lines <- list(N = NFUN, N = NFUN,
                         xBar_Bar = xBar_Bar, N = NFUN,
                         rMedian_LCL = rMedian_LCL,
                         rMedian = rMedian,
                         rMedian_UCL = rMedian_UCL)},
         "sBar" = {
           Lines <- list(N = NFUN, N = NFUN,
                         xBar_Bar = xBar_Bar, N = NFUN,
                         sBar_LCL = sBar_LCL,
                         sBar = sBar,
                         sBar_UCL = sBar_UCL)}

  )
  as.data.frame.list(
  unlist(lapply(Lines,
                FUN = function(f){f(data=data, value=value, grouping=grouping, formula=formula, n=n)}))
  )
}
