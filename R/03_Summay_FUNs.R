# 2nd Order Functions ------------------------------------------------------
  # Report Lines for XmR chart



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
#' @title Calculate QC Limits
#' @description Calculates QC chart lines for the following chart types:
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian,
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @param data vector or dataframe, as indicated below for each chart type
#' \itemize{
#' \item \bold{Indivduals & Attribute Charts}: vector of values;
#' \item \bold{Studentized & Dispersion Charts}: dataframe
#' }
#' @param value \bold{Studentize Charts} and \bold{Dispersion Charts}, numeric vector in dataframe with values of interest
#' @param grouping \bold{Studentize Charts} and \bold{Dispersion Charts}: single factor/variable to split
#' the dataframe "values" by
#' @param formula \bold{Studentize Charts} and \bold{Dispersion Charts}: a formula,
#' such as y ~ x1 + x2, where the y variable is
#' numeric data to be split into groups according to the grouping x
#' factors/variables
#' @param n number or vector as indicated below for each chart type.
#' \itemize{
#' \item \bold{Individuals Charts}: No effect
#' \item \bold{Attribute Charts}: (p and u) vector, indicating sample area of opportunity.
#' \item \bold{Studentized Charts}: number, user specified subgroup size.
#' \item \bold{Dispersion Charts}: No effect
#' }
#' @param method string, calling the following methods:
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @return a dataframe,
#' \itemize{
#' \item \bold{Attribute Data:} (p and u) Center Line, Upper Control Limit and Lower Control limit for each point.
#' \item \bold{Other Data}: single line dataframe, with relevant control limits noted in column headings.
#' }
#' @note If using the \bold{formula} argument do not use \bold{value} and \bold{group} arguments.
#' @references Wheeler, DJ, and DS Chambers. Understanding Statistical Process Control, 2nd Ed. Knoxville, TN: SPC, 1992. Print.
#' @examples
#' #############################################
#' #  Example 1: Charts other than "p" or "u"  #
#' #############################################
#'
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(plyr)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(processID = as.factor(rep(1,100)),
#'                         metric_value = rnorm(100,0,1),
#'                         subgroup_sample=rep(1:20, each=5),
#'                         Process_run_id = 1:100)
#'  set.seed(5555)
#'  Process2 <- data.frame(processID = as.factor(rep(2,100)),
#'                         metric_value = rnorm(100,5, 1),
#'                         subgroup_sample=rep(1:10, each=10),
#'                         Process_run_id = 101:200)
#'
#'  Both_Processes <- rbind(Process1, Process2)
#'
#'# QC Values For Individuals -----------------------------------------------
#'  # All Together
#'    QC_Lines(data = Both_Processes$metric_value, method = "XmR")
#'
#'
#'  # For Each Process
#'    ddply(Both_Processes, .variables = "processID",
#'      .fun =function(df){
#'        QC_Lines(data = df$metric_value, method = "XmR")
#'      }
#'    )
#'
#'# QC Values For Studentized Runs-------------------------------------------
#'  # All Together
#'    QC_Lines(data = Both_Processes,
#'         formula = metric_value ~ subgroup_sample)
#'
#'
#'  # For Each Process
#'    ddply(Both_Processes, .variables = "processID",
#'      .fun =function(df){
#'        QC_Lines(data = df, formula = metric_value ~ subgroup_sample)
#'      }
#'    )
#'
#'
#'########################
#'#  Example 2 "p" data  #
#'########################
#'
#'# Setup p Data ------------------------------------------------------------
#'  set.seed(5555)
#'  bin_data <- data.frame(
#'    trial = 1:30,
#'    Num_Incomplete_Items = rpois(n = 30, lambda = 30),
#'    Num_Items_in_Set = runif(n = 30, min = 50, max = 100))
#'
#'  bin_data$Proportion_Incomplete <- bin_data$Num_Incomplete_Items/bin_data$Num_Items_in_Set
#'
#'# QC_Lines for "p" data ---------------------------------------------------
#'  QC_Lines(data = bin_data$Proportion_Incomplete,
#'         n = bin_data$Num_Items_in_Set, method="p")
#'
#'
#'########################
#'#  Example 3 "u" data  #
#'########################
#'
#'# Setup u Data ------------------------------------------------------------
#'  set.seed(5555)
#'  bin_data <- data.frame(
#'    trial=1:30,
#'    Num_of_Blemishes = rpois(n = 30, lambda = 30),
#'    Num_Items_Inspected = runif(n = 30, min = 50, max = 100))
#'
#'  bin_data$Blemish_Rate <- bin_data$Num_of_Blemishes/bin_data$Num_Items_Inspected
#'
#'
#'# QC Lines for "u" data ---------------------------------------------------
#'  QC_Lines(data = bin_data$Blemish_Rate,
#'         n = bin_data$Num_Items_Inspected, method="u")





QC_Lines <- function(data=NULL, value=NULL, grouping=NULL, formula=NULL, n=NULL, method="xBar.rBar", na.rm = FALSE){
  switch(method,
         "mR" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = 1, method = "mR", na.rm = na.rm))
           }else{
             return(message("Error: method 'mR' requires a vector not a dataframe in data arg."))

             }
          },
         "XmR" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = 1, method = "XmR", na.rm = na.rm))
           }else{
             return(message("Error: method 'XmR' requires a vector not a dataframe in data arg."))

           }
         },
         "c" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = 1, method = "c", na.rm = na.rm))
           }else{
             return(message("Error: method 'c' requires a vector not a dataframe in data arg."))

           }
         },
         "np" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = n, method = "np", na.rm = na.rm))
           }else{
             return(message("Error: method 'np' requires a vector not a dataframe in data arg."))

           }
         },
         "p" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = n, method = "p", na.rm = na.rm))
           }else{
             return(message("Error: method 'p' requires a vector not a dataframe in data arg."))

           }
         },
         "u" = {
           if(is.vector(data)){
             return(ylines_indv(y = data, n = n, method = "u", na.rm = na.rm))
           }else{
             return(message("Error: method 'u' requires a vector not a dataframe in data arg."))

           }
         },
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
