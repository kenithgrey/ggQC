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


# Rule Set ----------------------------------------------------------------
RuleSet <-
data.frame(Sigma_Rule = c(3,2,1,0),
  Description = c("Points Exceeding 3 Sigma",
                  "2 or more consecutive, same-side points exceeding 2 Sigma",
                  "4 or more consecutive, same-side points exceeding 1 Sigma",
                  "8 or more consecutive, same-side points"
                  ),
  Alt_Description = factor(c("Violation 3 Sigma","Violation 2 Sigma",
                      "Violation 1 Sigma","Violation Same Side"),
                      levels = c("Violation Same Side",
                                 "Violation 1 Sigma",
                                 "Violation 2 Sigma",
                                 "Violation 3 Sigma")),
  Rule = c(1:4),
  breaks = c(
    "c(-Inf,-3,-2,-1,0,1,2,3,Inf)",
    "c(-Inf,-2,-1,0,1,2,Inf)",
    "c(-Inf,-1,0,1,Inf)",
    "c(-Inf,0,Inf)"
    ),
  threshold = c(1,2,4,8),
  criteria = c(
    "%in% c('(-Inf,-3]', '(3, Inf]')",
    "%in% c('(-Inf,-2]', '(2, Inf]')",
    "%in% c('(-Inf,-1]', '(1, Inf]')",
    "%in% c('(-Inf,0]', '(0, Inf]')"
    ), stringsAsFactors = F
)

# RuleSet$Alt_Description <- transform(RuleSet,
#   Alt_Description = factor(Alt_Description,
#                  levels = ))

#devtools::use_data(RuleSet, internal = TRUE, overwrite = T)

# Find Run Length Violations ----------------------------------------------
Find_Run_Length_Violations <- function(Rule, Test_df) #calls out to RuleSet df
{
  ####
  # wheeler91 <- read.csv("tests/testthat/wheeler91.csv", header=T)
  # Test_Vector <- wheeler91$Average
  # method = "XmR"
  # Rule = 4
  ####
  #QCL <- QC_Lines(Test_df, method = method)
  #df$Rule_2_Results <- Find_Run_Length_Violations(2, Test_df = df)

  inputDF <- Test_df
  #inputDF$z_score <- (inputDF$Test_Vector-QCL$mean)/QCL$sigma
  inputDF$sigma_bin <- cut(inputDF$z_score, breaks = eval(parse(text = RuleSet$breaks[Rule])))


  output <- rle(as.vector(inputDF$sigma_bin))

  output_DF <-
    data.frame(runlength = output$lengths,
               sigma_level = output$values)

  output_DF$record_end <-
    cumsum(output_DF$runlength)

  output_DF$record_start <-
    (output_DF$record_end - output_DF$runlength)+1


  output_DF$results <-
    eval(parse(text =
                 paste0("output_DF$sigma_level ", RuleSet$criteria[Rule],
                        " & output_DF$runlength >=", RuleSet$threshold[Rule])))
  #head(output_DF, 10)
  return(unlist(sapply(1:nrow(output_DF),
                       FUN = function(X){
                         rep(output_DF$results[X],
                             each = output_DF$runlength[X])
                       }
  )
  )
  )
}


# QC Violations Function --------------------------------------------------
#' @export
#' @title Calculate QC Violations
#' @description function that calculates QC violations on sequentially ordered data
#' based on the following 4 rules:
#' \itemize{
#' \item \bold{Violation Same Side:} 8 or more consecutive, same-side points
#' \item \bold{Violation 1 Sigma:} 4 or more consecutive, same-side points exceeding 1 sigma
#' \item \bold{Violation 2 Sigma:} 2 or more consecutive, same-side points exceeding 2 sigma
#' \item \bold{Violation 3 Sigma:} any points exceeding 3 sigma
#' }
#'
#' @param method string, calling the following methods:
#' \itemize{
#'   \item \bold{Individuals Charts}: XmR,
#'   \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' }
#' @param data vector or dataframe, as indicated below for each chart type
#' \itemize{
#' \item \bold{Individuals}: vector of values;
#' \item \bold{Studentized Charts}: dataframe
#' }
#' @param value \bold{Studentized Charts}: numeric vector in dataframe with values of interest
#' @param grouping \bold{Studentized Charts}: single factor/variable to split
#' the dataframe "values" by
#' @param formula \bold{Studentized Charts}: a formula,
#' such as y ~ x1 + x2, where the y variable is
#' numeric data to be split into groups according to the grouping x
#' factors/variables
#' @param ... further arguments passed to or from other methods.
#' @return a dataframe, with the following columns
#' \itemize{
#' \item \bold{data}: The input data if XmR, mean or median by group for Studentized methods
#' \item \bold{z_score}: z-score for the data point
#' \item \bold{Index}: number, indicating the order of the input data
#' \item \bold{Violation_Result}: description of the type of test being run.
#'    \itemize{
#'     \item \bold{Violation Same Side:} 8 or more consecutive, same-side points
#'     \item \bold{Violation 1 Sigma:} 4 or more consecutive, same-side points exceeding 1 sigma
#'     \item \bold{Violation 2 Sigma:} 2 or more consecutive, same-side points exceeding 2 sigma
#'     \item \bold{Violation 3 Sigma:} any points exceeding 3 sigma
#'     }
#' \item \bold{Index}: boolean, does the data point violate the rule?
#' }
#' @note If using the \bold{formula} argument do not use \bold{value} and \bold{group} arguments.
#' @references Wheeler, DJ, and DS Chambers. Understanding Statistical Process Control, 2nd Ed. Knoxville, TN: SPC, 1992. Print.
#' @examples
#' #####################################
#' #  Example 1: XmR Check Violations  #
#' #####################################
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'
#'# Setup Data --------------------------------------------------------------
#'
#'     set.seed(5555)
#'     QC_XmR <- data.frame(
#'     data = c(c(-1, 2.3, 2.4, 2.5),                        #Outlier Data
#'           sample(c(rnorm(60),5,-5), 62, replace = FALSE), #Normal Data
#'           c(1,-.3, -2.4,-2.6,-2.5,-2.7, .3)),             #Outlier Data
#'     Run_Order = 1:73                                      #Run Order
#'     )
#'
#'    QC_Vs <- QC_Violations(data  = QC_XmR$data, method = "XmR")
#'
#' #######################################
#' #  Example 2: Xbar Check Violations   #
#' #######################################
#'
#'# Setup Some Data ------------------------------------------------------------
#'      QC_xBar.rBar <- do.call(rbind, lapply(1:3, function(X){
#'        set.seed(5555+X)                                   #Loop over 3 seeds
#'        data.frame(
#'          sub_group = rep(1:42),                           #Define Subgroups
#'          sub_class = letters[X],
#'          c(
#'           c(runif(n = 5, min = 2.0,3.2)),                 #Outlier Data
#'           sample(c(rnorm(30),5,-4), 32, replace = FALSE), #Normal Data
#'           c(runif(n = 5, min = -3.2, max = -2.0))         #Outlier Data
#'          )
#'       )
#'      }
#'    )
#' )
#'
#' colnames(QC_xBar.rBar) <- c("sub_group","sub_class", "value")
#' QC_Vs <- QC_Violations(data  = QC_xBar.rBar,
#'                        formula = value~sub_group,
#'                        method = "xBar.rBar")
QC_Violations <- function(data, value=NULL, grouping=NULL, formula=NULL, method=NULL, ...)
{
  #print(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "XmR"))
 # Get the Center Point Method ---------------------------------------------
 if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar")){
   sigma_est <- QC_Lines(data = data, value=value, grouping=grouping,
                         method = method, formula=formula, na.rm = T)
   CentralFUN <- mean
   #print("Mean Route")
   if(is.null(formula)){
      f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
   }else{f1 <- formula}

   agg <- stats::aggregate(f1, FUN = CentralFUN, data = data)
   df <- data.frame(data = agg[,ncol(agg)])
   CentralLimitCol <- 6


 }else if (method %in% c("xMedian.rBar", "xMedian.rMedian")){
   sigma_est <- QC_Lines(data = data, value=value, grouping=grouping,
                         method = method, formula=formula, na.rm = T)
   CentralFUN <- stats::median
   #print("Median Route")
   if(is.null(formula)){
      f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
   }else{f1 <- formula}

   agg <- stats::aggregate(f1, FUN = CentralFUN, data = data)
   df <- data.frame(data = agg[,ncol(agg)])
   CentralLimitCol <- 6


 }else if (method == "XmR"){
   if(!is.vector(data)){
     return(warning("data parameter must be a vector when using method='XmR'."))
   }else{
    df <- data.frame(data = data)
    sigma_est <- QC_Lines(data = data, method = method, na.rm = T)
    CentralLimitCol <- 2
   }
 }else{
   return(warning(paste("Unknown method: ", method,
                        "\n Please see help file or use the following methods:",
                        "\n XmR, xBar.rBar, xBar.rMedian, xBar.sBar, xBar.rBar,
                        xBar.rMedian, xBar.sBar")))
 }





  # Get Sigma ---------------------------------------------------------------


  df$z_score <- (df$data - sigma_est[1,CentralLimitCol])/sigma_est$sigma
  df$UorL<-ifelse(df$z_score < 0, -1, 1)

  # Check the Rules ------------------------------------------------------------------
  df$Rule_1_bin<- cut(df$z_score, breaks = eval(parse(text = RuleSet$breaks[1])))

  df$Violation_3_Sigma <- df$Rule_1_bin %in% c("(-Inf,-3]", "(3, Inf]")
  df$Violation_2_Sigma <- Find_Run_Length_Violations(2, Test_df = df)
  df$Violation_1_Sigma <- Find_Run_Length_Violations(3, Test_df = df)
  df$Violation_Same_Side <- Find_Run_Length_Violations(4, Test_df = df)
  #print(nrow(df))
  df -> .
    dplyr::select(., c("data", "z_score", "Violation_1_Sigma", "Violation_2_Sigma",
           "Violation_3_Sigma", "Violation_Same_Side")) -> .
    dplyr::mutate(., Index = 1:nrow(.)) -> .
    tidyr::gather(., "Violation_Result", "values", 3:6) -> .
    dplyr::mutate(., Violation_Result = gsub(pattern="_",
                                             replacement=" ",
                                            x=.$Violation_Result)) -> .
    dplyr::rename(., Violation = "values")  ->
  df

  return(df)

}





