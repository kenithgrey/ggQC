##############################
# Copyright 2018 Kenith Grey #
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

#' @export
#' @title Calculate Summary of Quality Performance Parameters
#' @description function to report listing of quality performance parameters
#' \itemize{
#' \item \bold{Proc. Tolerance (sigma)}: Describes the number of your process sigma (from QC charting) that can fit in your customer's specification window (the larger the better).
#' \item \bold{DNS (sigma)}: Distance to Nearest Specification (DNS) limit. Measure of how centered your process is and how close you are to the nearest process limit in sigma units.
#' \item \bold{Cp}: Describes how many times your 6 sigma process window (from QC charting) can fit in your customer's specification window (the larger the better)}
#' \item \bold{Cpk}: Describes how centered your process is relative to customer specifications. How many times can you fit a 3 sigma window (from QC charting) between your process center and the nearest customer specification limit.}
#' \item \bold{Pp}: Describes how many times your 6 sigma process window (overall standard deviation) can fit in your customer's specification window (the larger the better)}}
#' \item \bold{Ppk}: Describes how centered your process is relative to customer specifications. How many times can you fit a 3 sigma window (overall standard deviation) between your process center and the nearest customer specification limit.}
#' @param LSL number, customer's lower specification limit.
#' @param USL number, customer's upper specification limit.
#' @param QC.Center number, the mean or median value determined from an XmR plot or a Studentized (e.g., xBar) analysis.
#' @param s.Sigma number, the sigma value determined from overall standard deviation (i.e., sd()).
#' @param digits integer, how many digits to report.
#' @return data frame , listing of metric lebel and value
capability.summary <-
  function(LSL, USL, QC.Center, QC.Sigma, s.Sigma, digits=2){
    capability_df <- data.frame(
      label = c("Proc. Tolerance (sigma)", "DNS (sigma)",
                 #"Upper Distance (Sigma)", "Lower Distance (Sigma)",
                 "Cp", "Cpk", "Pp", "Ppk"),
      values = c(process_tolerance(LSL,USL,QC.Sigma),
                 DNS(LSL, USL, QC.Center, QC.Sigma),
                 #UD(LSL, USL, QC.Center, QC.Sigma),
                 #LD(LSL, USL, QC.Center, QC.Sigma),
                 Cp(LSL, USL, QC.Sigma),
                 Cpk(LSL, USL, QC.Center, QC.Sigma),
                 Pp(LSL, USL, s.Sigma),
                 Ppk(LSL, USL, QC.Center, s.Sigma)
      )
    )
    format.data.frame(capability_df, digits=digits)
  }


#' @export
#' @title Calculate Summary of Quality Performance Parameters
#' @description function to report listing of quality performance parameters
#' \itemize{
#' \item \bold{Proc. Tolerance (sigma)}: Describes the number of your process sigma (from QC charting) that can fit in your customer's specification window (the larger the better).
#' \item \bold{DNS (sigma)}: Distance to Nearest Specification (DNS) limit. Measure of how centered your process is and how close you are to the nearest process limit in sigma units.
#' \item \bold{Cp}: Describes how many times your 6 sigma process window (from QC charting) can fit in your customer's specification window (the larger the better)}
#' \item \bold{Cpk}: Describes how centered your process is relative to customer specifications. How many times can you fit a 3 sigma window (from QC charting) between your process center and the nearest customer specification limit.}
#' \item \bold{Pp}: Describes how many times your 6 sigma process window (overall standard deviation) can fit in your customer's specification window (the larger the better)}}
#' \item \bold{Ppk}: Describes how centered your process is relative to customer specifications. How many times can you fit a 3 sigma window (overall standard deviation) between your process center and the nearest customer specification limit.}
#' }
#' @param data vector or dataframe, as indicated below for each chart type
#' \itemize{
#' \item \bold{Individuals (XmR)}: vector of values;
#' \item \bold{Studentized}: dataframe
#' }
#' @param value string, \bold{Studentized Charts}, name of numeric vector in dataframe with values of interest.
#' @param grouping string, \bold{Studentized Charts}, name of single factor/variable to split
#' the dataframe "values" by
#' @param formula \bold{Studentized Charts}: a formula,
#' such as y ~ x1 + x2, where the y variable is
#' numeric data to be split into groups according to the grouping x
#' factors/variables
#' @param method string, calling one of the following methods:
#' \itemize{
#' \item \bold{Individuals Charts}: XmR,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar
#' }
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param digits integer, how many digits to report.
#' @return data frame , listing of metric lebel and value
QC_Capability <-
  function(data=NULL, value=NULL, grouping=NULL,
           formula=NULL, method="xBar.rBar",
           na.rm = FALSE, LSL=NULL, USL=NULL, digits=2){
    if(any(is.null(c(LSL, USL)))){
      return(stop("Error: Lower Specification Limit (LSL) and Upper Specification Limit (USL) must be defined."))
    }

    if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar",
                     "xMedian.rBar", "xMedian.rMedian", "XmR")){
      if(method == "XmR"){
        result <- QC_Lines(data=data, method=method, na.rm = na.rm)
      }else{
        result <- QC_Lines(data=data, value=value, grouping=grouping,
                           formula=formula, n=1, method=method,
                           na.rm = na.rm)
      }

      QC.Center <- ifelse(method == "XmR", result[[2]], result[[6]])
      QC.Sigma <- result$sigma
      #s.Sigma <- 5
      s.Sigma <- ifelse(method=="XmR", sd(data),
                        eval(parse(text = paste0("sd(data$",value, ")")))
      )
    }else{
      warning(paste0("QC Method", method, ": Doesn't currently support the QC capability function"))
    }

    #print(list(QC.Center=QC.Center,QC.Sigma=QC.Sigma,s.Sigma=s.Sigma))
    capability.summary(LSL = LSL, USL = USL,
                       QC.Center = QC.Center,
                       QC.Sigma = QC.Sigma,
                       s.Sigma = s.Sigma, digits = digits)
  }
