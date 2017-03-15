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

chartoptions <- data.frame(    #List of constant based on chart need
    chart_options = c("mean_rBar",
                      "mean_rMedian",
                      "mean_sBar",
                      "stats::median_rBar",
                      "stats::median_rMedian"),
    Kname = c("d2", "d4", "c4", "b2", "b4"),
stringsAsFactors = FALSE)

#chartType(mean, rBar)
# N needed by Functions ---------------------------------------------------
NFUN <- function(data, value=NULL, grouping=NULL, formula=NULL, ...){
  if(is.null(formula)){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
  }else{f1 <- formula}

  N_df <- (stats::aggregate(f1, FUN = "length", data = data))
  N <- floor(mean(N_df[,ncol(N_df)]))
  N
}

# General Dispersion Function Factory -------------------------------------
  # Function factory to creat Rbar, Rmedian, Sbar functions
dispersionFUN <- function(subgroup_method, group_method,...){
  function(data, value, grouping, formula=NULL, ...){
    if(is.null(formula)){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    }else{f1 <- formula}

    agg <- stats::aggregate(f1, FUN = subgroup_method, data = data)
    group_method(agg[,ncol(agg)])
  }
}

# General X Central Limit Factory -----------------------------------------
  # Function Factory to create Xbar and Xmedian
XCentral_LimitFUN <- function(centralLimitFunction){
  function(data, value, grouping, formula=NULL, ...){
  #function(data, value, grouping, n = 2, natural = F, formula=NULL){
    if(is.null(formula)){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    }else{f1 <- formula}

    agg <- stats::aggregate(f1,
                     FUN = centralLimitFunction,
                     data = data)
    mean(agg[,ncol(agg)])
  }
}

# General XLimit Factory --------------------------------------------------
  # Function Factory to make limit functions
  # xMethod may be mean or median
  # dispersion method: rBar, rMedian, sBar
  # PM = string "+" or "-" for upper and lower limits
xLimitFun <- function(xMethod, dispersionMethod, PM){
  #Determine the Chart Constant Needed
  xMethod_txt <- deparse(substitute(xMethod))
  dispersionMethod_txt <- deparse(substitute(dispersionMethod))
  SelectedChartType <- paste0(xMethod_txt, "_", dispersionMethod_txt)
  CT <- chartoptions$Kname[chartoptions$chart_options == SelectedChartType]
  #print(CT)
  function(data, value, grouping, n = NULL, natural = F, formula=NULL){
    if(is.null(formula)){
      f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    }else{f1 <- formula}

    agg <- stats::aggregate(f1,
                     FUN = xMethod,
                     data = data)
    #If N is non integer use the smallest conservative value
    N_df <- (stats::aggregate(f1, FUN = "length", data = data))
    N <- floor(mean(N_df[,ncol(N_df)]))

    if(is.null(n)){n <- N} #Defaul calc limits off average subgroup size
    else if(natural == T){n <- 1} # If natural limits use n = 1
    else{n = n} # if user defined use n provided.

    if(N < 2){
      warning("For n = 1, use XmR methods")
      return(NA)
    }
    #Look up the constant value in the table qcK for n == ...
    #BCF <- 3/(qcK[qcK$n == N,CT]*sqrt(n)) #if n=1 (natural)
    BCF <- 3/(QC_constants(N)[1, CT]*sqrt(n)) #if n=1 (natural)
    Reduce(PM, right = T, #PM(+/-) report high or low limit
           c(mean(agg[,ncol(agg)]),
             dispersionMethod(data = data, value = value,
                              grouping=grouping, formula=formula)*BCF)
    )

  }
}


# dispersion-Limit Function Facotry (+/-) ------------------------------

DispersionLimitFun <- function(dispersionMethod, PM){
  #Determine the Chart Constant Needed
  SelectedChartType <- deparse(substitute(dispersionMethod))
  ChartTypes <- c("rBar", "rMedian", "sBar")
  lower_limit <- c("D3", "D5", "B3")
  upper_limit <- c("D4", "D6", "B4")
  dispersionDF <- data.frame(ChartTypes, lower_limit , upper_limit, stringsAsFactors = F )
  if(PM == "+"){
    dK <- dispersionDF$upper_limit[dispersionDF$ChartTypes == SelectedChartType]
    #print(dK)
  }else if (PM == "-"){
    dK <- dispersionDF$lower_limit[dispersionDF$ChartTypes == SelectedChartType]
    #print(dK)
  }

  function(data=data, value=value, grouping=grouping,formula=NULL, ...){
    #If N is non integer use the smallest conservative value
    if(is.null(formula)){
      f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    }else{f1 <- formula}

    N_df <- (stats::aggregate(f1, FUN = "length", data = data))
    N <- floor(mean(N_df[,ncol(N_df)]))

    #if(N > 20 || N < 2){
    if(N < 2){
      warning("For n = 1, use XmR methods")
      return(NA)
    }
    #BCF <- (qcK[qcK$n == N,dK])
    BCF <- QC_constants(N)[1, dK]
    dispersionMethod(data = data, value = value,grouping=grouping, formula=formula)*BCF
  }
}


