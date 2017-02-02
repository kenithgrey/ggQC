# QC Constants ------------------------------------------------------------
qcK <- read.csv(file = "Stat/Constants_Table.csv", header=T)
qcK$b2 <- qcK$A6 * sqrt(qcK$n) # used for median(x) Rbar
qcK$b4 <- qcK$A9 * sqrt(qcK$n) # used for median(x) RMedian



chartoptions <- data.frame(    #List of constant based on chart need
    chart_options = c("mean_rBar",
                      "mean_rMedian",
                      "mean_sBar",
                      "median_rBar",
                      "median_rMedian"),
    Kname = c("d2", "d4", "c4", "b2", "b4"),
    stringsAsFactors = F)

#chartType(mean, rBar)

# General Dispersion Function Factory -------------------------------------
  # Function factory to creat Rbar, Rmedian, Sbar functions
dispersionFUN <- function(subgroup_method, group_method,...){
  function(data, value, grouping, ...){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    agg <- aggregate(f1, FUN = subgroup_method, data = data)
    group_method(agg[,2])
  }
}

# General X Central Limit Factory -----------------------------------------
  # Function Factory to create Xbar and Xmedian
XCentral_LimitFUN <- function(centralLimitFunction){
  function(data, value, grouping, n = 2, natural = F){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    agg <- aggregate(f1,
                     FUN = centralLimitFunction,
                     data = data)
    mean(agg[,2])
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

  function(data, value, grouping, n = NULL, natural = F){
    f1 <- formula(eval(parse(text = paste0(value, "~", grouping))))
    agg <- aggregate(f1,
                     FUN = xMethod,
                     data = data)
    #If N is non integer use the smallest conservative value
    N <- floor(mean(aggregate(f1, FUN = "length", data = data)[,2]))

    if(is.null(n)){n <- N} #Defaul calc limits off average subgroup size
    else if(natural == T){n <- 1} # If natural limits use n = 1
    else{n = n} # if user defined use n provided.

    if(n > 20 || n < 1){
      warning("Control Chart Constants only available for 1 < n < 20")
      return(NA)
    }
    #Look up the constant value in the table qcK for n == ...
    BCF <- 3/(qcK[qcK$n == N,CT]*sqrt(n)) #if n=1 (natural)
    Reduce(PM, right = T, #PM(+/-) report high or low limit
           c(mean(agg[,2]),
             dispersionMethod(data = data, value = value,
                              grouping=grouping)*BCF)
    )

  }
}


