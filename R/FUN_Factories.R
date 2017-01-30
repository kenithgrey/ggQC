# QC Constants ------------------------------------------------------------
qcK <- read.csv(file = "Stat/Constants_Table.csv", header=T)
qcK$b2 <- qcK$A6 * sqrt(qcK$n)
qcK$b4 <- qcK$A9 * sqrt(qcK$n)



chartoptions <- data.frame(
    chart_options = c("mean_rBar",
                      "mean_rMedian",
                      "mean_sBar",
                      "median_rBar",
                      "median_rMedian"),
    Kname = c("d2", "d4", "c4", "b2", "b4"),
    stringsAsFactors = F)

#chartType(mean, rBar)

# General Dispersion Function Factory -------------------------------------
dispersionFUN <- function(subgroup_method, group_method,...){
  function(data, value, grouping, ...){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    agg <- aggregate(f1, FUN=subgroup_method,data = data)
    group_method(agg[,2])
  }
}

# General X Central Limit Factory -----------------------------------------
XCentral_LimitFUN <- function(centralLimitFunction){
  function(data, value, grouping, n=2, natural = F){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    agg <- aggregate(f1,
                     FUN=centralLimitFunction,
                     data = data)
    mean(agg[,2])
  }
}

# General XLimit Factory --------------------------------------------------
xLimitFun <- function(xMethod, dispersionMethod, PM){
  xMethod_txt <- deparse(substitute(xMethod))
  dispersionMethod_txt <- deparse(substitute(dispersionMethod))
  SelectedChartType <- paste0(xMethod_txt, "_", dispersionMethod_txt)
  CT <- chartoptions$Kname[chartoptions$chart_options == SelectedChartType]
  function(data, value, grouping, n=NULL, natural = F){
    f1 <- formula(eval(parse(text=paste0(value, "~", grouping))))
    agg <- aggregate(f1,
                     FUN=xMethod,
                     data = data)
    N <- floor(mean(aggregate(f1, FUN="length", data = data)[,2]))

    if(is.null(n)){n <- N}
    else if(natural == T){n <- 1}
    else{n=n}

    BCF <- 3/(qcK[qcK$n == N,CT]*sqrt(n)) #if n=1 than == E2
    Reduce(PM, right = T,
           c(mean(agg[,2]),
             dispersionMethod(data=data, value=value,
                              grouping=grouping)*BCF)
    )

  }
}


