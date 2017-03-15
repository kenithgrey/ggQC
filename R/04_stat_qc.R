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

STAT_QC <- ggplot2::ggproto("STAT_QC", ggplot2::Stat,
  compute_group = function(data, scales, n=NULL, digits=1, method=NULL, draw.line=draw.line){
     temp <- aggregate(data=data, y~x, mean)
     #print(temp)

     if(method %in% c("mR", "XmR", "c")){
       qcline <- if(draw.line == "center") c(2) else c(1,3)
       dflines <- ylines_indv(temp$y, n=n, method = method)
       limits_df <- data.frame(yintercept = t(dflines)[qcline])

     }else if(method == "np"){
       if (is.null(n)){
         warning("**np-chart Error**\n For np chart, specify value for n in stat_QC", call. = FALSE)

         return(NULL)

       }else if(!binCheck_pChart(temp$y/n, n)){
         warning("**np-chart Error**\n Items of Opportunity 'n' < Item Nonconforming\n check value of 'n' in stat_QC.", call. = FALSE)

        return(NULL)
       }
       #print(temp$y/n)
       qcline <- if(draw.line == "center") c(2) else c(1,3)
       dflines <- ylines_indv(temp$y, n=n, method = method)
       limits_df <- data.frame(yintercept = t(dflines)[qcline])


     }else if(method == "p"){
       if (is.null(data$n)){
         warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
         return(NULL)
         }
         qcline <- if(draw.line == "center") c(2,4) else c(1,3,4)
         pdata <- QC_Lines(data = data$y, n=data$n, method = "p")
         pdata$x <- data$x
         pchart_data <- reshape2::melt(pdata[,qcline], id.vars=c("x"))
         colnames(pchart_data) <- c("x", "group", "y")

         return(pchart_data)

     }else if(method == "u"){
       if (is.null(data$n)){
         warning("**u-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)

         return(NULL)
       }
       qcline <- if(draw.line == "center") c(2,4) else c(1,3,4)
       udata <- QC_Lines(data = data$y, n=data$n, method = "u")
       udata$x <- data$x
       uchart_data <- reshape2::melt(udata[,qcline], id.vars=c("x"))
       colnames(uchart_data) <- c("x", "group", "y")

       return(uchart_data)

     }else{
       qcline <- if(draw.line == "center") c(6) else c(5,7)
       dflines <- QC_Lines(data = data, value = "y", grouping = "x", n=n, method = method)
       limits_df <- data.frame(yintercept = t(dflines[,qcline]))

     }
     #print(limits_df)
     limits_df$y = limits_df$yintercept
     limits_df$x = Inf
     limits_df$label = round(limits_df$yintercept,digits)
     #print(limits_df)
     #print(data$group)
     limits_df

   }
)


#' @export
#' @title Produce QC Charts with ggplot Framework.
#' @description Produce QC charts with ggplot framework. Support for faceting and
#' layering of multiple QC chart lines on a single plot. Charts supported (see method argument for call):
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian,
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#'To label chart lines see \link[rQC]{stat_QC_labels}
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param n number, for \bold{Studentized Charts}, used for custom or hypothetical subgroup size.
#' @param method string, calling the following methods:
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @param color.qc_limits color, used to colorize the plot's upper and lower mR control limits.
#' @param color.qc_center color, used to colorize the plot's center line.
#' @return data need to produce the mR plot in ggplot.
#' @examples
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(processID = as.factor(rep(1,100)),
#'                         metric_value = rnorm(100,0,1),
#'                         subgroup_sample = rep(1:20, each=5),
#'                         Process_run_id = 1:100)
#'  set.seed(5556)
#'  Process2 <- data.frame(processID = as.factor(rep(2,100)),
#'                         metric_value = rnorm(100,5, 1),
#'                         subgroup_sample = rep(1:10, each=10),
#'                         Process_run_id = 101:200)
#'
#'  Both_Processes <- rbind(Process1, Process2)
#'
#'#############################
#'#  Example 1:  XmR Chart    #
#'#############################
#'
#'
#'ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  geom_point() + geom_line() + stat_QC(method="XmR") +
#'  stat_QC_labels(method="XmR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#'
#'ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  stat_mR() + ylab("Moving Range") +
#'  stat_QC_labels(method="mR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#'
#'
#'#############################
#'#  Example 2:  XbarR Chart  #
#'#############################
# XbarR Plot and rBar Plot ------------------------------------------------
#'
#'ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("point")) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("line")) +
#'  stat_QC(method = "xBar.rBar") + facet_grid(.~processID, scales = "free_x")
#'
#'ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "point") +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "line") +
#'  stat_QC(method = "rBar") +
#'  ylab("Range") +
#'  facet_grid(.~processID, scales = "free_x")
#'
#'
#'#############################
#'#  Example 3:  p Chart      #
#'#############################
#'# p chart Setup -----------------------------------------------------------
#'  set.seed(5556)
#'  bin_data <- data.frame(
#'    trial=1:30,
#'    Num_Incomplete_Items = rpois(30, lambda = 30),
#'    Num_Items_in_Set = runif(n = 30, min = 50, max = 100))
#'    bin_data$Proportion_Incomplete <- bin_data$Num_Incomplete_Items/bin_data$Num_Items_in_Set
#'
#'# Plot p chart ------------------------------------------------------------
#'ggplot(data = bin_data, aes(x=trial,
#'                            y=Proportion_Incomplete,
#'                            n=Num_Items_in_Set)) +
#'  geom_point() + geom_line() +
#'  stat_QC(method = "p")
#'
#'
#'#############################
#'#  Example 4:  u Chart      #
#'#############################
#'# u chart Setup -----------------------------------------------------------
#'  set.seed(5555)
#'  bin_data <- data.frame(
#'    trial=1:30,
#'    Num_of_Blemishes = rpois(30, lambda = 30),
#'    Num_Items_Inspected = runif(n = 30, min = 50, max = 100)
#'    )
#'    bin_data$Blemish_Rate <- bin_data$Num_of_Blemishes/bin_data$Num_Items_Inspected
#'
#'# Plot u chart ------------------------------------------------------------
#'ggplot(data = bin_data, aes(x=trial,
#'                            y=Blemish_Rate,
#'                            n=Num_Items_Inspected)) +
#'  geom_point() + geom_line() +
#'  stat_QC(method = "u")
#'
#'


stat_QC <- function(mapping = NULL,
                    data = NULL,
                    geom = "hline",
                    #yintercept = NULL,
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    n=NULL,
                    #digits=1,
                    method="xBar.rBar",
                    color.qc_limits = "red", color.qc_center = "green", ...) {

  if(method %in% c("p", "u")){
    Limits <- ggplot2::layer(
      stat = STAT_QC, data = data, mapping = mapping,
      geom = "step",
      position = position, show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n=n, digits=1, method=method,
                    color= color.qc_limits, direction="vh", draw.line = "limit",
                    ...)
    )

    Centerline <- ggplot2::layer(
      stat = STAT_QC, data = data, mapping = mapping,
      geom = "step",
      position = position, show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n=n,
                    digits=1, method=method,
                    color=color.qc_center, draw.line = "center",
                    ...)
    )
  }else{

  Limits <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n, digits=1, method=method,
                  color= color.qc_limits, draw.line = "limit", ...)
  )

  Centerline <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n,
                  digits=1, method=method,
                  color=color.qc_center, draw.line = "center", ...)
  )
  }

  return(list(Limits, Centerline))
}



#' @export
#' @title Write QC Line Lables to ggplot QC Charts.
#' @description Write QC line lables to ggplot QC Charts. Useful if you want
#' to see the value of the center line and QC limits. see method argument
#' for methods supported.
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param n number, for \bold{Studentized Charts}, used for custom or hypothetical subgroup size.
#' @param digits integer, indicating the number of decimal places
#' @param text.size number, size of the text label
#' @param method string, calling the following methods:
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @param color.qc_limits color, used to colorize the plot's upper and lower mR control limits.
#' @param color.qc_center color, used to colorize the plot's center line.
#' @return data need to produce the mR plot in ggplot.
#' @examples
#' #########################
#' #  Example 1: mR Chart  #
#' #########################
#'
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(processID = as.factor(rep(1,100)),
#'                         metric_value = rnorm(100,0,1),
#'                         subgroup_sample=rep(1:20, each=5),
#'                         Process_run_id = 1:100)
#'  set.seed(5556)
#'  Process2 <- data.frame(processID = as.factor(rep(2,100)),
#'                         metric_value = rnorm(100,5, 1),
#'                         subgroup_sample=rep(1:10, each=10),
#'                         Process_run_id = 101:200)
#'
#'  Both_Processes <- rbind(Process1, Process2)
#'
#'# Facet Plot - Both Processes ---------------------------------------------
#'ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  geom_point() + geom_line() + stat_QC(method="XmR") +
#'  stat_QC_labels(method="XmR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#'
#'ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  stat_mR() + ylab("Moving Range") +
#'  stat_QC_labels(method="mR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#'
#'
#'#############################
#'#  Example 2:  XbarR Chart  #
#'#############################
#'# Facet Plot - Studentized Process ----------------------------------------
#'
#'ggplot(Both_Processes, aes(x=subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  geom_point(alpha=.2) +
#'  stat_summary(fun.y = "mean", color="blue", geom=c("point")) +
#'  stat_summary(fun.y = "mean", color="blue", geom=c("line")) +
#'  stat_QC() + facet_grid(.~processID, scales = "free_x") +
#'  stat_QC_labels(text.size =3, label.size=.1)
#'
#'ggplot(Both_Processes, aes(x=subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "QCrange", color="blue", geom = "point") +
#'  stat_summary(fun.y = "QCrange", color="blue", geom = "line") +
#'  stat_QC(method="rBar") +
#'  stat_QC_labels(digits=2, method="rBar") +
#'  ylab("Range") +
#'  facet_grid(.~processID, scales = "free_x")

stat_QC_labels <- function(mapping = NULL,
                           data = NULL,
                           geom = "label",
                           #yintercept = NULL,
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           n=NULL,digits=1, method="xBar.rBar",
                           color.qc_limits = "red", color.qc_center = "black",
                           text.size=3,
                           ...) {
  Center <- ggplot2::layer(
    stat = STAT_QC,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method, color=color.qc_center,
                  draw.line = "center", ...)
  )

  Limits <- ggplot2::layer(
    stat = STAT_QC,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method, color=color.qc_limits,
                  draw.line = "limit",...)
  )
return(list(Center, Limits))
}


