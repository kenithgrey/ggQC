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

STAT_QC <- ggplot2::ggproto("STAT_QC", ggplot2::Stat,
  compute_group = function(data, scales, n=NULL, digits=1,
                           method=NULL, draw.line=draw.line,
                           physical.limits=c(NA,NA),
                           call.from = "QC.NULL",
                           show.1n2.sigma = FALSE, limit.txt.label=c("LCL","UCL")){
     temp <- aggregate(data=data, y~x, mean)
     # print("---NEW---")
     # print(call.from)
     # print(limit.txt.label)
     LCL <- limit.txt.label[1]
     UCL <- limit.txt.label[2]
     if(method %in% c("mR", "XmR", "c")){
       qcline <- if(draw.line == "center") c(2) else c(1,3)
       dflines <- ylines_indv(temp$y, n=n, method = method)
       sigma <- dflines$sigma
       center <- dflines[2]
       limits_txt_lbl <- limit.txt.label
       #print(limits_txt_lbl)
        if(!all(is.na(physical.limits)) & method %in% c("XmR", "c")){
          if(!any(is.na(physical.limits))){

            if(physical.limits[1] > dflines[1] & physical.limits[2] < dflines[3]){
              dflines[1] <- physical.limits[1]
              dflines[3] <- physical.limits[2]
              limits_txt_lbl <- c("LB", "UB")
            }else if(physical.limits[1] > dflines[1]){
              dflines[1] <- physical.limits[1]
              limits_txt_lbl <- c("LB", UCL)
            }else if(physical.limits[2] < dflines[3]){
              dflines[3] <- physical.limits[2]
              limits_txt_lbl <- c(LCL, "UB")
            }

          }else if(!is.na(physical.limits[1])){
             if(physical.limits[1] > dflines[1]){
             dflines[1] <- physical.limits[1]
             limits_txt_lbl <- c("LB", UCL)
             }
          }else if(!is.na(physical.limits[2])){
            if(physical.limits[2] < dflines[3]){
              dflines[3] <- physical.limits[2]
              limits_txt_lbl <- c(LCL, "UB")
            }
          }
       }
       #print(dflines)
       limits_df <- data.frame(yintercept = t(dflines)[qcline])
       #print(limits_df)


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
       sigma <- dflines$sigma
       center <- dflines[2]
       limits_txt_lbl <- limit.txt.label



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
       #Studentized charts Xbar-Rbar etc.
       qcline <- if(draw.line == "center") c(6) else c(5,7)
       dflines <- QC_Lines(data = data, value = "y", grouping = "x", n=n, method = method)
       sigma <- dflines$sigma
       center <- dflines[6]
       limits_txt_lbl <- limit.txt.label

       if(!all(is.na(physical.limits)) & !method %in% c("rBar", "rMedian", "sBar")){
         if(!any(is.na(physical.limits))){

           if(physical.limits[1] > dflines[5] & physical.limits[2] < dflines[7]){
             dflines[5] <- physical.limits[1]
             dflines[7] <- physical.limits[2]
             limits_txt_lbl <- c("LB", "UB")
           }else if(physical.limits[1] > dflines[5]){
             dflines[5] <- physical.limits[1]
             limits_txt_lbl <- c("LB", UCL)
           }else if(physical.limits[2] < dflines[7]){
             dflines[7] <- physical.limits[2]
             limits_txt_lbl <- c(LCL, "UB")
           }
         }else if(!is.na(physical.limits[1])){
           if(physical.limits[1] > dflines[5]){
             dflines[5] <- physical.limits[1]
             limits_txt_lbl <- c("LB", UCL)
           }
         }else if(!is.na(physical.limits[2])){
           if(physical.limits[2] < dflines[7]){
             dflines[7] <- physical.limits[2]
             limits_txt_lbl <- c(LCL, "UB")
           }
         }
       }
       limits_df <- data.frame(yintercept = t(dflines[,qcline]))
     }

    #print(qcline)

     limits_df$y <- limits_df$yintercept
     limits_df$x <- Inf
     limits_df$label <- paste0(round(limits_df$yintercept,digits))
     limits_df$hjust <- 1.1
     limits_df$alpha <- 1


     # Sigma Limits ------------------------------------------------------------
     if(grepl(pattern = "c|XmR|xBar|xMedian|np", method) &
        !call.from == "QC.Label" & show.1n2.sigma){
     sigma_df <- data.frame(
       yintercept = c(center[[1]] + 1*sigma, center[[1]] - 1*sigma,
                      center[[1]] + 2*sigma, center[[1]] - 2*sigma),
       y = rep(0, 4),
       x = rep(NA, 4),
       label = rep(NA, 4),
       hjust = rep(0, 4),
       alpha = c(.20,.20,.50,.50)
     )
     }else if(!grepl(pattern = "c|XmR|xBar|xMedian|np", method) &
              !call.from == "QC.Label" & show.1n2.sigma){
       warning(paste(method, " method: does not support drawing sigma lines"),call. = F)
       sigma_df <- data.frame(
         yintercept = integer(),
         y = integer(),
         x = integer(),
         label = integer(),
         hjust = integer(),
         alpha = integer()
       )
     }else{
       sigma_df <- data.frame(
         yintercept = integer(),
         y = integer(),
         x = integer(),
         label = integer(),
         hjust = integer(),
         alpha = integer()
         )
     }

     #sigma_df <- ifelse(call.from == "QC.Label", na.omit(sigma_df), sigma_df)

     #limits_df$sigma <- sigma
     #limits_df$center <- center[[1]]

     #limits_df$center <- center


     if(!any(qcline %in% c(2,6))){
       #print(limit.txt.label)
       if(!any(is.na(limit.txt.label))){
       limits_df_txt_lbl <- limits_df
       limits_df_txt_lbl$x <- -Inf
       limits_df_txt_lbl$label <- limits_txt_lbl
       limits_df_txt_lbl$hjust <- 0
       }else{
         limits_df_txt_lbl <- NULL
       }
       if(call.from == "QC.Label"){
         limits_df <- rbind(limits_df_txt_lbl, limits_df)
         # print(limits_txt_lbl)
         # print(limits_df)
       }else{
         if(show.1n2.sigma){
         limits_df <- rbind(limits_df_txt_lbl, limits_df, sigma_df)
         }else{
         limits_df <- rbind(limits_df_txt_lbl, limits_df)
         }
       }

       #print(limits_df)
      #print(limits_df)
       }

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
#'To label chart lines see \link[ggQC]{stat_QC_labels}
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param n number, for
#' \itemize{
#'   \item \bold{Studentized Charts}, used for custom or hypothetical subgroup size.
#'   \item \bold{np Charts}, used to specify a fixed area of opportunity.
#' }
#' @param method string, calling the following methods:
#' \itemize{
#'   \item \bold{Individuals Charts}: mR, XmR,
#'   \item \bold{Attribute Charts}: c, np, p, u,
#'   \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#'   \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @param color.qc_limits color, used to colorize the plot's upper and lower mR control limits.
#' @param physical.limits vector, specify lower physical boundary and upper physical boundary
#' @param color.qc_center color, used to colorize the plot's center line.
#' @param color.point color, used to colorize points in studentized plots. You will need geom_point() for C, P, U, NP, and XmR charts.
#' @param color.line color, used to colorize lines connecting points in studentized plots. You will need geom_line() for C, P, U, NP, and XmR charts.
#' @param auto.label boolean setting, if T labels graph with control limits.
#' @param label.digits integer, number of decimal places to display.
#' @param show.1n2.sigma boolean setting, if T labels graph 1 and 2 sigma lines. Line color is set by color.qc_limits
#' @param limit.txt.label vector, provides option for naming or not showing the limit text labels (e.g., UCL, LCL)
#' \itemize{
#' \item \bold{limit.txt.label = c("LCL", "UCL")}: default
#' \item \bold{limit.txt.label = c("Low", "High")}: changes the label text to low and high
#' \item \bold{limit.txt.label = NA}: does not show label text.
#' }
#' @return ggplot control charts.
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
#' EX1.1 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  geom_point() + geom_line() + stat_QC(method="XmR") +
#'  stat_QC_labels(method="XmR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.1
#'
#' EX1.2 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  stat_mR() + ylab("Moving Range") +
#'  stat_QC_labels(method="mR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.2
#'
#'#############################
#'#  Example 2:  XbarR Chart  #
#'#############################
# XbarR Plot and rBar Plot ------------------------------------------------
#'
#' EX2.1 <- ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("point")) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("line")) +
#'  stat_QC(method = "xBar.rBar") + facet_grid(.~processID, scales = "free_x")
#' #EX2.1
#'
#' EX2.2 <- ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "point") +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "line") +
#'  stat_QC(method = "rBar") +
#'  ylab("Range") +
#'  facet_grid(.~processID, scales = "free_x")
#'  #EX2.2
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
#' EX3.1 <- ggplot(data = bin_data, aes(x=trial,
#'                            y=Proportion_Incomplete,
#'                            n=Num_Items_in_Set)) +
#'  geom_point() + geom_line() +
#'  stat_QC(method = "p")
#'  #EX3.1
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
#' EX4.1 <- ggplot(data = bin_data, aes(x=trial,
#'                            y=Blemish_Rate,
#'                            n=Num_Items_Inspected)) +
#'  geom_point() + geom_line() +
#'  stat_QC(method = "u")
#' #EX4.1
#'
#'#############################
#'#  Example 5:  np Chart     #
#'#############################
#'# np chart Setup -----------------------------------------------------------
#'  set.seed(5555)
#'  bin_data <- data.frame(
#'    trial=1:30,
#'    NumNonConforming = rbinom(30, 30, prob = .50))
#'  Units_Tested_Per_Batch <- 60
#'
#'# Plot np chart ------------------------------------------------------------
#'  EX5.1 <- ggplot(data = bin_data, aes(trial, NumNonConforming)) +
#'   geom_point() +
#'   stat_QC(method = "np", n = Units_Tested_Per_Batch)
#' #EX5.1
#'
#'#############################
#'#  Example 6:  c Chart     #
#'#############################
#'# c chart Setup -----------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(Process_run_id = 1:30,
#'                         Counts=rpois(n = 30, lambda = 25),
#'                         Group = "A")
#'  Process2 <- data.frame(Process_run_id = 1:30,
#'                         Counts = rpois(n = 30, lambda = 5),
#'                         Group = "B")
#'
#'  all_processes <- rbind(Process1, Process2)
#'# Plot C Chart ------------------------------------------------------------
#'
#'  EX6.1 <- ggplot(all_processes, aes(x=Process_run_id, y = Counts)) +
#'    geom_point() + geom_line() +
#'    stat_QC(method = "c", auto.label = TRUE, label.digits = 2) +
#'    scale_x_continuous(expand =  expand_scale(mult = .25)) +
#'    facet_grid(.~Group)
#'# EX6.1



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
                    color.qc_limits = "red",
                    color.qc_center = "blue",
                    color.point="black",
                    color.line="black",
                    physical.limits=c(NA,NA),
                    auto.label = FALSE,
                    limit.txt.label=c("LCL","UCL"),
                    label.digits = 1,
                    show.1n2.sigma = FALSE,
                    #color.point="black",
                    #color.line="black",
                    ...) {

  if(method %in% c("p", "u")){
    Limits <- ggplot2::layer(
      stat = STAT_QC, data = data, mapping = mapping,
      geom = "step",
      position = position, show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n=n, digits=1, method=method,
                    color= color.qc_limits, direction="vh",
                    draw.line = "limit",
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

    #n_p_points <- ggplot2::layer(geom = "point", color=color.point)
    #n_p_line <- ggplot2::layer(geom="line", color=color.line)

  }else if(method %in% c("mR")){
    MR <- stat_mR(mapping = mapping,
                        data = data,
                        geom = "point",
                        #yintercept = NULL,
                        position = position,
                        show.legend = show.legend,
                        inherit.aes = inherit.aes,
                        na.rm = FALSE,
                        color.mr_point=color.point,
                        color.mr_line=color.line,
                        color.qc_limits = color.qc_limits,
                        color.qc_center = color.qc_center,
                        ...)



  }else{

  if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "xMedian.rBar", "xMedian.rMedian")){
    meanOmedian <- ifelse(grepl("xBar|XmR|c|np", method), "mean", "median")
    Summary_Stat_Point <-
      ggplot2::stat_summary(fun.y = meanOmedian, color = color.point, geom = c("point"))
    Summary_Stat_Line <-
      ggplot2::stat_summary(fun.y = meanOmedian, color = color.line, geom = c("line"))
  }

  if(method %in% c("np", "c","XmR")){
    #do nothing
  }

  if(method %in% c("rBar", "rMedian", "sBar")){
      rangeOsd <- ifelse(grepl("rBar|rMedian", method), "QCrange", "sd")
      Summary_Stat_Point <-
        ggplot2::stat_summary(fun.y = rangeOsd, color = color.point, geom = c("point"))
      Summary_Stat_Line <-
        ggplot2::stat_summary(fun.y = rangeOsd, color = color.line, geom = c("line"))
  }

  Limits <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n, digits=1, method=method,
                  color= color.qc_limits, draw.line = "limit",
                  physical.limits=physical.limits,
                  show.1n2.sigma=show.1n2.sigma,
                  #limit.txt.label=limit.txt.label,
                  ...)
  )

  Centerline <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n,
                  digits=1, method=method,
                  color=color.qc_center, draw.line = "center",
                  physical.limits=c(NA,NA),
                  limit.txt.label=limit.txt.label,
                  ...)
  )
  }


  #return(list(Limits, Centerline))
  if(!method %in% c("p", "u")){
  QC_Labels <- stat_QC_labels(mapping = mapping,
                              data = data,
                              geom = "label",
                              #yintercept = NULL,
                              position = position,
                              na.rm = na.rm,
                              show.legend = show.legend,
                              inherit.aes = TRUE,
                              n=n, digits=label.digits,
                              method=method,
                              color.qc_limits = color.qc_limits,
                              color.qc_center = color.qc_center,
                              text.size=3,
                              physical.limits=physical.limits,
                              limit.txt.label = limit.txt.label,
                              ...)
  }



  if(auto.label){
    if(method %in% c("p", "u")){
      return(list(Limits, Centerline))
    }
    if(method == "mR"){
      return(list(MR, QC_Labels))
      }
    if(method %in% c("np", "c","XmR")){
      return(list(Limits, Centerline, QC_Labels))
    }
    return(list(Limits, Centerline, Summary_Stat_Line,
                Summary_Stat_Point, QC_Labels))

  }else{
    if(method == "mR"){
      return(list(MR))
    }
    if(method %in% c("p", "u", "np", "c","XmR")){
      return(list(Limits, Centerline))
    }
      return(list(Limits, Centerline,
                  Summary_Stat_Line, Summary_Stat_Point
                  ))
    }
}



#' @export
#' @title Write QC Line Labels to ggplot QC Charts.
#' @description Write QC line labels to ggplot QC Charts. Useful if you want
#' to see the value of the center line and QC limits. see method argument
#' for methods supported.
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param n number, for
#' \itemize{
#'   \item \bold{Studentized Charts}, used for custom or hypothetical subgroup size.
#'   \item \bold{np Charts}, used to specify a fixed area of opportunity.
#' }
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
#' @param physical.limits vector, specify lower physical boundary and upper physical boundary
#' @param color.qc_center color, used to colorize the plot's center line.
#' @param limit.txt.label vector, provides option for naming or not showing the limit text labels (e.g., UCL, LCL)
#' \itemize{
#' \item \bold{limit.txt.label = c("LCL", "UCL")}: default
#' \item \bold{limit.txt.label = c("Low", "High")}: changes the label text to low and high
#' \item \bold{limit.txt.label = NA}: does not show label text.
#' }
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
#' EX1.1 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  geom_point() + geom_line() + stat_QC(method="XmR") +
#'  stat_QC_labels(method="XmR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.1
#'
#' EX1.2 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  stat_mR() + ylab("Moving Range") +
#'  stat_QC_labels(method="mR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.2
#'
#'#############################
#'#  Example 2:  XbarR Chart  #
#'#############################
#'# Facet Plot - Studentized Process ----------------------------------------
#'
#' EX2.1 <- ggplot(Both_Processes, aes(x=subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  geom_point(alpha=.2) +
#'  stat_summary(fun.y = "mean", color="blue", geom=c("point")) +
#'  stat_summary(fun.y = "mean", color="blue", geom=c("line")) +
#'  stat_QC() + facet_grid(.~processID, scales = "free_x") +
#'  stat_QC_labels(text.size =3, label.size=.1)
#' #EX2.1
#'
#' EX2.2 <- ggplot(Both_Processes, aes(x=subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "QCrange", color="blue", geom = "point") +
#'  stat_summary(fun.y = "QCrange", color="blue", geom = "line") +
#'  stat_QC(method="rBar") +
#'  stat_QC_labels(digits=2, method="rBar") +
#'  ylab("Range") +
#'  facet_grid(.~processID, scales = "free_x")
#'  #EX2.2

stat_QC_labels <- function(mapping = NULL,
                           data = NULL,
                           geom = "label",
                           #yintercept = NULL,
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           n=NULL,digits=1, method="xBar.rBar",
                           color.qc_limits = "red",
                           color.qc_center = "black",
                           text.size=3,
                           physical.limits=c(NA,NA), limit.txt.label=c("LCL","UCL"),

                           ...) {
  Center <- ggplot2::layer(
    stat = STAT_QC,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method,
                  color="black",
                  draw.line = "center",
                  call.from = "QC.Label",
                  limit.txt.label=limit.txt.label, ...)
  )

  Limits <- ggplot2::layer(
    stat = STAT_QC,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method,
                  color="black",
                  draw.line = "limit",
                  physical.limits=physical.limits,
                  call.from = "QC.Label",
                  limit.txt.label=limit.txt.label,
                  ...)
  )
if(method %in% c("p", "u")){
  return(warning("Stat_QC_Label does not support u or p charts"))
}else{return(list(Center, Limits))}
}


