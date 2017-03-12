
Stat_QC_LIMITS <- ggplot2::ggproto("Stat_QC_LIMITS", ggplot2::Stat,
  compute_group = function(data, scales, n=NULL, digits=1, method=NULL){
    temp <- aggregate(data=data, y~x, mean)
    #print(temp)
    if(method %in% c("mR", "XmR", "c")){
      limits_df <- data.frame(yintercept = c(t(ylines_indv(temp$y, method = method))[c(1,3)])
      )
    }else if(method == "np"){
      if (is.null(n)){
        warning("**np-chart Error**\n For np chart, specify value for n in stat_QC", call. = FALSE)
        return(NULL)
      }else if(!binCheck_pChart(temp$y/n, n)){
        warning("**np-chart Error**\n Items of Opportunity 'n' < Item Nonconforming\n check value of 'n' in stat_QC.", call. = FALSE)
        return(NULL)
      }
      #print(temp$y/n)
      limits_df <- data.frame(yintercept = c(t(ylines_indv(temp$y, n=n, method = method))[c(1,3)])

      )
    }else if(method == "p"){
      if (is.null(data$n)){
        warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
        return(NULL)
      }
      pdata <- QC_Lines(data = data$y, n=data$n, method = "p")
      pdata$x <- data$x
      pchart_data <- reshape2::melt(pdata[,c(1,3,4)], id.vars=c("x"))
      colnames(pchart_data) <- c("x", "group", "y")
      pchart_data$x <- pchart_data$x + 0.5
      return(pchart_data)

    }else if(method == "u"){
      if (is.null(data$n)){
        warning("**u-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
        return(NULL)
      }
      udata <- QC_Lines(data = data$y, n=data$n, method = "u")
      udata$x <- data$x
      uchart_data <- reshape2::melt(udata[,c(1,3,4)], id.vars=c("x"))
      colnames(uchart_data) <- c("x", "group", "y")
      uchart_data$x <- uchart_data$x + 0.5
      return(uchart_data)

    }else{
      limits_df <- data.frame(yintercept = c(t(QC_Lines(data = data, value = "y", grouping = "x",
                                                        n=n, method = method))[c(5,7)])
      )
      #print(limits_df)
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

Stat_QC_CL <- ggplot2::ggproto("Stat_QC_CL", ggplot2::Stat,
                                compute_group = function(data, scales, n=NULL, digits=1, method=NULL){
                                     temp <- aggregate(data=data, y~x, mean)
                                     #print(temp)
                                     if(method %in% c("mR", "XmR", "c")){
                                       limits_df <- data.frame(yintercept =
                                                                 c(t(ylines_indv(temp$y,
                                                                                 method = method))[c(2)])
                                       )
                                     }else if(method == "np"){
                                       if (is.null(n)){
                                         warning("**np-chart Error**\n For np chart, specify value for n in stat_QC", call. = FALSE)
                                         return(NULL)
                                       }else if(!binCheck_pChart(temp$y/n, n)){
                                         warning("**np-chart Error**\n Items of Opportunity 'n' < Item Nonconforming\n check value of 'n' in stat_QC.", call. = FALSE)
                                         return(NULL)
                                       }
                                       #print(temp$y/n)
                                       limits_df <- data.frame(yintercept =
                                                                 c(t(ylines_indv(temp$y, n=n,
                                                                                 method = method))[c(2)])

                                       )
                                     }
                                     else if(method == "p"){
                                           if (is.null(data$n)){
                                           warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
                                           return(NULL)
                                           }
                                         pdata <- QC_Lines(data = data$y, n=data$n, method = "p")
                                         pdata$x <- data$x
                                         pchart_data <- reshape2::melt(pdata[,c(2,4)], id.vars=c("x"))
                                         colnames(pchart_data) <- c("x", "group", "y")

                                         return(pchart_data)

                                     }else if(method == "u"){
                                       if (is.null(data$n)){
                                         warning("**u-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
                                         return(NULL)
                                       }
                                       udata <- QC_Lines(data = data$y, n=data$n, method = "u")
                                       udata$x <- data$x
                                       uchart_data <- reshape2::melt(udata[,c(2,4)], id.vars=c("x"))
                                       colnames(uchart_data) <- c("x", "group", "y")

                                       return(uchart_data)

                                     }else{
                                       limits_df <- data.frame(yintercept =
                                                                 c(t(QC_Lines(data = data,
                                                                              value = "y", grouping = "x",
                                                                              n=n, method = method))[c(6)])#[-c(1:4)])
                                       )
                                       #print(limits_df)
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
stat_QC <- function(mapping = NULL,
                    data = NULL,
                    geom = "hline",
                    yintercept = NULL,
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    n=NULL, digits=1,
                    method="xBar.rBar",
                    color.qc_limits = "red", color.qc_center = "green", ...) {

  if(method %in% c("p", "u")){
    Limits <- ggplot2::layer(
      stat = Stat_QC_LIMITS, data = data, mapping = mapping,
      geom = "step",
      position = position, show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n=n, digits=digits, method=method,
                    color= color.qc_limits, direction="vh", ...)
    )

    Centerline <- ggplot2::layer(
      stat = Stat_QC_CL, data = data, mapping = mapping,
      geom = "step",
      position = position, show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n=n,
                    digits=digits, method=method,
                    color=color.qc_center, ...)
    )
  }else{

  Limits <- ggplot2::layer(
    stat = Stat_QC_LIMITS, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n, digits=digits, method=method,
                  color= color.qc_limits, ...)
  )

  Centerline <- ggplot2::layer(
    stat = Stat_QC_CL, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n,
                  digits=digits, method=method,
                  color=color.qc_center, ...)
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
#' color.mr_point color, to be used for the mR points.
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
#'  require(rQC)
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
    stat = Stat_QC_CL,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method, color=color.qc_center, ...)
  )

  Limits <- ggplot2::layer(
    stat = Stat_QC_LIMITS,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=text.size,n=n,
                  digits=digits,method=method, color=color.qc_limits, ...)
  )
return(list(Center, Limits))
}


