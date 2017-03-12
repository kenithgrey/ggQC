
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
stat_QC_labels <- function(mapping = NULL,
                           data = NULL,
                           geom = "label",
                           yintercept = NULL,
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           n=NULL,digits=1, method="xBar.rBar",
                           color.qc_limits = "red", color.qc_center = "black",
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
                  vjust=.5, size=3,n=n,
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
                  vjust=.5, size=3,n=n,
                  digits=digits,method=method, color=color.qc_limits, ...)
  )
return(list(Center, Limits))
}


