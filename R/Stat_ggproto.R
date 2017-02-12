
Stat_QC <- ggplot2::ggproto("Stat_QC", ggplot2::Stat,
        compute_group = function(data, scales, n=NULL, digits=1, method=NULL ){
          temp <- aggregate(data=data, y~x, mean)
          #print(temp)
          if(method %in% c("mR", "XmR", "c")){
            limits_df <- data.frame(yintercept =
                         c(t(ylines_indv(temp$y,
                         method = method))[c(1:3)])
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
                         c(t(ylines_indv(temp$y/n, n=n,
                         method = method))[c(1:3)])

            )
          }
          else if(method == "p"){
            if (is.null(data$n)){
               warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
               return(NULL)
            # # }else if(!binCheck_pChart(temp$y/n, n)){
            #   warning("**p-chart Error**\n Items of Opportunity 'n' < Item Nonconforming\n check value of 'n' in stat_QC.", call. = FALSE)
            #   return(NULL)
             }
            data$UCL <- pBar_UCL(data$y, data$n)
            data$LCL <- pBar_LCL(data$y, data$n)
            data$meanP <- pBar(data$y)
            print(data)
            limits_df <- data.frame(yintercept =
                                      c(t(ylines_indv(temp$y/n, n=n,
                                                      method = method))[c(1:3)])

            )
          }else{
            limits_df <- data.frame(yintercept =
            c(t(QC_Lines(data = data,
                         value = "y", grouping = "x",
                         n=n, method = method))[-c(1:4)])
                         )
          }
          #print(limits_df)
          limits_df$y = limits_df$yintercept
          limits_df$x = Inf
          limits_df$label = round(limits_df$yintercept,digits)
          #print(limits_df)
          limits_df

        }
        )


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
                       ...) {
  ggplot2::layer(
    stat = Stat_QC,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n,
                  digits=digits, method=method, ...)
  )
}

stat_QC_labels <- function(mapping = NULL,
                 data = NULL,
                 geom = "label",
                 yintercept = NULL,
                 position = "identity",
                 na.rm = FALSE,
                 show.legend = NA,
                 inherit.aes = TRUE,
                 n=NULL,digits=1, method="xBar.rBar",
                 ...) {
  ggplot2::layer(
    stat = Stat_QC,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=3,n=n,
                  digits=digits,method=method, ...)
  )
}


