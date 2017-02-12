Stat_PCHART <- ggplot2::ggproto("Stat_PCHART", ggplot2::Stat,
                            compute_group = function(data, scales){
                              #suppressWarnings()
                              if (is.null(data$n)){
                                warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
                                return(NULL)
                                # # }else if(!binCheck_pChart(temp$y/n, n)){
                                #   warning("**p-chart Error**\n Items of Opportunity 'n' < Item Nonconforming\n check value of 'n' in stat_QC.", call. = FALSE)
                                #   return(NULL)
                              }
                              data$UCL <- pBar(data$y) + pBar_UCL(data$y, data$n)
                              data$LCL <- pBar(data$y) + pBar_LCL(data$y, data$n)
                              data$mean <- pBar(data$y)
                              data2 <- reshape2::melt(data = data[,c("x","mean","UCL","LCL")],
                                                                  id.vars = c("x"))
                              colnames(data2) <- c("x", "group", "y")
                              data2
                              #print(data2)



                              #mRs3<- mR_points_gg(data = data, value = "y", grouping = "x")
                              #mRs <- data.frame(y=mRs3, x=data$x)
                              #mRs

                            }

)


stat_pchart <- function(mapping = NULL,
                    data = NULL,
                    geom = "step",
                    direction  = "hv",
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    ...) {

  ggplot2::layer(
    stat = Stat_PCHART,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}

