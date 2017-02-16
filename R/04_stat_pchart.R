Stat_PCHART <- ggplot2::ggproto("Stat_PCHART", ggplot2::Stat,
                            compute_group = function(data, scales){
                              if (is.null(data$n)){
                                warning("**p-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
                                return(NULL)
                              }
                              pdata <- ylines_indv(y = data$y, data$n, method = "p")
                              pdata$x <- data$x
                              pchart_data <- reshape2::melt(pdata, id.vars=c("x"))
                              colnames(pchart_data) <- c("x", "group", "y")
                              pchart_data


                              # data$UCL <- pBar_UCL(data$y, data$n)
                              # data$LCL <- pBar_LCL(data$y, data$n)
                              # data$mean <- mean(data$y)
                              # pchart_data <- reshape2::melt(data = data[,c("x","mean","UCL","LCL")],
                              #                                     id.vars = c("x"))
                              # colnames(pchart_data) <- c("x", "group", "y")
                              # pchart_data

                            }

)

#' @export
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

