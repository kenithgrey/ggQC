# Stat U-chart ------------------------------------------------------------

Stat_UCHART <- ggplot2::ggproto("Stat_UCHART", ggplot2::Stat,
                                compute_group = function(data, scales){
                                  if (is.null(data$n)){
                                    warning("**u-chart Error**\n supply vector of opportunity, 'n', in ggplot aes", call. = FALSE)
                                    return(NULL)
                                  }
                                  udata <- ylines_indv(y = data$y, data$n, method = "u")
                                  udata$x <- data$x
                                  uchart_data <- reshape2::melt(udata, id.vars=c("x"))
                                  colnames(uchart_data) <- c("x", "group", "y")
                                  uchart_data
                                }

)

#' @export
stat_uchart <- function(mapping = NULL,
                        data = NULL,
                        geom = "step",
                        direction  = "hv",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        ...) {

  ggplot2::layer(
    stat = Stat_UCHART,
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

