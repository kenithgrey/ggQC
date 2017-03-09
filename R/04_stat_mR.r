Stat_MR <- ggplot2::ggproto("Stat_MR", ggplot2::Stat,
      compute_group = function(data, scales){
        mRs3<- mR_points_gg(data = data, value = "y", grouping = "x")
        mRs <- data.frame(y=mRs3, x=data$x)
       }

)

#' @export
stat_mR <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    yintercept = NULL,
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE, color.mr_point="black",
                    color.mr_line="black", color.qc_limits = "red",
                    color.qc_center = "blue",
                    ...) {


  Points <- ggplot2::layer(
    stat = Stat_MR,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color=color.mr_line, ...))

  Connects <- ggplot2::layer(
    stat = Stat_MR,
    data = data,
    mapping = mapping,
    geom = "line",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color=color.mr_point, ...))

  Limits <- ggplot2::layer(
    stat = Stat_QC_LIMITS, data = data, mapping = mapping,
    geom = "hline", position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=1, method="mR",
                  color= color.qc_limits, ...))

  Centerline <- ggplot2::layer(
    stat = Stat_QC_CL, data = data, mapping = mapping,
    geom = "hline", position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=1, method="mR",
                  color=color.qc_center, ...)
  )


return(list(Limits, Centerline, Connects, Points))

}

