Stat_MR <- ggplot2::ggproto("Stat_MR", ggplot2::Stat,
      compute_group = function(data, scales){
        #suppressWarnings()
        mRs3<- mR_points_gg(data = data, value = "y", grouping = "x")
         mRs <- data.frame(y=mRs3, x=data$x)
         #mRs

      }

)


stat_mR <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    yintercept = NULL,
                    position = "identity",
                    na.rm = FALSE,
                    show.legend = NA,
                    inherit.aes = TRUE,
                    ...) {

  ggplot2::layer(
    stat = Stat_MR,
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

