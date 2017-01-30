
XBar <- ggplot2::ggproto("Xbar", ggplot2::Stat,
        compute_group = function(data, scales, n=NULL, digits=1){
          #print(dfs)
          limits_df <- data.frame(yintercept =
            c(
              t(QC_Lines(data = data, value = "y", grouping = "x", n=n))[-c(1:2)]
            )
          )
          limits_df$y = limits_df$yintercept
          limits_df$x = Inf
          limits_df$label = round(limits_df$yintercept,digits)
          limits_df
        }
        )


xbar <- function(mapping = NULL,
                       data = NULL,
                       geom = "hline",
                       yintercept = NULL,
                       position = "identity",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       n=NULL, digits=1,
                       ...) {
  ggplot2::layer(
    stat = XBar,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n,
                  digits=digits, ...)
  )
}

xbar_label <- function(mapping = NULL,
                 data = NULL,
                 geom = "label",
                 yintercept = NULL,
                 position = "identity",
                 na.rm = FALSE,
                 show.legend = NA,
                 inherit.aes = TRUE,
                 n=NULL,digits=1,
                 ...) {
  ggplot2::layer(
    stat = XBar,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, hjust=1.1,
                  vjust=.5, size=3,n=n, digits=digits, ...)
  )
}


