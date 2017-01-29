require(ggplot2)


#How do you say how much you want to round?
XBar <- ggproto("Xbar", Stat, 
        compute_group = function(data, scales, n=1, digits=1){
          limits_df <- data.frame(yintercept = 
            c(max(data$y)/sqrt(n), 
            min(data$y)/sqrt(n), 
            mean(data$y)))
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
                       n=1, digits=1,
                       ...) {
  layer(
    stat = XBar,
    #yintercept=XBar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=n, digits=digits, ...)
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
                 n=1,digits=1,
                 ...) {
  layer(
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


random_data <- data.frame(x=1:100, y=rnorm(100, 0, .5), grouping="A")
random_data2 <- data.frame(x=101:200, y=rnorm(100, 0, .2), grouping="B")
random_data3 <- rbind(random_data, random_data2)

ggplot(random_data3, aes(x=x, y=y, group=grouping)) +
  geom_point() + geom_line() +
  xbar(n=10, digits = 2) +
  #xbar(geom="label", hjust=1.1, vjust=.5, size=3) + 
  xbar_label(n=10, digits = 2) +
  facet_grid(.~grouping, scales = "free_x") + theme_bw()
