require(rQC)
require(plyr)
require(ggplot2)
##


df <- data.frame(process = rep(1,100), DIN = rnorm(100,0,1), repitition=letters[1:20] )
df2 <- data.frame(process = rep(2,100), DIN = rnorm(100,5,.5), repitition=letters[1:10] )
df_all <- rbind(df, df2)
df_all$x <- rep(1:100, times=2)
df_all$regions <- rep(c("top", "bottom"), each=50)

ylines_XbarR(data = df_all, value = "DIN", grouping = "repitition", n=2)

LimitData <- ddply(df_all,
      .variables = c("process","regions"),
      .fun = function(df) {
        ylines_XbarR(data = df, value = "DIN",
                     grouping = "repitition")}  )

(LimitData$xBar_UCL - LimitData$xBar_Bar) ==
(LimitData$xBar_Bar - LimitData$xBar_LCL)

ddply(df_all, .variables = "process", summarise,
      mean = mean(DIN),
      mR = mR(DIN),
      LCL = xBar_one_LCL(DIN),
      UCL = xBar_one_UCL(DIN)
      )


ggplot(df_all, aes(x=repitition, y=DIN, group=process)) +
  geom_point() + #geom_line() +
  stat_summary(fun.y = "mean", colour = "red", size = 1, geom = c("line"))+
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = c("point"))+
  xbar(digits = 2) +
  xbar_label(digits = 2) +
  facet_grid(process~regions, scales = "free_x") + theme_bw()
