require(rQC)
require(plyr)
require(ggplot2)
require(gridExtra)
##


df <- data.frame(process = rep(1,100), DIN = rnorm(100,0,1), repitition=letters[1:20] )
df2 <- data.frame(process = rep(2,100), DIN = rnorm(100,5,.5), repitition=letters[1:10] )
df_all <- rbind(df, df2)
df_all$x <- rep(1:100, times=2)
df_all$regions <- rep(c("top", "bottom"), each=50)

mR(y=df_all$DIN)
xBar_one_UCL(y=df_all$DIN)
xBar_one_LCL(y=df_all$DIN)

rBar_UCL(data = df, value = "DIN", "repitition" )
rBar_LCL(data = df, value = "DIN", "repitition" )

rMedian_UCL(data = df, value = "DIN", "repitition" )
rMedian_LCL(data = df, value = "DIN", "repitition" )
rMedian_UCL(data = df, formula = DIN~repitition )

ddply(df_all, .variables = "process", summarise,
      mean = mean(DIN),
      mR = mR(DIN),
      LCL = xBar_one_LCL(DIN),
      UCL = xBar_one_UCL(DIN)
)


QC_Lines(data = df_all, value = "DIN", grouping = "repitition", n=2, method = "rBar")

ddply(df_all,
      .variables = c("process","regions"),
      .fun = function(df) {
        QC_Lines(data = df, value = "DIN", n=20,
                     grouping = "repitition", method = "rMedian")}  )


QC_Lines(data= df_all, formula = DIN~process+regions+repitition)

aggregate(formula=DIN~process+regions+repitition, data=df_all, FUN=length)

(LimitData$xBar_UCL - LimitData$xBar_Bar) ==
(LimitData$xBar_Bar - LimitData$xBar_LCL)

# xBar_Bar(df_all, "DIN", "repitition")
# xMedian_Bar(df_all, "DIN", "repitition")

Wheeler108 <- read.csv(file = "tests/testthat/Wheeler_USPC_103.csv", header=T)
head(Wheeler108)


QC_Lines(Wheeler108, formula=value~Hour+PressCycle, method = "xBar.rBar")

QC_Lines(Wheeler108, formula=value~Hour+Cavity, method = "xBar.rBar")


ddply(Wheeler108,
      .variables = c("Cavity"),
      .fun = function(df) {
        QC_Lines(data = df, formula = value~Cavity+Hour)}  )


aov.mdl <- aov(value~as.factor(Hour)/PressCycle+Cavity, data=Wheeler108)
summary(aov.mdl)

require(BHH2)
anovaPlot(aov.mdl, labels = T)

#ggplot(Wheeler108, aes(x=Hour, y=value, group=Cavity)) +
Wheeler108$Hour_Cycle <- paste0(Wheeler108$Hour, "_", Wheeler108$PressCycle)
ggplot(Wheeler108, aes(x=Hour, y=value, group=1)) +
  #geom_point() + #geom_line() +
  #geom_line(aes(group=interaction(Hour,PressCycle))) +
  #geom_line(data = Wheeler108, aes(group=as.factor(Hour))) +
  stat_summary(fun.y = "mean", colour = "red", size = 1, geom = c("line") )+
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = c("point"))+
  stat_QC(digits = 2) +
  stat_QC_labels(digits = 2) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  facet_grid(.~Cavity, scales = "free_x") # + theme_bw()

QCrange <- function(x){
  max(x) - min(x)
}



ggvalues <- ggplot(Wheeler108, aes(x=Hour, y=value, group=1)) +
  #geom_point() + #geom_line() +
  #geom_line(aes(group=interaction(Hour,PressCycle))) +
  #geom_line(data = Wheeler108, aes(group=as.factor(Hour))) +
  stat_summary(fun.y = "mean", colour = "red", size = 1, geom = c("line") )+
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = c("point"))+
  stat_QC(digits = 2, method="xBar.sBar") +
  stat_QC_labels(digits = 2, method="xBar.sBar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  facet_grid(.~Cavity, scales = "free_x") # + theme_bw()

ggDispersion <- ggplot(Wheeler108, aes(x=Hour, y=value, group=1)) +
  #geom_point() + #geom_line() +
  #geom_line(aes(group=interaction(Hour,PressCycle))) +
  #geom_line(data = Wheeler108, aes(group=as.factor(Hour))) +
  stat_summary(fun.y = "sd", colour = "red", size = 1, geom = c("line") )+
  stat_summary(fun.y = "sd", colour = "red", size = 2, geom = c("point"))+
  stat_QC(digits = 2, method = "sBar") +
  stat_QC_labels(digits = 2, method = "sBar") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  facet_grid(.~Cavity, scales = "free_x") # + theme_bw()

grid.arrange(ggvalues, ggDispersion, nrow=2)
