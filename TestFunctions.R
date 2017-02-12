require(rQC)
require(plyr)
require(ggplot2)
require(gridExtra)

# Test Data ---------------------------------------------------------------

df <- data.frame(process = rep(1,100), DIN = rnorm(100,0,1), repitition=1:20 )
df2 <- data.frame(process = rep(2,100), DIN = rnorm(100,5,.5), repitition=1:10 )
df_all <- rbind(df, df2)
df_all$x <- rep(1:100, times=2)
df_all$regions <- rep(c("top", "bottom"), each=50)

# Basic Test Data ---------------------------------------------------------



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

# Wheeler Data 2 way XbarR ------------------------------------------------
Wheeler108 <- read.csv(file = "tests/testthat/Wheeler_USPC_103.csv", header=T)
head(Wheeler108)


QC_Lines(Wheeler108, formula=value~Hour+PressCycle, method = "xBar.rBar")

QC_Lines(Wheeler108, formula=value~Hour+Cavity, method = "xBar.rBar")


ddply(Wheeler108,
      .variables = c("Cavity"),
      .fun = function(df) {
        QC_Lines(data = df, formula = value~Cavity+Hour)}  )


# aov.mdl <- aov(value~as.factor(Hour)/PressCycle+Cavity, data=Wheeler108)
# summary(aov.mdl)

# require(BHH2)
# anovaPlot(aov.mdl, labels = T)

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

ggDispersion <- ggplot(Wheeler108, aes(x=as.factor(Hour), y=value, group=1)) +
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


# Background XmR Stuff ----------------------------------------------------
mR_points(df$y)

##mR by group##
ddply(.data = df_all, .variables = "process",
        mutate, mR =  mR_points(DIN))

#head(df_all)
# aggregate(formula=DIN~process, FUN=mR_points, data=df_all)
   # plot(mR_points(x), ylim=c(0,5))
 # abline(h = c(mR(x), mR_UCL(x)))
 # mR_points(formula = value~Cavity+Hour, data =Wheeler108 )
 #mR_points(df$y)
 df <- data.frame(x=1:10, y=rnorm(n = 10,mean = 10, sd = 1))
  mR_points3(df, formula = y~x, )
 head(df)
qplot(data=df, x=x, y=y) +
  stat_mR(color="red") +
  stat_mR(color="red", geom="line")


# Two Way XmR Chart -------------------------------------------------------
#rm(ggRbar)
ggX <- ggplot(data = df_all, aes(x=repitition,y=DIN)) +
  geom_point(alpha=.3) +
  stat_summary(fun.y = mean, color="black", geom=c("line")) +
  stat_summary(fun.y = mean, color="black", geom=c("point")) +
  stat_QC(digits = 2) +
  stat_QC(digits = 2, n=1, color="blue") + #indv
  stat_QC(digits = 2, n=1, color="green", method="XmR") + #XmR
  stat_QC_labels(digits=2) +

  facet_grid(.~process, scales = "free_x")

#df_all2<-transform(df_all, repitition=as.factor(repitition))

ggmR <- ggplot(data = df_all, aes(x=repitition,y=DIN)) +
  #geom_point() +
  stat_mR(color="red", geom=c("line")) +
  stat_mR(color="red")  +
  stat_QC(digits = 2, method="mR")  +

  facet_grid(.~process, scales = "free_x")


ggRbar <- ggplot(data = df_all, aes(x=repitition,y=DIN)) +
  #geom_point() +
  stat_summary(fun.y = "QCrange", color="blue", geom = "point") +
  stat_QC(digits=2, method="rBar") +
  facet_grid(.~process, scales = "free_x")

grid.arrange(ggX, ggmR, ggRbar, nrow=3)
