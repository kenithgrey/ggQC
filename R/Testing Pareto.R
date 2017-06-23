df <- data.frame(
  x = letters[1:10],
  y = as.integer(runif(n = 10, min = 0, max=100))
)
# df$x <- factor(df$x, ordered = T,
#                   levels = df$x[order(df$y)])
#
# df<-df[order(df$Time, decreasing = T),]
# df$cumsum_prop <- cumsum(df$Time/sum(df$Time))
# df$Thing_FAC <- factor(df$Thing, ordered = T, levels = df$Thing)
#
require(ggplot2)
ggplot(df, aes(x=x, y=y)) +
  Stat_pareto()

require(ggplot2)
ggplot(df, aes(x=x, y=y)) +
  Stat_pareto()
  geom_point() +
  geom_col(data = df, aes(x=Thing_FAC, y=Time, fill=Thing_FAC), color="black") +
  scale_fill_manual(values = colorRampPalette(c("red", "white"))(nrow(df))) +
  guides(fill=FALSE) +
  scale_y_continuous(sec.axis = sec_axis(~./max(.)))
