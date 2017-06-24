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
df
require(ggplot2)
ggplot(df, aes(x=x, y=y)) +
  Stat_pareto() #+
  #scale_fill_manual(values = colorRampPalette(c("orange", "white"))(nrow(df)))


