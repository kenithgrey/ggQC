##################################
#Starting place for a pareto Stat#
##################################

# df <- data.frame(
#   Thing = letters[1:10],
#   Time = as.integer(runif(n = 10, min = 0, max=100))
# )
#
# df<-df[order(df$Time, decreasing = T),]
# df$cumsum_prop <- cumsum(df$Time/sum(df$Time))
# df$Thing_FAC <- factor(df$Thing, ordered = T, levels = df$Thing)
#
# ggplot(df, aes(x=Thing_FAC, y=cumsum(Time))) +
#   geom_point() +
#   geom_col(data = df, aes(x=Thing_FAC, y=Time, fill=Thing_FAC), color="black") +
#   scale_fill_manual(values = colorRampPalette(c("red", "white"))(nrow(df))) +
#   guides(fill=FALSE) +
#   scale_y_continuous(sec.axis = sec_axis(~./max(.)))

#https://rpubs.com/kohske/dual_axis_in_ggplot2
