require(ggplot2)

# Setup Some Data ---------------------------------------------------------
set.seed(5555)
candle_df1t3 <- data.frame(
  Cycle = as.factor(rep(1:24, each=3)),
  candle_width = rnorm(n = 3*24, mean = 10, sd = 1),
  mold_cell = as.ordered(rep(1:3))
)

candle_df4 <- data.frame(
  Cycle = as.factor(rep(1:24, each=1)),
  candle_width = rnorm(n = 1*24, mean = 11, sd = 2),
  mold_cell = as.ordered(rep(4, each=24))
)

candle_df <- rbind(candle_df1t3, candle_df4)

candle_df <- candle_df[order(candle_df$mold_cell, candle_df$Cycle),]
candle_df$INDEX <- 1:nrow(candle_df)

candle_df <- candle_df[order(candle_df$Cycle, candle_df$mold_cell),]
candle_df$INDEX2 <- 1:nrow(candle_df)


# PLot the Data -----------------------------------------------------------
XmR <- ggplot(candle_df, aes(x = INDEX, y = candle_width, group = 1)) +
  geom_point() + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  stat_QC(method = "XmR")

XmR + ggtitle("Candle Data", subtitle = "Ordered by Mold Cell then Cycle")


# Look at the Violatrions -------------------------------------------------
XmR.violations <-
  ggplot(candle_df[order(candle_df$INDEX),],
         aes(x = INDEX, y = candle_width, group = 1)) +
  stat_qc_violations(method = "XmR", show.facets = c(3,2)) +
  #stat_qc_violations(method = "XmR") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

XmR.violations +
  ggtitle("Candle Violation Data", subtitle = "Ordered by Mold Cell then Cycle")

QC_Violations(data = candle_df$candle_width, method = "XmR" )


