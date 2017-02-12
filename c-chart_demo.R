# c-chart demo ------------------------------------------------------------
cdata <- data.frame(x=1:30, y=rpois(n = 30, lambda = 25), group="A")
cdata2 <- data.frame(x=1:30, y=rpois(n = 30, lambda = 3), group="B")

df_C <- rbind(cdata, cdata2)

ggplot(data = df_C, aes(x, y)) +
    geom_point() +
    stat_QC(method = "c") +
    facet_grid(.~group)

ylines_indv(y=df_C$y, method = "c")

# p chart ----------------------------------------------------------------
set.seed(5555)
bin_data <- data.frame(
            trial=1:30,
            NNC = rbinom(30, 60, prob = .20),
            N = rep(15, 30))
bin_data$prop <- bin_data$NNC/bin_data$N


sum(bin_data$NNC)/(60*30)
mean(bin_data$prop)

npBar(p = bin_data$NNC/60, 60)

ylines_indv(bin_data$prop, n=60, method = "np")

ggplot(data = bin_data, aes(trial, NNC)) +
  geom_point() +
  stat_QC(method = "np")

ggplot(data = bin_data, aes(trial, NNC)) +
  geom_point() +
  stat_QC(method = "np", n = 14)

ggplot(data = bin_data, aes(trial, NNC)) +
  geom_point() +
  stat_QC(method = "np", n = 15)

# vector boolean test -----------------------------------------------------

if(binCheck_pChart(bin_data$prop, bin_data$N)){
  print("All Good")
}else{

}

# p-chart items of opportunity not constant -------------------------------

wheeler264 <- read.table(file = "tests/testthat/wheeler_USPC_264.csv", header=T, sep=",")
colnames(wheeler264) <- c("Day", "Date", "Num_Incomplete_Items", "Num_Items_in_Set",
  "Proportion_Incomplete")

ggplot(data = wheeler264, aes(x=Day, y=Proportion_Incomplete)) +
  geom_point() + geom_line() +
  stat_QC(method="p")

ggplot(data = wheeler264, aes(x=Day, y=Proportion_Incomplete, n=Num_Items_in_Set)) +
  geom_point() + geom_line() +
  stat_pchart()


# U-chart ------------------------------------------------------------------
wheeler276 <- read.table(file = "tests/testthat/wheeler_USPC_276.csv", header=T, sep=",")
colnames(wheeler276)
uBar_LCL(wheeler276$Rate,wheeler276$No_of_Radiators)

ggplot(data = wheeler276, aes(x=ID, y=Rate)) +
  geom_point() + geom_line() +
  stat_QC(method="u")

ggplot(data = wheeler276, aes(x=ID, y=Rate)) +
  geom_point() + geom_line() +
  stat_uchart()

ggplot(data = wheeler276, aes(x=ID, y=Rate, n=No_of_Radiators)) +
  geom_point() + geom_line() +
  stat_uchart()


