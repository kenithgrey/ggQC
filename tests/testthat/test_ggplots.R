# NP-CHART ----------------------------------------------------------------
# |CHECK-PLOT| np-chart ggplot Checks
# # should give warning "... n in stat_QC"
# require(ggplot2)
# ggplot(data = bin_data, aes(trial, NNC)) +
#   geom_point() +
#   stat_QC(method = "np")
#
# # should give warning "... n < Item Nonconforming"
# ggplot(data = bin_data, aes(trial, NNC)) +
#   geom_point() +
#   stat_QC(method = "np", n = 10)
#
# #Should PASS with plot
# ggplot(data = bin_data, aes(trial, NNC)) +
#   geom_point() +
#   stat_QC(method = "np", n = 60)

# P-CHART ----------------------------------------------------------------
# |CHECK-PLOT| p-chart ggplot Checks
# wheeler264 <- read.table(file = "tests/testthat/wheeler_USPC_264.csv", header=T, sep=",")
# colnames(wheeler264) <- c("Day", "Date", "Num_Incomplete_Items", "Num_Items_in_Set",
#                           "Proportion_Incomplete")
# ggplot(data = wheeler264, aes(x=Day, y=Proportion_Incomplete)) +
#   geom_point() + geom_line() +
#   stat_QC(method="p")
#
# ggplot(data = wheeler264, aes(x=Day, y=Proportion_Incomplete, n=Num_Items_in_Set)) +
#   geom_point() + geom_line() +
#   stat_pchart()
