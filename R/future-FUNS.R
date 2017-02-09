# Future Functions

#Want to build up for a 3 way control chart. see wheeler_USPC pg. 222

#Right now the mR function only works if the data one dimentional.
# mR_points_future <- function(y, data=NULL, formula = NULL){
#   if(is.null(formula)) {
#     return(c(NA, abs(diff(y))))
#   }
#   aggTemp <- aggregate(formula, data=data, FUN=mean)
#   return(c(NA, abs(diff(aggTemp[, ncol(aggTemp)]))))
# }

# # mR for three way plot ---------------------------------------------------
# #Not ready for this think need special stat
# ggmR <- ggplot(Wheeler108, aes(x=Hour, y=value, group=1)) +
#   #geom_point() + #geom_line() +
#   #geom_line(aes(group=interaction(Hour,PressCycle))) +
#   #geom_line(data = Wheeler108, aes(group=as.factor(Hour))) +
#   stat_summary(fun.y = "mR_points",
#                #fun.args = list(data=Wheeler108,
#                #                 formula=value~Cavity+Hour),
#                colour = "red", size = 1, geom = c("line") )+
#   stat_summary(fun.y = "mR_points",
#                #fun.args = list(data=Wheeler108,
#                #                 formula=value~Cavity+Hour),
#                colour = "red", size = 2, geom = c("point"))+
#   stat_QC(digits = 2, method = "sBar") +
#   stat_QC_labels(digits = 2, method = "sBar") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
#   facet_grid(.~Cavity, scales = "free_x") # + theme_bw()
#
# ggmR
