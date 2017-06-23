##################################
#Starting place for a pareto Stat#
##################################

# df <- data.frame(
#   x = letters[1:10],
#   y = as.integer(runif(n = 10, min = 0, max=100))
# )
#
# #
# # df<-df[order(df$Time, decreasing = T),]
# # df$cumsum_prop <- cumsum(df$Time/sum(df$Time))
# # df$Thing_FAC <- factor(df$Thing, ordered = T, levels = df$Thing)
# #
# require(ggplot2)
# ggplot(df, aes(x=x, y=y)) +
#   Stat_pareto()
#   geom_point() +
#   geom_col(data = df, aes(x=Thing_FAC, y=Time, fill=Thing_FAC), color="black") +
#   scale_fill_manual(values = colorRampPalette(c("red", "white"))(nrow(df))) +
#   guides(fill=FALSE) +
#   scale_y_continuous(sec.axis = sec_axis(~./max(.)))

#https://rpubs.com/kohske/dual_axis_in_ggplot2
##############################
# Copyright 2017 Kenith Grey #
##############################

# Copyright Notice --------------------------------------------------------
# This file is part of ggQC.
#
# ggQC is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ggQC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with ggQC.  If not, see <http://www.gnu.org/licenses/>.
Stat_PARETO <- ggplot2::ggproto("Stat_PARETO", ggplot2::Stat,
                              compute_group = function(data, scales, cumsums=F){
                              print(data)
                              df <- data
                              print(str(scales$x$range$range))
                              df$range <- strsplit(scales$x$range$range, " ")
                              df<-df[order(df$y, decreasing = T),]
                              df$x <- seq(1:nrow(df))
                              if (cumsums) {
                                df$y_Natural <- df$y
                                df$y <- cumsum(df$y)
                              }
                              scales$x$range$range <- as.character(df$range)
                              print(str(scales$x$range$range))
                              print(df)
                              #df$x <- factor(df$x,
                              #       levels = df$x[order(df$y)])
                              #print(df)
                              #print(strsplit(" ", scales$x$range$range))
                              #print(data)
                              #data<-df
                              #x$name <- factor(x$name, levels = x$name[order(x$val)])
                              #data2<-data[order(data$y, decreasing = T),]
                              #data2$x <- factor(data2$x, ordered = T,
                              #                  levels = data2$x[order(data2$y)])
                              # data2$breaks <- c(data$x)#if (cumsums) data2$y <- cumsum(data2$y)
                              # # print(data2)
                              #print(data2)
                              #data2
                              }


)

#' @export
#' @title Generate mR chart in ggplot
#' @description ggplot stat used to create a mR chart in ggplot
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param color.mr_point color, to be used for the mR points.
#' @param color.mr_line color, to be used for line connecting points.
#' @param color.qc_limits color, used to colorize the plot's upper and lower mR control limits.
#' @param color.qc_center color, used to colorize the plot's center line.
#' @return data need to produce the mR plot in ggplot.
#' @examples
#' #########################
#' #  Example 1: mR Chart  #
#' #########################
#'
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(processID = as.factor(rep(1,100)),
#'                         metric_value = rnorm(100,0,1),
#'                         subgroup_sample=rep(1:20, each=5),
#'                         Process_run_id = 1:100)
#'  set.seed(5556)
#'  Process2 <- data.frame(processID = as.factor(rep(2,100)),
#'                         metric_value = rnorm(100,5, 1),
#'                         subgroup_sample=rep(1:10, each=10),
#'                         Process_run_id = 101:200)
#'
#'  Both_Processes <- rbind(Process1, Process2)
#'
#'# One Plot Both Processes -------------------------------------------------
#'  ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'    stat_mR() + ylab("Moving Range")
#'
#'# Facet Plot - Both Processes ---------------------------------------------
#'  ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'    stat_mR() + ylab("Moving Range") +
#'    facet_grid(.~processID, scales = "free_x")

# ggplot(df, aes(x=Thing_FAC, y=cumsum(Time))) +
#   geom_point() +
#   geom_col(data = df, aes(x=Thing_FAC, y=Time, fill=Thing_FAC), color="black") +
#   scale_fill_manual(values = colorRampPalette(c("red", "white"))(nrow(df))) +
#   guides(fill=FALSE) +
#   scale_y_continuous(sec.axis = sec_axis(~./max(.)))

Stat_pareto <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    #yintercept = NULL,
                    position = "identity",
                    show.legend = NA,
                    inherit.aes = TRUE,
                    group = 1,
                    na.rm = FALSE,
                    color.mr_point="black",
                    color.mr_line="black", color.qc_limits = "red",
                    color.qc_center = "blue",
                    ...) {

  Points <- ggplot2::layer(
    stat = Stat_PARETO,
    data = data,
    mapping = aes(group=1),
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cumsums = T, ...))

  scaleX <- ggplot2::scale_y_continuous(sec.axis = sec_axis(~./(max(.)*.95)))

  Bars <- ggplot2::layer(
    stat = Stat_PARETO,
    data = data,
    mapping = aes(group=1),
    geom = "col",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cumsums = F, ...))



  return(list(Points, Bars, scaleX))

}

