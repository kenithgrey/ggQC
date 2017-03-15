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

Stat_MR <- ggplot2::ggproto("Stat_MR", ggplot2::Stat,
      compute_group = function(data, scales){
        mRs3<- mR_points_gg(data = data, value = "y", grouping = "x")
        mRs <- data.frame(y=mRs3, x=data$x)
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
stat_mR <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    #yintercept = NULL,
                    position = "identity",
                    show.legend = NA,
                    inherit.aes = TRUE,
                    na.rm = FALSE,
                    color.mr_point="black",
                    color.mr_line="black", color.qc_limits = "red",
                    color.qc_center = "blue",
                    ...) {


  Points <- ggplot2::layer(
    stat = Stat_MR,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color=color.mr_line, ...))

  Connects <- ggplot2::layer(
    stat = Stat_MR,
    data = data,
    mapping = mapping,
    geom = "line",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, color=color.mr_point, ...))

  Limits <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = "hline", position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=1, method="mR",
                  color= color.qc_limits, draw.line = "limit", ...))

  Centerline <- ggplot2::layer(
    stat = STAT_QC, data = data, mapping = mapping,
    geom = "hline", position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=1, method="mR",
                  color=color.qc_center,draw.line = "center", ...)
  )


return(list(Limits, Centerline, Connects, Points))

}

