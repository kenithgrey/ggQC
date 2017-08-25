#################################
#Starting place for a pareto Stat#
##################################
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
                              compute_group = function(data, scales, cumsums=F, fill.bars=c("red", "white")){
                              #print(data)
                              df <- data # copy the data
                              #print(str(scales$x$range$range))
                              df$range <- strsplit(scales$x$range$range, " ") #get the current listing of the X axis
                              df<-df[order(df$y, decreasing = T),] # Reorder the data frame acording to the value of Y
                              df$x <- seq(1:nrow(df)) # resequence the x labeles
                              if (cumsums) { # Bars or Points if Points
                                df$y_Natural <- df$y
                                df$y <- cumsum(df$y) # get the cumlative
                                return(df)
                              }
                              scales$x$range$range <- as.character(df$range) # reset the scale labels
                              df$fill <- grDevices::colorRampPalette(fill.bars)(nrow(df)) # makes the prety color gradient in the bars
                              #print(str(scales$x$range$range))
                              #print(df)
                              df # returns the data frame
                              }


)

#' @export
#' @title Generate a Pareto Plot with ggplot
#' @description stat function to creat ggplot Pareto chart
#' #####NEED TO FILL THESE IN######
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param group defines grouping for variable for pareto plot, default and suggested is 1.
#' @param color.point color, used to define point color of cumulative percentage line
#' @param size.point number, used to define point size of cumulative percentage line
#' @param color.line color, used to define line color of cumulative percentage line
#' @param size.line color, used to define line weight of cumulative percentage line
#' @param fill.bars character vector length 2, start and end colors for pareto bars.
#'
#' @return Pareto plot.
#'
#' @examples
#' ############################
#' #  Example 1: Pareto Plot  #
#' ############################
#'
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  df <- data.frame(
#'                   x = letters[1:10],
#'                   y = as.integer(runif(n = 10, min = 0, max=100))
#'                  )
#'
#'# Render Pareto Plot ------------------------------------------------------
#'
#'
#' ggplot(df, aes(x=x, y=y)) +
#'  Stat_pareto(color.point = "red",
#'              size.point = 3,
#'              color.line = "black",
#'              #size.line = 1,
#'              fill.bars = c("blue", "orange"),
#'  )

Stat_pareto <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    #yintercept = NULL,
                    position = "identity",
                    show.legend = NA,
                    inherit.aes = TRUE,
                    group = 1,
                    na.rm = FALSE,
                    color.point="black",
                    size.point=2,
                    color.line="black",
                    size.line=.5,
                    fill.bars=c("red", "white"),
                    ...) {

  Points <- ggplot2::layer( #take care of the points
    stat = Stat_PARETO,
    data = data,
    mapping = ggplot2::aes(group=1),
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cumsums = T,
                  color = color.point,
                  size = size.point, ...))

  Line <- ggplot2::layer( # Take care of the lines connecting points
    stat = Stat_PARETO,
    data = data,
    mapping = ggplot2::aes(group=1),
    geom = "line",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cumsums = T,
                  color = color.line,
                  size = size.line,
                   ...))

  # Takes care of the double axis
  SEC.scaleY <- ggplot2::scale_y_continuous(
    sec.axis = ggplot2::sec_axis(~./(max(.)*.95)*100,
    name = "Cumulative Percentage" ))


  Bars <- ggplot2::layer( #draw the bars
    stat = Stat_PARETO,
    data = data,
    mapping = ggplot2::aes(group=1),
    geom = "col",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cumsums = F,
                  color = "black",
                  fill.bars = fill.bars,
                  ...))



  return(list(Bars, Line, Points, SEC.scaleY))

}

