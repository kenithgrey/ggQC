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
Stat_QC_VIOLATIONS <- ggplot2::ggproto("Stat_QC_VIOLATIONS", ggplot2::Stat,
          compute_group = function(data, scales, method = method, callFrom = NULL, n=NULL){

        df <- data # copy the data

            if (method == "XmR"){
              df$Index <- 1:nrow(df)
              CentralLimitCol <- 2

              viloation_df <- QC_Violations(data = data$y, method = method)
              df2 <- merge(df, viloation_df, by="Index", all.x = TRUE)
              df3 <- df2[df2$Violation_Result == df2$PANEL, ]

              centerLine <- QC_Lines(data = data$y, method=method)[CentralLimitCol][[1]]
              Sigma <- QC_Lines(data = data$y, method=method)$sigma

            }else if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "xMedian.rBar", "xMedian.rMedian")) {
              df$Index <- 1:nrow(df)

              CentralLimitCol <- 6
              viloation_df <- QC_Violations(data = df, value = "y", grouping = "x", method = method)
              df2 <- merge(df, viloation_df, by="Index", all.y = TRUE) #don't think the index soln will work
              df3 <- df2[df2$Violation_Result == df2$PANEL, ]
              df3$y <- df3$data
              centerLine <- QC_Lines(data = df, value = "y", grouping = "x", n=n, method = method)[CentralLimitCol][[1]]
              Sigma <- QC_Lines(data = df, value = "y", grouping = "x", n=n, method = method)$sigma

            }else{
              return(warning(paste("Unknown method: ", method,
                                   "\n Please see help file or use the following methods:",
                                   "\n XmR, xBar.rBar, xBar.rMedian, xBar.sBar, xBar.rBar,
                                   xBar.rMedian, xBar.sBar")))
            }

        #Setup the color display for points or lines
        if (callFrom == "SigmaLines"){
          df3$colour <- "darkgreen"
        }else{
          df3$colour <- ifelse(df3$Violation == TRUE, "red", "black")
        }

        #make the lines that go at the sigma levels
        df3$yintercept <- c(centerLine,
                            centerLine + (as.numeric(df3$PANEL[1])-1)*Sigma,
                            centerLine - (as.numeric(df3$PANEL[1])-1)*Sigma)
        #print(df3)
        return(df3)




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

stat_qc_violations <- function(mapping = NULL,
                    data = NULL,
                    geom = "point",
                    #yintercept = NULL,
                    position = "identity",
                    show.legend = NA,
                    inherit.aes = TRUE,
                    #group = 1,
                    na.rm = FALSE,
                    method="xBar.rBar",
                    # color.point="black",
                    # size.point=2,
                    # color.line="black",
                    # size.line=.5,
                    # fill.bars=c("red", "white"),
                    ...) {

Points <- ggplot2::layer( #take care of the points
    stat = Stat_QC_VIOLATIONS,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="Points",  ...))

SigmaLines <- ggplot2::layer( #take care of the points
    stat = Stat_QC_VIOLATIONS,
    data = data,
    mapping = mapping,
    geom = "hline",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="SigmaLines", ...))

Facet <- facet_qc_violations(method=method)



return(list(SigmaLines, Points, Facet))

}

