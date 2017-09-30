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
          compute_group = function(data, scales, method=method, callFrom=NULL){
            if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "xMedian.rBar", "xMedian.rMedian")) {
              CentralLimitCol <- 6
            }else if (method == "XmR"){
              CentralLimitCol <- 2
            }else{
              return(warning(paste("Unknown method: ", method,
                                   "\n Please see help file or use the following methods:",
                                   "\n XmR, xBar.rBar, xBar.rMedian, xBar.sBar, xBar.rBar,
                                   xBar.rMedian, xBar.sBar")))
            }

            #print("stat")
            #print(head(data))
          #print(method)
          df <- data # copy the data
          df$Index <- 1:nrow(df)
          if (method == "XmR"){
          #print("Do XmR")
          viloation_df <- QC_Violations(data = data$y, method = "XmR")
          #print("DF2")
          df2 <- merge(df, viloation_df, by="Index", all.x = TRUE)
          #print(head(df2))

          #print("DF3")
          df3 <- df2[df2$Violation_Result == df2$PANEL, ]
          #print(head(df3))
          if (callFrom == "SigmaLines"){
            df3$colour <- "green"
          }else{
            df3$colour <- ifelse(df3$Violation == TRUE, "red", "black")
          }
          #make the lines that go at the sigma levels
          centerLine <- QC_Lines(data = data$y, method=method)[CentralLimitCol][[1]]
          Sigma <- QC_Lines(data = data$y, method=method)$sigma
          #print(centerLine[[1]])
          #print(Sigma)
          df3$yintercept <- c(centerLine, centerLine + (as.numeric(df3$PANEL[1])-1)*Sigma, centerLine -
                            (as.numeric(df3$PANEL[1])-1)*Sigma)
          #print(yintercept)
          #print(unique(df3$PANEL))
          #print(head(df3))
          #df3$size<- df3$z_score
          # print(data.frame(Pannel_name=unique(df3$PANEL)),
          #       pnum = as.numeric(unique(df3$PANEL)))
          #print(head(df3))

          df3
                              }else if(method %in% c("c", "p", "u", "np")){
                                print("c, p, u, and np charts not supported by Stat_qc_violations")
                              }else{
                                print("Do XbarR type anaylsis")
                              }

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
    #color = color,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="Points",  ...))

  SigmaLines <- ggplot2::layer( #take care of the points
    stat = Stat_QC_VIOLATIONS,
    data = data,
    mapping = mapping,
    geom = "hline",
    #color = "green",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="SigmaLines", ...))

  # Line <- ggplot2::layer( # Take care of the lines connecting points
  #   stat = Stat_PARETO,
  #   data = data,
  #   mapping = ggplot2::aes(group=1),
  #   geom = "line",
  #   position = position,
  #   show.legend = show.legend,
  #   inherit.aes = inherit.aes,
  #   params = list(na.rm = na.rm, cumsums = T,
  #                 color = color.line,
  #                 size = size.line,
  #                  ...))
  Facet <- facet_qc_violations(method=method)
  # Takes care of the double axis
  # SEC.scaleY <- ggplot2::scale_y_continuous(
  #   sec.axis = ggplot2::sec_axis(~./(max(.)*.95)*100,
  #   name = "Cumulative Percentage" ))


  # Bars <- ggplot2::layer( #draw the bars
  #   stat = Stat_PARETO,
  #   data = data,
  #   mapping = ggplot2::aes(group=1),
  #   geom = "col",
  #   position = position,
  #   show.legend = show.legend,
  #   inherit.aes = inherit.aes,
  #   params = list(na.rm = na.rm, cumsums = F,
  #                 color = "black",
  #                 fill.bars = fill.bars,
  #                 ...))



  return(list(SigmaLines, Points, Facet))

}

