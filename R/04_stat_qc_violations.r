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
          compute_group = function(data, scales, method = method, callFrom = NULL,
                                   n = NULL, line.color=line.color,
                                   point.color = point.color,
                                   violation_point.color = violation_point.color,
                                   rule.color = rule.color){

        df <- data # copy the data

            if (method == "XmR"){
              df$Index <- 1:nrow(df)
              CentralLimitCol <- 2

              viloation_df <- QC_Violations(data = data$y, method = method)
              df2 <- merge(df, viloation_df, by="Index", all.x = TRUE)
              df3 <- df2[df2$Violation_Result == df2$PANEL, ]

              if (callFrom == "SigmaLines"){
                QC_DATA <- QC_Lines(data = data$y, method=method)
                centerLine <- QC_DATA[CentralLimitCol][[1]]
                Sigma <- QC_DATA$sigma
                }
            }else if(method %in% c("xBar.rBar", "xBar.rMedian", "xBar.sBar", "xMedian.rBar", "xMedian.rMedian")) {
              df$Index <- 1:nrow(df)

              CentralLimitCol <- 6
              viloation_df <- QC_Violations(data = df, value = "y", grouping = "x", method = method)
              df2 <- merge(df, viloation_df, by="Index", all.y = TRUE) #don't think the index soln will work
              df3 <- df2[df2$Violation_Result == df2$PANEL, ]
              df3$y <- df3$data

              if (callFrom == "SigmaLines"){
                QC_DATA <- QC_Lines(data = df, value = "y", grouping = "x", n=n, method = method)
                centerLine <- QC_DATA[CentralLimitCol][[1]]
                Sigma <- QC_DATA$sigma
                }

            }else{
              return(warning(paste("Unknown method: ", method,
                                   "\n Please see help file or use the following methods:",
                                   "\n XmR, xBar.rBar, xBar.rMedian, xBar.sBar, xBar.rBar,
                                   xBar.rMedian, xBar.sBar")))
            }

        #Setup the color display for points or lines
        if (callFrom == "SigmaLines"){
          df3 <- df3[1:3,]
          df3$colour <- rule.color
          df3$yintercept <- c(centerLine,
                              centerLine + (as.numeric(df3$PANEL[1])-1)*Sigma,
                              centerLine - (as.numeric(df3$PANEL[1])-1)*Sigma)
          }else if(callFrom == "Points"){
          df3$colour <- ifelse(df3$Violation == TRUE, violation_point.color, point.color)

        }else if(callFrom == "Lines"){
          df3$colour <- line.color
        }

        #make the lines that go at the sigma levels

        #print(df3)
        return(df3)




  }
)

#' @export
#' @title Inspect QC Violations
#' @description ggplot stat function that renders a faceted plot of QC violations
#' based on the following 4 rules:
#' \itemize{
#' \item \bold{Violation Same Side:} 8 or more consecutive, same-side points
#' \item \bold{Violation 1 Sigma:} 4 or more consecutive, same-side points exceeding 1 sigma
#' \item \bold{Violation 2 Sigma:} 2 or more consecutive, same-side points exceeding 2 sigma
#' \item \bold{Violation 3 Sigma:} any points exceeding 3 sigma
#' }
#'
#' @param method string, calling the following methods:
#' \itemize{
#'   \item \bold{Individuals Charts}: XmR,
#'   \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' }
#'
#' @param geom_points boolean, draw points
#' @param point.size number, size of points on chart
#' @param point.color string, color of points on charts (e.g., "black")
#' @param violation_point.color string, color of violation points on charts (e.g., "red")
#' @param geom_line boolean, draw line
#' @param line.color string, color of lines connecting points
#' @param rule.color string, color or horizontal rules indicating distribution center and sigma levels
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @inheritParams ggplot2::stat_identity
#'
#' @return faceted plot.
#'
#' @examples
#' #####################################
#' #  Example 1: XmR Check Violations  #
#' #####################################
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'
#'     set.seed(5555)
#'     QC_XmR <- data.frame(
#'     data = c(c(-1, 2.3, 2.4, 2.5),                        #Outlier Data
#'           sample(c(rnorm(60),5,-5), 62, replace = FALSE), #Normal Data
#'           c(1,-.3, -2.4,-2.6,-2.5,-2.7, .3)),             #Outlier Data
#'     Run_Order = 1:73                                      #Run Order
#'     )
#'
#'
#'# Render QC Violation Plot ------------------------------------------------------
#'
#'    EX1 <- ggplot(QC_XmR, aes(x = Run_Order, y = data)) +
#'      stat_qc_violations(method = "XmR")   #Makes facet graph with violations
#'    #EX1
#' #######################################
#' #  Example 2: Xbar Check Violations   #
#' #######################################
#'
#'# Setup Some Data ------------------------------------------------------------
#'      QC_xBar.rBar <- do.call(rbind, lapply(1:3, function(X){
#'        set.seed(5555+X)                                   #Loop over 3 seeds
#'        data.frame(
#'          sub_group = rep(1:42),                           #Define Subgroups
#'          sub_class = letters[X],
#'          c(
#'           c(runif(n = 5, min = 2.0,3.2)),                 #Outlier Data
#'           sample(c(rnorm(30),5,-4), 32, replace = FALSE), #Normal Data
#'           c(runif(n = 5, min = -3.2, max = -2.0))         #Outlier Data
#'          )
#'       )
#'      }
#'    )
#' )
#'
#' colnames(QC_xBar.rBar) <- c("sub_group","sub_class", "value")
#'
#'# Render QC Violation Plot --------------------------------------------------
#'     EX2 <- ggplot(QC_xBar.rBar, aes(x = sub_group, y = value)) +
#'       stat_qc_violations(method = "xBar.rBar")
#'       #stat_qc_violations(method="xBar.rMedian")
#'       #stat_qc_violations(method="xBar.sBar")
#'       #stat_qc_violations(method="xMedian.rBar")
#'       #stat_qc_violations(method="xMedian.rMedian")
#'    #EX2
#' #######################################################
#' # Complete User Control - Bypass stat_qc_violation   #
#' #######################################################
#' #### The code below has two options if you are looking for complete
#' #### control over the look and feel of the graph. Use option 1 or option
#' #### 2 as appropriate. If you want something quick and easy use examples above.
#'
#' ##### Option 1: Setup for XmR Type Data
#'  # QC_XmR: Defined in Example 1
#'    QC_Vs <- QC_Violations(data  = QC_XmR$data, method = "XmR")
#'    QC_Stats <- QC_Lines(data  = QC_XmR$data, method = "XmR")
#'    MEAN <- QC_Stats$mean
#'    SIGMA <- QC_Stats$sigma
#'
#'##### Option 2: Setup for xBar.rBar Type Data
#'  # QC_xBar.rBar: Defined in Example 2
#'    QC_Vs <- QC_Violations(data  = QC_xBar.rBar,
#'                           formula = value~sub_group,
#'                           method = "xBar.rBar")
#'    QC_Stats <- QC_Lines(data  = QC_xBar.rBar,
#'                         formula = value~sub_group,
#'                         method = "xBar.rBar")
#'    MEAN <- QC_Stats$xBar_Bar
#'    SIGMA <- QC_Stats$sigma
#'
#'##### Setup second table for horizontal rules
#'  FacetNames <- c("Violation Same Side",
#'                  "Violation 1 Sigma",
#'                  "Violation 2 Sigma",
#'                  "Violation 3 Sigma")
#'
#'  QC_Vs$Violation_Result <- ordered(QC_Vs$Violation_Result,
#'                                      levels=FacetNames)
#'
#'  QC_Stats_df <- data.frame(
#'    Violation_Result = factor(x = FacetNames, levels = FacetNames),
#'    SigmaPlus = MEAN+SIGMA*0:3,
#'    MEAN = MEAN,
#'    SigmaMinus = MEAN-SIGMA*0:3
#'  )
#'
#'##### Make the Plot
#'  ggplot(QC_Vs, aes(x=Index, y=data, color=Violation, group=1)) +
#'    geom_point() + geom_line() +
#'    facet_grid(.~Violation_Result) +
#'    geom_hline(data = QC_Stats_df, aes(yintercept = c(SigmaPlus))) +
#'    geom_hline(data = QC_Stats_df, aes(yintercept = c(SigmaMinus))) +
#'    geom_hline(data = QC_Stats_df, aes(yintercept = c(MEAN)))



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
                    geom_points = TRUE,
                    geom_line = TRUE,
                    # color.point="black",

                    point.size = 1.5,
                    point.color = "black",
                    violation_point.color = "red",

                    rule.color = "darkgreen",

                    line.color=NULL,
                    # size.line=.5,
                    # fill.bars=c("red", "white"),
                    ...) {
if(geom_points){
Points <- ggplot2::layer( #take care of the points
    stat = Stat_QC_VIOLATIONS,
    data = data,
    mapping = mapping,
    geom = "point",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="Points",
                  size=point.size,
                  point.color = point.color,
                  violation_point.color = violation_point.color,
                  ...))
}

if(geom_line){
Lines <- ggplot2::layer( #take care of the lines between points
  stat = Stat_QC_VIOLATIONS,
  data = data,
  mapping = mapping,
  geom = "line",
  position = position,
  show.legend = show.legend,
  inherit.aes = inherit.aes,
  params = list(method=method, callFrom="Lines",line.color=line.color,
                ...))
}

SigmaLines <- ggplot2::layer( #take care of the points
    stat = Stat_QC_VIOLATIONS,
    data = data,
    mapping = mapping,
    geom = "hline",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(method=method, callFrom="SigmaLines",
                  rule.color = rule.color, ...))

Facet <- facet_qc_violations(method=method)


if(all(geom_line, geom_points)){
  return(list(SigmaLines, Lines, Points,  Facet))
  }else if(geom_points){
    return(list(SigmaLines, Points, Facet))
  }else if(geom_line){
    return(list(SigmaLines, Lines, Facet))
  }

}

