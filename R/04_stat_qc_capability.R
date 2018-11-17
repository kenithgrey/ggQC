##############################
# Copyright 2018 Kenith Grey #
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

STAT_QC_CAPABILITY <- ggplot2::ggproto("STAT_QC_CAPABILITY", ggplot2::Stat,
  compute_group = function(data, scales, n=1, USL=USL, LSL=LSL,
                           digits = 1, method = NULL,  show = show,
                           geom.type, direction = direction, type = type,
                           py=NULL, px=NULL){
    print(geom.type)
    print(type)
    if(method=="XmR"){
      QC_Lines <- QC_Lines(data = data$x,method="XmR")
        LCL <- QC_Lines[[1]]
        center <- QC_Lines[[2]]
        UCL <- QC_Lines[[3]]
      QC_CAP <- QC_Capability(data=data$x, LSL=LSL, USL = USL, method="XmR")
    }else{
      QC_Lines <- QC_Lines(data = data, value = "x", grouping = QC.Subgroup,
                           n = 1, method=method)
        LCL <- QC_Lines[[5]]
        center <- QC_Lines[[6]]
        UCL <- QC_Lines[[7]]
      QC_CAP <- QC_Capability(data=data, value="x", grouping = QC.Subgroup,
                    LSL=LSL, USL = USL, method = method)
    }


    # Takes care of vlines and vlabels ----------------------------------------
    if(any(geom.type %in% c("vline", "label")) & direction == "v" & is.na(type) ){
      vlines_df <- data.frame(xintercept = c(LSL,LCL, center, UCL, USL),
                              label = c("LSL", "LCL", "X", "UCL", "USL"),
                              x = c(LSL, LCL, center, UCL, USL),
                              y = c(Inf,Inf,Inf,Inf,Inf),
                              vjust = c(1,1,1,1,1),
                              hjust = c(0.5,0.5,0.5,0.5,0.5)
                              )

      vlines_df <- vlines_df[vlines_df$label %in% show,]
    return(vlines_df)
    }

    if(any(geom.type %in% c("hline", "label")) & direction == "h" & is.na(type)){
      hlines_df <- data.frame(yintercept = c(LSL,LCL, center, UCL, USL),
                              label = c("LSL", "LCL", "X", "UCL", "USL"),
                              y = c(LSL, LCL, center, UCL, USL),
                              x = c(Inf,Inf,Inf,Inf,Inf),
                              vjust = c(1,1,1,1,1),
                              hjust = c(.5,.5,0.5,0.5,0.5)
      )

      hlines_df <- vlines_df[vlines_df$label %in% show,]
    return(vlines_df)
    }
    if(geom.type == "label" & type == "table"){
      QC_CAP[1,1] <- "Tol.(sigma)"
      QC_CAP[2,1] <- "DNS (simga)"
      df <-
        data.frame(
        hjust = 1, vjust=0,
        x = px,
        y = py,
        label=paste0(QC_CAP$label,": " , QC_CAP$values, collapse = "\n")
        )
    #  df <- df[df$label %in% show,]

    return(df)

    }


    # QC_Cap_Table ------------------------------------------------------------

   }
)


#' @export
#' @title Produce QC Charts with ggplot Framework.
#' @description Produce QC charts with ggplot framework. Support for faceting and
#' layering of multiple QC chart lines on a single plot. Charts supported (see method argument for call):
#' \itemize{
#' \item \bold{Individuals Charts}: mR, XmR,
#' \item \bold{Attribute Charts}: c, np, p, u,
#' \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian,
#' \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#'To label chart lines see \link[ggQC]{stat_QC_labels}
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' stripped before the computation proceeds.
#' @param n number, for
#' \itemize{
#'   \item \bold{Studentized Charts}, used for custom or hypothetical subgroup size.
#'   \item \bold{np Charts}, used to specify a fixed area of opportunity.
#' }
#' @param method string, calling the following methods:
#' \itemize{
#'   \item \bold{Individuals Charts}: mR, XmR,
#'   \item \bold{Attribute Charts}: c, np, p, u,
#'   \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#'   \item \bold{Dispersion Charts}: rBar, rMedian, sBar.
#' }
#' @return ggplot control charts.
#' @examples
#'# Load Libraries ----------------------------------------------------------
#'  require(ggQC)
#'  require(ggplot2)
#'
#'# Setup Data --------------------------------------------------------------
#'  set.seed(5555)
#'  Process1 <- data.frame(processID = as.factor(rep(1,100)),
#'                         metric_value = rnorm(100,0,1),
#'                         subgroup_sample = rep(1:20, each=5),
#'                         Process_run_id = 1:100)
#'  set.seed(5556)
#'  Process2 <- data.frame(processID = as.factor(rep(2,100)),
#'                         metric_value = rnorm(100,5, 1),
#'                         subgroup_sample = rep(1:10, each=10),
#'                         Process_run_id = 101:200)
#'
#'  Both_Processes <- rbind(Process1, Process2)
#'
#'#############################
#'#  Example 1:  XmR Chart    #
#'#############################
#'
#'
#' EX1.1 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  geom_point() + geom_line() + stat_QC(method="XmR") +
#'  stat_QC_labels(method="XmR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.1
#'
#' EX1.2 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
#'  stat_mR() + ylab("Moving Range") +
#'  stat_QC_labels(method="mR", digits = 2) +
#'  facet_grid(.~processID, scales = "free_x")
#' #EX1.2
#'
#'#############################
#'#  Example 2:  XbarR Chart  #
#'#############################
# XbarR Plot and rBar Plot ------------------------------------------------
#'
#' EX2.1 <- ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("point")) +
#'  stat_summary(fun.y = "mean", color = "blue", geom = c("line")) +
#'  stat_QC(method = "xBar.rBar") + facet_grid(.~processID, scales = "free_x")
#' #EX2.1
#'
#' EX2.2 <- ggplot(Both_Processes, aes(x = subgroup_sample,
#'                           y = metric_value,
#'                           group = processID)) +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "point") +
#'  stat_summary(fun.y = "QCrange", color = "blue", geom = "line") +
#'  stat_QC(method = "rBar") +
#'  ylab("Range") +
#'  facet_grid(.~processID, scales = "free_x")
#'  #EX2.2
#'

stat_QC_Capability <- function(
                    LSL, USL, method="xBar.rBar",
                    digits=1,
                    mapping = NULL, data = NULL,
                    geom = "vline",
                    position = "identity", na.rm = FALSE,
                    show.legend = NA, inherit.aes = TRUE,
                    show = c("LSL","USL"), direction="v",
                    type = NA,

                    ...) {

#expand <- ggplot2::scale_x_continuous(expand =  ggplot2::expand_scale(mult = 0.5))

#Warning this feature only works with the following methods
  # "xBar.rBar", "xBar.rMedian", "xBar.sBar",
  # "xMedian.rBar", "xMedian.rMedian", "XmR"
#Lines
  SpecLines <- ggplot2::layer(
    stat = STAT_QC_CAPABILITY, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=1,
                  method=method, USL=USL, LSL=LSL,
                  show=show, geom.type = geom,
                  direction = direction, type=type,
                  ...)
  )

#Lables


#Table



return(list(SpecLines))
}

