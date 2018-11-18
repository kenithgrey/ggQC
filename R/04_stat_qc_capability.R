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
                           digits = 8, method = NULL,  show = show,
                           geom.type=geom.type, direction = direction, type = type,
                           py=NA, px=NA){

    # print(geom.type)
    #print(digits)
    if(method=="XmR"){
      QC_Lines <- QC_Lines(data = data$x,method="XmR")
        LCL <- QC_Lines[[1]]
        center <- QC_Lines[[2]]
        UCL <- QC_Lines[[3]]
        Sigma <- QC_Lines$sigma

      QC_CAP <- QC_Capability(data=data$x, LSL=LSL, USL = USL,
                              method="XmR", digits = 20)

      #if(!is.na(type)) print(QC_CAP)

    }else{
      QC_Lines <- QC_Lines(data = data, value = "x", grouping = "QC.Subgroup",
                           n = 1, method=method)
        LCL <- QC_Lines[[5]]
        center <- QC_Lines[[6]]
        UCL <- QC_Lines[[7]]
        Sigma <- QC_Lines$sigma

      QC_CAP <- QC_Capability(data=data, value="x", grouping = "QC.Subgroup",
                    LSL=LSL, USL = USL, method = method, digits = 20)
    }

QCM <- data.frame(
        label=c("LCL", "X", "UCL", "Sigma"),
        values=c(LCL, center, UCL, Sigma))

    QC_CAP <- rbind(QC_CAP, QCM)
    QC_CAP <- transform(QC_CAP,
                label = as.character(label),
                values = round(as.numeric(values), digits)
    )


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

      hlines_df <- hlines_df[hlines_df$label %in% show,]
    return(hlines_df)
    }

    #print("hello")
    hyphens <- paste0(rep("-",digits/2), collapse = "")
    header <- paste0("---", hyphens, "[ Capability Data ]", hyphens, "---\n",collapse = "")
    #print(header)
    if(geom.type == "label" & type == "table"){
      QC_CAP[1,1] <- "TOL (sigma)"
      QC_CAP[2,1] <- "DNS (sigma)"
      #print(QC_CAP)
      QC_CAP$ABR <- substr(QC_CAP$label, 1,3)

      if("all" %in% show || "ALL" %in% show){
        #do nothing
      }else{
        QC_CAP <- QC_CAP[QC_CAP$ABR %in% show,]
        QC_CAP <- QC_CAP[match(show, QC_CAP$ABR),]
      }

      capa <- paste0(header, paste0(QC_CAP$label,": " , QC_CAP$values, collapse = "\n"))
      #print(capa)
      #print(QC_CAP)
      #print(show)
      df <-
        data.frame(
        hjust = 1, vjust=0,
        x = px,
        y = py,
        label=capa
        )
    #print(df)
    #  df <- df[df$label %in% show,]

    return(df)

    }
   }
)


#' @title Generic Function for drawing QC capability information on plots
#' @description Generic Function for drawing QC capability information on plots
#' @param LSL numeric, Customer's lower specification limit
#' @param USL numeric, Customer's Upper specification limit
#' @param method string, calling the following methods:
#' \itemize{
#'   \item \bold{Individuals Charts}: XmR,
#'   \item \bold{Studentized Charts}: xBar.rBar, xBar.rMedian, xBar.sBar, xMedian.rBar,
#' xMedian.rMedian
#' }
#' @return ggplot control charts.
#' @examples
#' # Setup Data --------------------------------------------------------------
#' set.seed(5555)
#' Process1 <- data.frame(ProcessID = as.factor(rep(1,100)),
#'                        Value = rnorm(100,10,1),
#'                        Subgroup = rep(1:20, each=5),
#'                        Process_run_id = 1:100)
#' set.seed(5556)
#' Process2 <- data.frame(ProcessID = as.factor(rep(2,100)),
#'                        Value = rnorm(100,20, 1),
#'                        Subgroup = rep(1:10, each=10),
#'                        Process_run_id = 101:200)
#'
#' df <- rbind(Process1, Process2)
#'
#' ######################
#' ## Example 1 XmR    ##
#' ######################
#' ##You may need to use the r-studio Zoom for these plots or make the size of the
#' ##stat_QC_cap_summary smaller with size = some number"
#'
#' method <- "XmR"
#'
#' # Normal Histogram XmR --------------------------------------------------------
#'
#' EX1.1 <-  ggplot(df[df$ProcessID == 1,], aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_histogram(binwidth = 1, color="purple") +
#'   geom_hline(yintercept=0, color="grey") +
#'   stat_QC_cap_vlines(LSL = 5, USL = 15, show=c("X", "LSL", "USL"), method=method) +
#'   stat_QC_cap_vlabels(LSL = 5, USL = 15, show=c("X", "LSL", "USL"), method=method) +
#'   stat_QC_cap_summary(LSL = 5, USL = 15, method=method,
#'                       #show="ALL",
#'                       #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                       #show=c("Sig","TOL", "DNS"),
#'                       show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                       color="black", digits=2, size=4) +
#'   scale_x_continuous(expand =  expand_scale(mult = c(0.15,.8))) +
#'   ylim(0,45)
#' #Ex1.1
#'
#' # Facet Histogram XmR -----------------------------------------------------
#'
#' EX1.2 <- ggplot(df[order(df$Process_run_id),], aes(x=Value, QC.Subgroup=Subgroup, color=ProcessID)) +
#'   geom_histogram(binwidth = 1) +
#'   geom_hline(yintercept=0, color="grey") +
#'   stat_QC_cap_vlines(LSL = 5, USL = 15, method=method) +
#'   stat_QC_cap_vlabels(LSL = 5, USL = 15, method=method) +
#'   stat_QC_cap_summary(LSL = 5, USL = 15, method=method,
#'                       #show="ALL",
#'                       #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                       #show=c("Sig","TOL", "DNS"),
#'                       show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                       color="black", digits=4, size=4) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8))) +
#'   facet_grid(.~ProcessID) + ylim(0,45)
#' #EX1.2
#'
#' # Facet Density Plot XmR -------------------------------------------------
#'
#' EX1.3 <- ggplot(df[df$ProcessID == 1,], aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_density(bw = .4, fill="purple", trim=TRUE) +
#'   geom_hline(yintercept=0, color="grey") +
#'   stat_QC_cap_vlines(LSL = 5, USL = 15, show=c("X", "LSL", "USL"), method=method) +
#'   stat_QC_cap_vlabels(LSL = 5, USL = 15, show=c("X", "LSL", "USL"), method=method) +
#'   stat_QC_cap_summary(LSL = 5, USL = 15, method=method,
#'                       #show="ALL",
#'                       #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                       #show=c("Sig","TOL", "DNS"),
#'                       show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                       color="black", digits=2, size=4) +
#'
#'   scale_x_continuous(expand =  expand_scale(mult = c(0.15,.8)))  + ylim(0,.5)
#' #EX1.3
#'
#' # Facet Density Plot XmR --------------------------------------------------
#'
#' EX1.4 <- ggplot(df[order(df$Process_run_id),], aes(x=Value, QC.Subgroup=Subgroup, color=ProcessID)) +
#'   geom_density(bw = .4, fill="grey", trim=TRUE ) +
#'   stat_QC_cap_vlines(LSL = 5, USL = 15, method=method) +
#'   stat_QC_cap_vlabels(LSL = 5, USL = 15, method=method) +
#'   stat_QC_cap_summary(LSL = 5, USL = 15, method=method, #py=.3,
#'                       #show="ALL",
#'                       #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                       #show=c("Sig","TOL", "DNS"),
#'                       show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                       color="black", digits=4, size=4) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8))) +
#'   # geom_hline(yintercept=0, color="black") +
#'   facet_grid(.~ProcessID) + ylim(0,.5)
#' #EX1.4
#'
#'
#' ########################################
#' ##  Example 2: xBar.rBar or xBar.sBar ##
#' ########################################
#'
#' method <- "xBar.rBar" #Alternativly Use "xBar.sBar" if desired
#'
#'
#' # Single Histogram xBar.rBar ----------------------------------------------
#'
#' EX2.1 <- ggplot(df[df$ProcessID==1,], aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_histogram(binwidth = 1) +
#'   stat_QC_Capability(LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(geom="label", LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(px=Inf, py=0, LSL = 5, USL = 15, geom="label",
#'                      type="table", method=method,
#'                      #show="ALL",
#'                      #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                      #show=c("Sig","TOL", "DNS"),
#'                      show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                      size=3.5) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8))) #+
#' #EX2.1
#'
#'
#' # Faceted Histogram xBar.rBar ---------------------------------------------
#'
#' EX2.2 <- ggplot(df, aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_histogram(binwidth = 1) +
#'   stat_QC_Capability(LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(geom="label", LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(px=Inf, py=0, LSL = 5, USL = 15, geom="label",
#'                      type="table", method=method,
#'                      #show="ALL",
#'                      #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                      #show=c("Sig","TOL", "DNS"),
#'                      show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                      size=3.5) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8)))+
#'   facet_grid(.~ProcessID, scales="free_x")
#' #EX2.2
#'
#' # Single Density xBar.rBar ----------------------------------------------
#'
#' EX2.3 <- ggplot(df[df$ProcessID==1,], aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_density(bw = .4, fill="grey", alpha=.4) +
#'   stat_QC_Capability(LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(geom="label", LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(px=Inf, py=0, LSL = 5, USL = 15, geom="label",
#'                      type="table", method=method,
#'                      #show="ALL",
#'                      #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                      #show=c("Sig","TOL", "DNS"),
#'                      show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                      size=3.5) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8))) #+
#' #EX2.3
#'
#' # Faceted Density xBar.rBar ---------------------------------------------
#'
#' EX2.4 <-  ggplot(df, aes(x=Value, QC.Subgroup=Subgroup)) +
#'   geom_density(bw = .4, fill="grey", alpha=.4) +
#'   stat_QC_Capability(LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(geom="label", LSL = 5, USL = 15, show = c("LSL", "USL"), method=method) +
#'   stat_QC_Capability(px=Inf, py=0, LSL = 5, USL = 15, geom="label",
#'                      type="table", method=method,
#'                      #show="ALL",
#'                      #show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig")
#'                      #show=c("Sig","TOL", "DNS"),
#'                      show=c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk"),
#'                    size=3.5) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8)))+
#'   facet_grid(.~ProcessID, scales="free_x")
#' #EX2.4
#'
#'
#' ###############################
#' ##  Example 3: xBar.rMedian  ##
#' ###############################
#'
#' ## Plots involving medians should give warning: "median based QC methods represent
#' ## at best *potential* process capability"
#'
#' ##These plot work the same as in examples 2.X; below is an example.
#'
#' method <- "xBar.rMedian"
#' EX3.1 <- ggplot(df[order(df$Run),], aes(x=Value, QC.Subgroup=Run)) +
#'   geom_histogram(binwidth = 1) +
#'   stat_QC_Capability(LSL = 0, USL = 15, show = c("LCL", "UCL"), method=method) +
#'   stat_QC_Capability(geom="label", LSL = 0, USL = 15, show = c("LCL", "UCL"), method=method) +
#'   stat_QC_Capability(px=Inf, py=0, LSL = 0, USL = 15, geom="label", type="table",
#'                      show=c("TOL", "DNS", "Sig"),  size=3.5, method=method) +
#'   scale_x_continuous(expand =  ggplot2::expand_scale(mult = c(0.15,.8)))
#'#EX3.1

stat_QC_Capability <- function(
                    LSL, USL, method="xBar.rBar",
                    digits=1,
                    mapping = NULL, data = NULL,
                    geom = "vline",
                    position = "identity", na.rm = FALSE,
                    show.legend = NA, inherit.aes = TRUE,
                    show = c("LSL","USL"), direction="v",
                    type = NA, ...) {

if(!method %in%  c("xBar.rBar", "xBar.rMedian", "xBar.sBar",
                   "xMedian.rBar", "xMedian.rMedian", "XmR")){
  stop("Error: This feature only works with the following methods
        xBar.rBar, xBar.rMedian, xBar.sBar xMedian.rBar, xMedian.rMedian, XmR")
}

if(method %in% c("xBar.rMedian", "xMedian.rBar", "xMedian.rMedian")){
  warning("median based QC methods represent at best *potential* process capability", call. = F)
}

#Lines, #Lables #Table
ggplot2::layer(
    stat = STAT_QC_CAPABILITY, data = data, mapping = mapping,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n=1, digits=digits,
                  method=method, USL=USL, LSL=LSL,
                  show=show, geom.type = geom,
                  direction = direction, type=type,
                  ...)
  )




}

#' @export
#' @title Vertical Line Capability Stat
#' @description Draws Vertical Capability Stats
#' @inheritParams stat_QC_Capability
#' @param show vector, indicating which lines to draw ie., c("LCL", "LSL", "X", "USL", "UCL")
#' \itemize{
#'   \item \bold{LCL}: Lower Control Limit
#'   \item \bold{LSL}: Lower Specification Limit
#'   \item \bold{X}: Process Center
#'   \item \bold{USL}: Upper Specification Limit
#'   \item \bold{UCL}: Upper Control Limit
#' }
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' @return vertical lines for histogram and density plots.
#' @inherit stat_QC_Capability examples
#'
stat_QC_cap_vlines <- function(
  LSL, USL, method="xBar.rBar",
  show = c("LSL","USL"),
  mapping = NULL, data = NULL,
  inherit.aes = TRUE, ...){

  stat_QC_Capability(
    LSL=LSL, USL=USL, method=method,
    mapping = mapping, data = data,
    geom = "vline",
    position = "identity", na.rm = FALSE,
    show.legend = NA, inherit.aes = inherit.aes,
    show = show, direction="v",
    type = NA, ...)

}

#' @export
#' @title Vertical Label Capability Stat
#' @description Draws Vertical Lables on Vertical Capability lines
#' @inheritParams stat_QC_Capability
#' @param show vector, indicating which lines to draw ie., c("LCL", "LSL", "X", "USL", "UCL")
#' \itemize{
#'   \item \bold{LCL}: Lower Control Limit
#'   \item \bold{LSL}: Lower Specification Limit
#'   \item \bold{X}: Process Center
#'   \item \bold{USL}: Upper Specification Limit
#'   \item \bold{UCL}: Upper Control Limit
#' }
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' @return vertical lines for histogram and density plots.
#' @inherit stat_QC_Capability examples
#'
stat_QC_cap_vlabels <- function(
  LSL, USL, method="xBar.rBar",
  show = c("LSL","USL"),
  mapping = NULL, data = NULL,
  inherit.aes = TRUE, ...){

  stat_QC_Capability(
    LSL=LSL, USL=USL, method=method,
    mapping = mapping, data = data,
    geom = "label",
    position = "identity", na.rm = FALSE,
    show.legend = NA, inherit.aes = inherit.aes,
    show = show, direction="v",
    type = NA, ...)

}

#' @export
#' @title horizontal Line Capability Stat
#' @description Draws horizontal Capability Lines
#' @inheritParams stat_QC_Capability
#' @param show vector, indicating which lines to draw ie., c("LCL", "LSL", "X", "USL", "UCL")
#' \itemize{
#'   \item \bold{LCL}: Lower Control Limit
#'   \item \bold{LSL}: Lower Specification Limit
#'   \item \bold{X}: Process Center
#'   \item \bold{USL}: Upper Specification Limit
#'   \item \bold{UCL}: Upper Control Limit
#' }
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' @return horizontal lines for histogram and density plots.
#' @inherit stat_QC_Capability examples
#'
stat_QC_cap_hlines <- function(
  LSL, USL, method="xBar.rBar",
  show = c("LSL","USL"),
  mapping = NULL, data = NULL,
  inherit.aes = TRUE, ...){

  stat_QC_Capability(
    LSL=LSL, USL=USL, method=method,
    mapping = mapping, data = data,
    geom = "vline",
    position = "identity", na.rm = FALSE,
    show.legend = NA, inherit.aes = inherit.aes,
    show = show, direction="h",
    type = NA, ...)

}

#' @export
#' @title horizontal Label Capability Stat
#' @description Draws horizontal Lables on horizontal Capability lines
#' @inheritParams stat_QC_Capability
#' @param show vector, indicating which lines to draw ie., c("LCL", "LSL", "X", "USL", "UCL")
#' \itemize{
#'   \item \bold{LCL}: Lower Control Limit
#'   \item \bold{LSL}: Lower Specification Limit
#'   \item \bold{X}: Process Center
#'   \item \bold{USL}: Upper Specification Limit
#'   \item \bold{UCL}: Upper Control Limit
#' }
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' @return horizontal lines for histogram and density plots.
#' @inherit stat_QC_Capability examples
#'
stat_QC_cap_hlabels <- function(
  LSL, USL, method="xBar.rBar",
  show = c("LSL","USL"),
  mapping = NULL, data = NULL,
  inherit.aes = TRUE, ...){

  stat_QC_Capability(
    LSL=LSL, USL=USL, method=method,
    mapping = mapping, data = data,
    geom = "label",
    position = "identity", na.rm = FALSE,
    show.legend = NA, inherit.aes = inherit.aes,
    show = show, direction="h",
    type = NA, ...)

}

#' @export
#' @title horizontal Label Capability Stat
#' @description Draws horizontal Lables on horizontal Capability lines
#' @inheritParams stat_QC_Capability
#' @param show vector, indicating which lines to draw ie.,
#' c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig").
#' The order given in the vector is the order presented in the graph.
#' \itemize{
#'   \item \bold{TOL}: Tolerance in Sigma Units  (USL-LSL)/sigma
#'   \item \bold{DNS}: Distance to Nearest Specification Limit in Simga Units
#'   \item \bold{Cp}: Cp (Within)
#'   \item \bold{Cpk}: Cpk (Within)
#'   \item \bold{Pp}: Pp (Between)
#'   \item \bold{Ppk}: Ppk (Between)
#'   \item \bold{LCL}: Lower Control Limit
#'   \item \bold{X}: Process Center
#'   \item \bold{UCL}: Upper Control Limit
#'   \item \bold{Sig}: Sigma from control charts
#' }
#' @param px numeric, x position for table. Use Inf to force label to x-limit.
#' @param py numeric, y position for table. Use Inf to force label to y-limits. May also need vjust parameter.
#' @inheritParams ggplot2::stat_identity
#' @param na.rm a logical value indicating whether NA values should be
#' @return horizontal lines for histogram and density plots.
#' @inherit stat_QC_Capability examples
stat_QC_cap_summary <- function(
  LSL, USL, method="xBar.rBar", px=Inf, py= -Inf,
  show = c("TOL","DNS", "Cp", "Cpk", "Pp", "Ppk", "LCL", "X", "UCL", "Sig"),
  digits=8,
  mapping = NULL, data = NULL,
  inherit.aes = TRUE, ...){

  stat_QC_Capability(
    LSL=LSL, USL=USL, method=method,
    digits=digits,
    mapping = mapping, data = data,
    geom = "label",
    position = "identity", na.rm = FALSE,
    show.legend = NA, inherit.aes = inherit.aes,
    show = show, direction=NA,
    type = "table", px=px, py=py, ...)

}

