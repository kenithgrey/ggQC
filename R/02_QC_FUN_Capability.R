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

#' @export
#' @title Calculate QC Process Tolerance
#' @description function to calculate a standardized process tolerance with sigma unit
#' @param LSL number, customer's lower specification limit.
#' @param USL number, customer's upper specification limit.
#' @param QC.Sigma number, the sigma value determined from an XmR plot or a Studentized (e.g., xBar) analysis.
#' @return numeric, standardized process tolerance value in sigma units
process_tolerance <- function(LSL, USL, QC.Sigma){
  (USL-LSL)/QC.Sigma
}

#' @export
#' @title Calculate Distance to Nearest Specification Limit
#' @description function to calculate a standardized distance to the nearest specification limit (sigma units)
#' @param LSL number, customer's lower specification limit.
#' @param USL number, customer's upper specification limit.
#' @param QC.Center number, the mean or median value determined from an XmR plot or a Studentized (e.g., xBar) analysis.
#' @param QC.Sigma number, the sigma value determined from an XmR plot or a Studentized (e.g., xBar) analysis.
#' @return numeric, standardized distance to the nearest specification limit (sigma units)
DNS <- function(LSL, USL, QC.Center, QC.Sigma){
  UpperDist <- (USL-QC.Center)/QC.Sigma
  LowerDist <- (QC.Center-LSL)/QC.Sigma
  min(c(UpperDist,LowerDist))
}

#' @export
#' @title Calculate Distance to Upper Specification Limit
#' @description function to calculate a standardized distance to the Upper specification limit (sigma units)
#' @inheritParams DNS
#' @return numeric, standardized distance to the upper specification limit (sigma units)
UD <- function(LSL, USL, QC.Center, QC.Sigma){(USL-QC.Center)/QC.Sigma}

#' @export
#' @title Calculate Distance to Lower Specification Limit
#' @description function to calculate a standardized distance to the Lower specification limit (sigma units)
#' @inheritParams DNS
#' @return numeric, standardized distance to the lower specification limit (sigma units)
LD <- function(LSL, USL, QC.Center, QC.Sigma){(QC.Center-LSL)/QC.Sigma}


#' @export
#' @title Calculate Cp
#' @description function to calculate Cp - "The elbowroom or margin your process"
#' @inheritParams process_tolerance
#' @return numeric, Cp value (unitless)
Cp <- function(LSL, USL, QC.Sigma){
  (USL-LSL)/(6*QC.Sigma)
}

#' @export
#' @title Calculate Cpk
#' @description function to calculate Cpk - "measure of process centering"
#' @inheritParams DNS
#' @return numeric, Cpk value (unitless)
Cpk <- function(LSL, USL, QC.Center, QC.Sigma){
  UpperDist <- (USL-QC.Center)/QC.Sigma
  LowerDist <- (QC.Center-LSL)/QC.Sigma
  min(c(UpperDist,LowerDist)/3)
}

#' @export
#' @title Calculate Pp
#' @description function to calculate Pp - "The elbowroom or margin your process" uses overall sigma value not the QC chart sigma values.
#' @param LSL number, customer's lower specification limit.
#' @param USL number, customer's upper specification limit.
#' @param s.Sigma number, the sigma value determined from overall standard deviation (i.e., sd()).
#' @return numeric, Pp value (unitless)
Pp <-function(LSL, USL, s.Sigma){
  (USL-LSL)/(6*s.Sigma)
}

#' @export
#' @title Calculate Cpk
#' @description function to calculate Cpk - "measure of process centering"
#' @param LSL number, customer's lower specification limit.
#' @param USL number, customer's upper specification limit.
#' @param QC.Center number, the mean or median value determined from an XmR plot or a Studentized (e.g., xBar) analysis.
#' @param s.Sigma number, the sigma value determined from overall standard deviation (i.e., sd()).
#' @return numeric, Ppk value (unitless)
Ppk <- function(LSL, USL, QC.Center, s.Sigma){
  UpperDist <- (USL-QC.Center)/s.Sigma
  LowerDist <- (QC.Center-LSL)/s.Sigma
  min(c(UpperDist,LowerDist)/3)
}

