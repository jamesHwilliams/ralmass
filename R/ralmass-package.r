#' ralmass.
#'
#' @name ralmass
#' @docType package
#' @import stringr
#' @import RColorBrewer
#' @import plyr
#' @import data.table
#' @import sp
#' @import rgdal
#' @import gridExtra
#' @import ggplot2
#' 
#' @useDynLib ralmass
#' @importFrom Rcpp sourceCpp
#' 
.onUnload <- function (libpath) {
  library.dynam.unload("ralmass", libpath)
}
NULL
