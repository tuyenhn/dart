#' dart: under construction
#'
#' This package provides functions that performs raster spatial analysis and
#' processing to be used in the DART (Dengue Advanced Readiness Tools) project
#' by OUCRU, in collaboration with RMIT University Vietnam. The main function of
#' this package calculates the average of raster pixel values in a buffer around
#' a geometric feature (i.e. Average rainfall of a commune/ward in a city)
#'
#' @docType package
#' @name dart
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom units as_units drop_units
#' @importFrom dplyr mutate rename
#'
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
