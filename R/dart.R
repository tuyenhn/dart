#' dart: under construction
#'
#' This package is under construction
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
