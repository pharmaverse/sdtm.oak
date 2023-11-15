#' An EDC and Data Standard agnostic SDTM data transformation engine that automates
#' the transformation of raw clinical data in ODM format to SDTM based on standard
#' mapping algorithms
#'
#' @name sdtm.oak
#'
#' @import rlang
#' @importFrom tibble tibble
NULL

#' onLoad function
#'
#' This function is called automatically during package loading.
#'
#' @param libname lib name
#' @param pkgname package name
#' @noRd
.onLoad <- function(libname, pkgname) { # nolint
}

#' Temporary dummy function
#' @noRd
dummy <- function() {
  rlang::as_list
}
