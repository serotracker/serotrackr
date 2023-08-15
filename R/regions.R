
#' Regions list
#'
#' @description
#' A nested named list of regions at three administrative (ADM) levels. This
#'  list can be used in the `st_validate()` or `st_locate()` functions to
#'  rename geographic areas based on SeroTracker's predefined list of region
#'  names. This will help automate the generation of visualizations and
#'  analyses.
#'
#' @format `regions`
#' A list. Each element of the list returns a unique ID.
#' @details
#' adm0: Use this level to select your study's country.
#'
#' adm1: Use this level to select your study's state/province.
#'
#' adm2: Use this level to select your study's district/municipality or equivalent division.
#'
#' @source Based on the CGAZ dataset from \href{https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/CGAZ}{geoBoundaries}.
#'
#' @examples
#' regions$adm2$Canada$Alberta$Calgary
"regions"
