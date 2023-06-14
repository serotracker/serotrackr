
#' Sample raw data
#'
#' @description
#' Example dataset to showcase the package's expectations from a raw dataset and the package's workflow to make the raw data ready for submission to SeroTracker 2.0.
#'
#' @format `sample_raw_data`
#' A data frame with 3 rows and 10 columns:
#' \describe{
#'   \item{dataset_id}{ID of each collection period}
#'   \item{id}{Anonimized ID of each participant or sample}
#'   \item{age}{Age}
#'   \item{sex}{Sex}
#'   \item{state}{Administative level 1 region names}
#'   \item{city}{Administative level 2 region names}
#'   \item{collection_start_date}{Collection start date}
#'   \item{collection_end_date}{Collection end date}
#'   \item{result}{Test results}
#'   \item{result_cat}{Interpretation of test results; e.g. positive, negative, or borderline}
#' }
#' @source Fictitious data
"sample_raw_data"
