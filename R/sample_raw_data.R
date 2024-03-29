
#' Example fictitious data
#'
#' @description
#' Example dataset to showcase the package's expectations from a raw dataset and the package's workflow to make the raw data ready for submission to SeroTracker 2.0.
#'
#' @format `sample_raw_data`
#' A data frame with 100 rows and 13 columns:
#' \describe{
#'   \item{dataset_id}{ID of each collection period}
#'   \item{id}{Anonimized ID of each participant or sample}
#'   \item{age_group}{Age group}
#'   \item{age}{Age}
#'   \item{sex}{Sex}
#'   \item{country}{Administative level 0 (country) region names}
#'   \item{state}{Administative level 1 (state/province) region names}
#'   \item{city}{Administative level 2 (district/municipalities) region names}
#'   \item{start_date}{Collection start date}
#'   \item{end_date}{Collection end date}
#'   \item{test_id}{Test ID}
#'   \item{result}{Test results}
#'   \item{result_cat}{Interpretation of test results; e.g. positive, negative, or borderline}
#' }
#' @source Fictitious data
"sample_raw_data"
