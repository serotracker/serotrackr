
#' Assays dataframe
#'
#' @description
#' Assay data for different pathogens.
#'
#'
#' @format `assays_df`
#' A data frame with 309 rows and 16 columns:
#' \describe{
#'   \item{pathogen}{Pathogen; currently only SARS-CoV-2 is available}
#'   \item{test_id}{Unique descriptive ID of the test}
#'   \item{test_name}{Test name}
#'   \item{isotype}{Isotype}
#'   \item{test_type}{Test type}
#'   \item{antibody_target}{Antibody target}
#'   \item{rdt_test}{RDT test: a boolean variable}
#'   \item{manufacturer}{Manufacturer}
#'   \item{multiplex_detection}{Multiplex detection: a boolean variable}
#'   \item{spike_antibody_target}{Spike antibody target}
#'   \item{manufacturer_sensitivity}{Manufacturer sensitivity}
#'   \item{manufacturer_specificity}{Manufacturer specificity}
#'   \item{unique_identifier}{Unique identifier}
#'   \item{info_page_url}{Info page URL}
#'   \item{quantitative_qualitative}{Quantitative and/or qualitative}
#'   \item{who_doherty_find_verified}{WHO Doherty find verified}
#' }
#' @source Based on the list of assays gathered by SeroTracker for SARS-CoV-2.
"assays_df"
