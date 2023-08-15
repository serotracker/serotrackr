
#' Regions dataframe
#'
#' @description
#' Regions data at three administrative levels
#'
#' @format `regions_df`
#' A data frame with 53087 rows and 6 columns:
#' \describe{
#'   \item{shapeGroup}{ISO 3166 code for each country}
#'   \item{shapeType}{Aministrative (ADM) level of the region. ADM0: country; ADM1: state/province; ADM2: district/municipality/or equivalent }
#'   \item{NAME_0}{Country name of the region}
#'   \item{NAME_1}{State/province name of the region}
#'   \item{NAME_2}{District/municipality/ or equivalent name of the region}
#'   \item{shapeID_v5}{Unique ID for the region and administartive level}
#' }
#' @source Based on the CGAZ dataset from \href{https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/CGAZ}{geoBoundaries}.
"regions_df"
