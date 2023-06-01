
#'
#' @title Map columns of data
#' @description
#' Maps the required columns in your data and validates them.
#' @param data a data.frame
#' @param dataset_id column, data collection event
#' @param id column for IDs
#' @param age_group column containing age min and age max for each age group
#' @param sex column for sex
#' @param country a string, name of the country
#' @param state column for state
#' @param county column for county
#' @param city column for city
#' @param collection_start_date column for sampling start date
#' @param collection_end_date column for sampling end date
#' @param test_id Will add the list of test later
#' @param result column with test results
#' @param result_cat column with values of "positive", "negative", or "borderline"
#' @param include_others include additional columns or not
#' @return A cleaned data.frame
#' @import dplyr stringr
#' @importFrom dplyr %>%
#' @export
#' @examples
#' library(dplyr)
#' library(stringr)
#' df_orig <- data.frame(dataset_id = 1,
#'                       id = 1:3,
#'                       age = c("0 - 10", "20-30", "30,40"),
#'                       sex = c("f", "Male", "binary"),
#'                       collection_start_date = "2020-03-01",
#'                       collection_end_date = "2020-04-01",
#'                       result = c("'2.4'", "3.5", "9"),
#'                       result_cat = rep("negative", times = 3))
#'
#' map_cols(df_orig,
#'          dataset_id = dataset_id,
#'          id = id,
#'          age_group = age,
#'          sex = sex,
#'          country = "Earth!",
#'          collection_start_date = collection_start_date,
#'          collection_end_date = collection_end_date,
#'          test_id = assays$`SARS-CoV-2`$`EUROIMMUN - IgG - Anti-SARS-CoV-2 ELISA IgG`,
#'          result = result,
#'          result_cat = result_cat,
#'          include_others = FALSE)
#'

map_cols <- function(data,
                     dataset_id = NULL,
                     id = NULL,
                     age_group,
                     sex,
                     country,
                     state = NULL,
                     county = NULL,
                     city = NULL,
                     collection_start_date,
                     collection_end_date,
                     test_id,
                     result,
                     result_cat,
                     include_others = FALSE) {
  stopifnot("Data must be of class 'data.frame'" = "data.frame" %in% class(data))
  # if(is.null({{id}})) {data <- data %>% mutate(id = row_number())}

  fncols <- function(data, cname) {
    add <-cname[!cname%in%names(data)]

    if(length(add)!=0) data[add] <- NA
    data
  }

  data <- data %>%
    dplyr::mutate(
      dataset_id = {{dataset_id}},
      id = {{id}},
      age_group = {{age_group}},
      sex = {{sex}},
      country = {{country}},
      collection_start_date = {{collection_start_date}},
      collection_end_date = {{collection_end_date}},
      test_id = {{test_id}},
      result = {{result}},
      result_cat = {{result_cat}}
    ) %>%
    fncols(c("dataset_id", "id", "state", "county", "city"))

  if(!isTRUE(include_others)) {
    data <- data %>%
      dplyr::select(dataset_id, id, age_group, sex, country, state, county,
                    city, collection_start_date, collection_end_date, test_id,
                    result, result_cat)
  }
  return(data)
}
