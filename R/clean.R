
#'
#' @title Clean the data
#' @description
#' Cleans and validates the main columns used during submission to the serotracker platform
#' @param data a data.frame
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
#' mapped_cols <- map_cols(
#'   df_orig,
#'   dataset_id = dataset_id,
#'   id = id,
#'   age_group = age,
#'   sex = sex,
#'   country = "Earth!",
#'   collection_start_date = collection_start_date,
#'   collection_end_date = collection_end_date,
#'   test_id = assays$`SARS-CoV-2`$`EUROIMMUN - IgG - Anti-SARS-CoV-2 ELISA IgG`,
#'   result = result,
#'   result_cat = result_cat,
#'   include_others = FALSE
#' )
#'
#' clean(mapped_cols)

# clean <- function(data) {
#   stopifnot("Data must be of class 'data.frame'" = "data.frame" %in% class(data))
#   # if(is.null({{id}})) {data <- data %>% mutate(id = row_number())}
#
#   data <- data %>%
#     dplyr::mutate(
#       age_min = stringr::str_extract(age_group, "\\d+"),
#       age_max = stringr::str_extract(age_group, "(\\d+)(?!.*\\d)"),
#       age_group = paste0(age_min, "-", age_max),
#       sex = dplyr::case_when(
#         grepl("f|female|woman|women", sex, ignore.case = TRUE) ~ "female",
#         grepl("m|male|man|men", sex, ignore.case = TRUE) ~ "male",
#         grepl("^na$|^n/a$", sex, ignore.case = TRUE) | is.na(sex) ~ NA,
#         TRUE ~ "other"
#       ),
#       dplyr::across(dplyr::ends_with("date"), ~ as.Date(.)),
#       result = as.numeric(stringr::str_extract(result, "\\d+\\.*\\d*"))
#     ) %>%
#     dplyr::select(dataset_id, id, age_group, sex, country, state, county,
#                   city, collection_start_date, collection_end_date, test_id,
#                   result, result_cat)
#
#   return(data)
# }
