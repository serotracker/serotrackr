
#' @title Save as xlsx
#'
#' @param data a cleaned data.frame, output of map_cols()
#' @param path where to save the xlsx file
#'
#' @return Saves data.fram to disk
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#' library(openxlsx)
#'
#' df_orig <- data.frame(dataset_id = 1,
#'                       id = 1:3,
#'                       age = c("0 - 10", "20-30", "30,40"),
#'                       sex = c("f", "Male", "binary"),
#'                       collection_start_date = "2020-03-01",
#'                       collection_end_date = "2020-04-01",
#'                       result = c("'2.4'", "3.5", "9"),
#'                       result_cat = rep("negative", times = 3))
#'
#' df_out <- map_cols(df_orig,
#'                    dataset_id = dataset_id,
#'                    id = id,
#'                    age_group = age,
#'                    sex = sex,
#'                    country = "Earth!",
#'                    collection_start_date = collection_start_date,
#'                    collection_end_date = collection_end_date,
#'                    result = result,
#'                    result_cat = result_cat,
#'                    include_others = FALSE)
#'
#'save_xlsx(df_out, path = "foo.xlsx")
#'
#' @import openxlsx

save_xlsx <- function(data, path) {
  path_template <- system.file("extdata", "Blank Template-Scrubbed v2.0.xlsx",
                               package="serotrackr")
  wb <- openxlsx::loadWorkbook(path_template)
  openxlsx::writeData(wb = wb, sheet = "Data", x = data, startCol = 1,
                      startRow = 3, colNames = FALSE, borders = "none")
  openxlsx::addStyle(wb, sheet = "Data",
                     style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                     rows = 1:(nrow(data)+2), cols = 1:12, gridExpand = TRUE,
                     stack = TRUE)
  openxlsx::addCreator(wb, "SeroTracker")
  openxlsx::setLastModifiedBy(wb, "SeroTracker")
  openxlsx::saveWorkbook(wb = wb, file = path, overwrite = TRUE)
}
