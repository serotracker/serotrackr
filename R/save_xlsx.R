
#' @title Save as xlsx
#'
#' @param data a validated data.frame, output of st_validate()
#' @param path where to save the xlsx file
#'
#' @return Saves data.fram to disk
#' @export
#'
#' @examples
#' validated_df <- st_validate(
#'   sample_raw_data,
#'   dataset_id = dataset_id,
#'   id = id,
#'   age_group = "12-17",
#'   sex = "m",
#'   adm0 = regions$adm0$Canada,
#'   adm1 = regions$adm1$Canada$Alberta,
#'   adm2 = regions$adm2$Canada$Alberta$Calgary,
#'   collection_start_date = "2023-01-01",
#'   collection_end_date = "2023-02-01",
#'   test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
#'   result = result,
#'   result_cat = "negative",
#'   include_others = TRUE
#' )
#'
#' save_xlsx(validated_df, path = tempdir())
#'
#' @import openxlsx


save_xlsx <- function(data, path) {
  data <- data[, names(data) != "country"]
  path_template <- system.file("extdata", "Blank_Template_Scrubbed_v2.0.xlsx",
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
