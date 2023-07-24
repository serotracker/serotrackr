
# TODO add colnames for `other columns` in Data sheet.
# TODO automate scope on Study Metadata sheet.
# TODO check for validation rules.
# TODO add adm0 unique code somewhere (not just country name).
# TODO check data and estimates have the same object ID.

#' @title Save to xlsx format
#' @description
#' `r lifecycle::badge("experimental")`
#' Organizes individual level data and aggregate estimates into a single xlsx
#'  file to be uploaded on the \href{https://serotracker.com}{SeroTracker} website.
#' @param data A validated data.frame, output of `st_validate()`
#' @param estimates a data.frame, output of `st_aggregate()`
#' @param path where to save the xlsx file
#'
#' @return An xlsx file saved to disc
#' @export
#'
#' @examples
#' new_raw_data <- dplyr::mutate(
#'   sample_raw_data,
#'   age_group = rep(c("0-9", "10-19", "20-29", "30-39", "40+"), each=20),
#'   age = c(sample(0:9, 20, replace=TRUE), sample(10:19, 20, replace=TRUE),
#'           sample(20:29, 20, replace=TRUE), sample(30:39, 20, replace=TRUE),
#'           sample(40:120, 20, replace=TRUE)),
#'   sex = sample(c(rep("f", 40), rep("m", 40), rep("o", 20))),
#'   result_cat = dplyr::case_when(result_cat == "neg" ~ "negative",
#'                                 result_cat == "pos" ~ "positive",
#'                                 TRUE ~ result_cat)
#' )
#'
#'
#' validated_df <- st_validate(
#'   new_raw_data,
#'   dataset_id = dataset_id,
#'   id = id,
#'   age_group = age_group,
#'   age = age,
#'   sex = sex,
#'   adm0 = regions$adm0$Canada,
#'   adm1 = regions$adm1$Canada$Alberta,
#'   adm2 = regions$adm2$Canada$Alberta$Calgary,
#'   collection_start_date = "2023-01-01",
#'   collection_end_date = "2023-02-01",
#'   test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
#'   result = result,
#'   result_cat = result_cat,
#'   include_others = TRUE
#' )
#'
#' estimates <- st_aggregate(validated_df)
#'
#' st_save(validated_df, estimates, path = tempdir())
#'

st_save <- function(data, estimates, path) {

  path_template <- system.file("extdata", "Blank_Template_Scrubbed_v2.0.xlsx",
                               package="serotrackr")
  wb <- openxlsx::loadWorkbook(path_template)

  ## data sheet --------------------------------------------------------------

  preset_colnames <- c("dataset_id", "id", "age_group", "sex", "adm1", "adm2",
                       "collection_start_date", "collection_end_date",
                       "test_id", "result", "result_cat")
  # Make an NA column for any of optional columns that are missing
  data_sheet <- add_missing_cols(data, preset_colnames)
  data_sheet <- dplyr::select(
    data_sheet,
    !!!dplyr::any_of(c(preset_colnames, setdiff(names(data), preset_colnames)))
  )
  openxlsx::writeData(wb = wb, sheet = "Data", x = data_sheet, startCol = 1,
                      startRow = 3, colNames = FALSE, borders = "none")
  openxlsx::addStyle(wb, sheet = "Data",
                     style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                     rows = 1:(nrow(data_sheet)+2), cols = 1:12, gridExpand = TRUE,
                     stack = TRUE)

  ## estimates sheet ---------------------------------------------------------

  openxlsx::writeData(wb = wb, sheet = "Estimates", x = estimates, startCol = 1,
                      startRow = 3, colNames = FALSE, borders = "none")
  openxlsx::addStyle(wb, sheet = "Estimates",
                     style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                     rows = 1:(nrow(estimates)+2), cols = 1:28, gridExpand = TRUE,
                     stack = TRUE)

  ## assays sheet ------------------------------------------------------------

  test_id_used <- unique(data$test_id)
  assays_used <- dplyr::filter(assays_df, test_id %in% test_id_used)
  assays_sheet <- data.frame(
    n_tests = c(length(test_id_used),
                rep(NA_integer_, times = length(test_id_used)-1)),
    commercial = NA,
    test_id = test_id_used,
    cutoff_value = NA,
    cutoff_unit = NA,
    sensitivity = assays_used$manufacturer_sensitivity,
    specificity = assays_used$manufacturer_specificity,
    test_validation = NA,
    specimen_type = NA,
    test_type = assays_used$test_type,
    isotype = assays_used$isotype,
    isotype_comb = NA,
    ab_target = assays_used$antibody_target,
    notes = NA
  )

  openxlsx::writeData(wb = wb, sheet = "Immunoassays", x = assays_sheet,
                      startCol = 1, startRow = 4, colNames = FALSE,
                      borders = "none")
  openxlsx::addStyle(wb, sheet = "Immunoassays",
                     style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                     rows = 1:(nrow(assays_sheet)+3), cols = 1:14,
                     gridExpand = TRUE, stack = TRUE)

  ## age sheet ---------------------------------------------------------------

  age_sheet <- data.frame(age_min = NA, age_max = NA, age_group = NA)
  if ("age_group" %in% names(data)) {
    data_grouped <- dplyr::group_by(data, age_group)
    if ("age" %in% names(data)) {
      age_sheet <- dplyr::summarise(data_grouped, age_min = min(age),
                                    age_max = max(age))
    } else {
      age_sheet <- data.frame(age_group = unique(data$age_group))
      age_sheet <- dplyr::mutate(
        age_sheet,
        age_min = as.numeric(stringr::str_extract(age_group,"^.*(?=((\\-|\\+)))")),
        age_max = as.numeric(stringr::str_extract(age_group, "(?<=\\-).*$"))
      )
    }
    age_sheet <- dplyr::select(age_sheet, age_min, age_max, age_group)
  }

  openxlsx::writeData(wb = wb, sheet = "Age Groups", x = age_sheet,
                      startCol = 2, startRow = 2, colNames = FALSE,
                      borders = "none")
  openxlsx::addStyle(wb, sheet = "Age Groups",
                     style = openxlsx::createStyle(border = "TopBottomLeftRight"),
                     rows = 1:(nrow(age_sheet)+1), cols = 1:4,
                     gridExpand = TRUE, stack = TRUE)

  ## study sheet -------------------------------------------------------------

  pathogen <- attr(data, "pathogen")
  adm0 <- attr(data, "adm0")
  country <- regions_df$NAME_0[regions_df$shapeID_v5 == adm0]

  # Determine age_min and age_max
  if ("age" %in% names(data)) {
    age_min_study <- min(data$age); age_max_study <- max(data$age)
  } else if ("age_group" %in% names(data)) {
    age_min_study <- as.numeric(stringr::str_extract(data$age_group,
                                                     "^.*(?=((\\-|\\+)))"))
    if (any(grepl("\\+", unique(data$age_group)))) {
      age_max_study <- NA
    } else {
      age_max_study = as.numeric(stringr::str_extract(data$age_group,
                                                      "(?<=\\-).*$"))
    }
  } else {age_min_study <- NA; age_max_study <- NA}

  # Determine sex
  if ("sex" %in% names(data)) {
    len_sex <- length(unique(stats::na.omit(data$sex)))
    if (len_sex == 0) {sex_study <- NA}
    if (len_sex == 1) {sex_study <- unique(stats::na.omit(data$sex))}
    if (len_sex > 1) {sex_study <- "All"}
  } else {sex_study <- NA}

  study_sheet <- data.frame(
    country = country,
    n_datasets = length(unique(data$dataset_id)),
    V1 = NA,
    pathogen = pathogen,
    scope = find_scope(data_sheet),
    V2 = NA, V3 = NA, V4 = NA, V5 = NA, V6 = NA,
    age_min = age_min_study,
    age_max = age_max_study,
    sex = sex_study,
    V7 = NA, V8 = NA,
    start_year = lubridate::year(min(data$collection_start_date)),
    end_year = lubridate::year(max(data$collection_end_date)),
    V9 = NA, V10 = NA,
    number_people = nrow(data)
  )

  openxlsx::writeData(wb = wb, sheet = "Study Metadata", x = study_sheet,
                      startCol = 9, startRow = 3, colNames = FALSE,
                      borders = "none")

  ## save wb -----------------------------------------------------------------

  openxlsx::addCreator(wb, "SeroTracker")
  openxlsx::setLastModifiedBy(wb, "SeroTracker")
  openxlsx::saveWorkbook(wb = wb, file = path, overwrite = TRUE)
}



# Helper functions --------------------------------------------------------

add_missing_cols <- function(data, colname) {
  cols_to_add <- colname[!colname %in% names(data)]
  if(length(cols_to_add) != 0) data[cols_to_add] <- NA
  data
}


find_scope <- function(data) {
  uniq_adm1 <- unique(data$adm1)
  uniq_adm2 <- unique(data$adm2)
  len_adm1 <- length(uniq_adm1[!is.na(uniq_adm1)])
  len_adm2 <- length(uniq_adm2[!is.na(uniq_adm2)])
  national <- "Country/Territory Wide"
  regional <- "Regional (province or state-level)"
  local    <- "Local (county, municipality, city)"

  if (len_adm1 == 0) {
    if (len_adm2 == 0) {scope <- national} else {scope <- local}
  }
  if (len_adm1 == 1) {
    if (len_adm2 %in% c(1, 2)) {scope <- local} else {scope <- regional}
  }
  if (len_adm1 == 2) {scope <- regional}
  if (len_adm1 > 2) {scope <- national}
  scope
}
