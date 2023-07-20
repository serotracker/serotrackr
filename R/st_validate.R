
#'
#' @title Validate individual level data
#' @description
#' `r lifecycle::badge("experimental")`
#' Locates the required and optional columns in your data and validates them.
#' @param data a data.frame.
#' @param dataset_id An unquoted column name or a length-one vector that
#'  differentiates the data collection event(s).
#' @param id column for anonymized individual level IDs. This column will be
#'  used to generate aggregate estimates.
#' @param adm0,adm1,adm2 a string or an unquoted name of a character column that
#'  contains the country (adm0), state/province (adm1), or district/municipality
#'  (adm2) codes. Use serotrackr::regions to select these. Only one adm0 is
#'  acceptable. adm1 and adm2 can be more.
#' @param collection_start_date,collection_end_date Unquoted name of a date or
#'  character column or a date or string scalar (vector of length one) for
#'  sampling start and end dates. `lubridate::parse_date_time2()` is used to
#'  parse dates. Only `yyyy-mm-dd` or `dd-mm-yyyy` structures are acceptable.
#'  It recognize arbitrary non-digit separators as well as no separator. Month
#'  Can be entered as a digit or a full or abbreviated name.
#' @param test_id a string or an unquoted name of a character column that
#' contains the test IDs. Use serotrackr::assays to select these.
#' @param result Unquoted name of a numeric column containing test results.
#' @param result_cat Unquoted name of a character column with values of
#'  `positive`, `borderline`, or `negative`, ignoring case. A single string
#'  is also acceptable.
#' @param age_group Unquoted name of a character column or a string containing
#' age group(s). The only structures acceptable are `number-number` or
#'  `number+`. E.g. 18-64, and 65+.
#' @param age Unquoted name of a numeric column or a single number. Acceptable
#'  values are between 0 and 120 inclusive.
#' @param sex Unquoted name of a character column or a string. Acceptable
#'  values are: `f`, `m`, `o`, `female`, `male`, or `other` ignoring case.
#' @param include_others include additional columns or not
#' @return A validated data.frame
#' @import dplyr stringr
#' @importFrom dplyr `%>%`
#' @export
#' @examples
#' st_validate(sample_raw_data,
#'             dataset_id = dataset_id,
#'             id = id,
#'             age_group = "12-17",
#'             sex = "m",
#'             adm0 = regions$adm0$Canada,
#'             adm1 = regions$adm1$Canada$Alberta,
#'             adm2 = regions$adm2$Canada$Alberta$Calgary,
#'             collection_start_date = "2023-01-01",
#'             collection_end_date = "2023-02-01",
#'             test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
#'             result = result,
#'             result_cat = "negative",
#'             include_others = TRUE)

# TODO add feature to generate age_group, if missing, based on age

st_validate <- function(data,
                        dataset_id,
                        id,
                        adm0,
                        adm1 = NULL,
                        adm2 = NULL,
                        collection_start_date,
                        collection_end_date,
                        test_id,
                        result,
                        result_cat = NULL,
                        age_group = NULL,
                        age = NULL,
                        sex = NULL,
                        include_others = TRUE) {

  assert("data.frame" %in% class(data), "Data must be of class 'data.frame'.")
  # assert(length(adm0) == 1, "Only one adm0 (country) value is acceptable.")

  cli::cli_h1(cli::col_cyan("Mapping columns and validating data"))
  err_count <- 0
  data_name <- deparse(substitute(data))


  ## Missing req args --------------------------------------------------------

  required_args <- c("data", "adm0", "collection_start_date",
                     "collection_end_date", "test_id", "result",
                     "dataset_id", "id")
  index <- c(missing(data), missing(adm0), missing(collection_start_date),
             missing(collection_end_date), missing(test_id), missing(result),
             missing(dataset_id), missing(id))
  if(any(index)) {
    missing_arg <- required_args[index]
    cli::cli_abort(paste("{.arg {missing_arg}} argument{?s} {?is/are}",
                         "required but missing."))
  }


  ## Columns -----------------------------------------------------------------

  # Overall structure of validating each column is:
  # 1) Assess if input is a column or an atomic vector
  # 2) Check type (character, numeric, or date), and sometimes, sub-type
  # 3) Run validation rules
  # 4) Message the result, and if needed, transform that column

  # To prevent conflict between the Template's column names with the user's
  # column names, each renamed or mutated column will be stored separately
  # and bound together in the end.

  ### age_group ---------------------------------------------------------------

  msg_progress("age_group")
  timestamp <- Sys.time()
  err_age_group_n <- -1

  if (is.name(substitute(age_group))) {
    col_name <- deparse(substitute(age_group))
    if (!col_name %in% names(data)) {
      msg_colNotFound("age_group", data_name, col_name, start_time=timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("age_group", req_type="char", mode="col", name=col_name,
                      data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_age_group <- dplyr::select(data, age_group = {{age_group}})
        err_age_group <- validate_data(
          data = df_age_group,
          rules = rules[c("age_group_dashPlus", "lower_age", "upper_age",
                          "age_group_upLTE120", "age_group_lowLTEup",
                          "age_group_plusInRange")],
          mode = "col"
        )
        err_age_group_n <- err_age_group_n + 1 + length(err_age_group)
        if (err_age_group_n > 0) {
          msg_result("age_group", err_age_group, start_time = timestamp)
          err_count <- err_count + err_age_group_n
        } else {
          msg_result("age_group", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(age_group)) &&
           !is.null(substitute(age_group))) {
    if (!is.character(age_group)) {
      msg_wrongType("age_group", req_type = "char", mode = "val",
                    name = substitute(age_group), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(age_group)) {
        msg_wrongType("age_group", req_type = "char", mode = "no_typeof",
                      name = substitute(age_group), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_age_group <- data.frame(age_group = {{age_group}})
        err_age_group <- validate_data(
          data = df_age_group,
          rules = rules[c("age_group_dashPlus", "lower_age", "upper_age",
                          "age_group_upLTE120", "age_group_lowLTEup",
                          "age_group_plusInRange")],
          mode = "val"
        )
        err_age_group_n <- err_age_group_n + 1 + length(err_age_group)
        if (err_age_group_n > 0) {
          msg_result("age_group", err_age_group, start_time = timestamp)
          err_count <- err_count + err_age_group_n
        } else {
          df_age_group <- data.frame(age_group=rep({{age_group}},
                                                   times=nrow(data)))
          msg_result("age_group", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(age_group))) {
    cat('\r')  # Clears the previous (progress) message in console
  }


  ### age ---------------------------------------------------------------------

  if (!is.null(substitute(age))) {
    msg_progress("age")
    timestamp <- Sys.time()
    err_age_n <- -1
  }


  if (is.name(substitute(age))) {
    col_name <- deparse(substitute(age))
    if (!col_name %in% names(data)) {
      msg_colNotFound("age", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.numeric(data[[col_name]])) {
        msg_wrongType("age", req_type="num", mode="col", name=col_name,
                      data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_age <- dplyr::select(data, age = {{age}})
        if(err_age_group_n == 0) df_age <- cbind(df_age_group, df_age)
        err_age <- validate_data(
          data = df_age,
          rules = rules[c("lower_age", "upper_age", "age_notInf",
                          "age_GTE0", "age_LTE120", "age_GTElowerAge",
                          "age_LTEupperAge")],
          mode = "col", col_to_validate = "age", rules_msgCut = "age_notInf"
        )
        if(err_age_group_n == 0) df_age <- df_age["age"]
        err_age_n <- err_age_n + 1 + length(err_age)
        if (err_age_n > 0) {
          msg_result("age", err_age, start_time = timestamp)
          err_count <- err_count + err_age_n
        } else {
          msg_result("age", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(age)) && !is.null(substitute(age))) {
    if (!is.numeric(age)) {
      msg_wrongType("age", req_type = "num", mode = "val",
                    name = substitute(age), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      df_age <- data.frame(age = rep({{age}}, times = nrow(data)))
      if(err_age_group_n == 0) df_age <- cbind(df_age, df_age_group)
      err_age <- validate_data(
        data = df_age,
        rules = rules[c("lower_age", "upper_age", "age_notInf",
                        "age_GTE0", "age_LTE120", "age_GTElowerAge",
                        "age_LTEupperAge")],
        mode = "val", col_to_validate = "age", rules_msgCut = "age_notInf"
      )
      if(err_age_group_n == 0) df_age <- df_age["age"]
      err_age_n <- err_age_n + 1 + length(err_age)
      if (err_age_n > 0) {
        msg_result("age", err_age, start_time = timestamp)
        err_count <- err_count + err_age_n
      } else {
        msg_result("age", "is a valid number.", error=FALSE,
                   start_time=timestamp)
      }
    }
  }
  # else if (is.null(substitute(age))) {
  #   cat('\r')  # Clears the previous (progress) message in console
  # }


  ### sex ---------------------------------------------------------------------

  msg_progress("sex")
  timestamp <- Sys.time()
  err_sex_n <- -1

  if (is.name(substitute(sex))) {
    col_name <- deparse(substitute(sex))
    if (!col_name %in% names(data)) {
      msg_colNotFound("sex", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("sex", req_type = "char", mode = "col", name = col_name,
                      data = data, start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_sex <- dplyr::select(data, sex = {{sex}})
        err_sex <- validate_data(data = df_sex, rules = rules["sex_presetVal"],
                                 mode = "col")
        err_sex_n <- err_sex_n + 1 + length(err_sex)
        if (err_sex_n > 0) {
          msg_result("sex", err_sex, start_time = timestamp)
          err_count <- err_count + err_sex_n
        } else {
          df_sex <- dplyr::mutate(
            df_sex,
            sex = dplyr::case_when(
              grepl("^f$|^female$", sex, ignore.case = TRUE) ~ "Female",
              grepl("^m$|^male$", sex, ignore.case = TRUE) ~ "Male",
              grepl("^o$|^other$", sex, ignore.case = TRUE) ~ "Other",
              is.na(sex) ~ NA # TODO grepl("^na$|^n/a$", sex, ignore.case=TRUE)
            )
          )
          msg_result("sex", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(sex)) && !is.null(substitute(sex))) {
    if (!is.character(sex)) {
      msg_wrongType("sex", req_type = "char", mode = "val",
                    name = substitute(sex), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(sex)) {
        msg_wrongType("sex", req_type = "char", mode = "no_typeof",
                      name = substitute(sex), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_sex <- data.frame(sex = {{sex}})
        err_sex <- validate_data(data = df_sex, rules = rules["sex_presetVal"],
                                 mode = "val")
        err_sex_n <- err_sex_n + 1 + length(err_sex)
        if (err_sex_n > 0) {
          msg_result("sex", err_sex, start_time = timestamp)
          err_count <- err_count + err_sex_n
        } else {
          df_sex <- dplyr::mutate(
            df_sex,
            sex = dplyr::case_when(
              grepl("^f$|^female$", sex, ignore.case = TRUE) ~ "Female",
              grepl("^m$|^male$", sex, ignore.case = TRUE) ~ "Male",
              grepl("^o$|^other$", sex, ignore.case = TRUE) ~ "Other",
              is.na(sex) ~ NA # TODO grepl("^na$|^n/a$", sex, ignore.case=TRUE)
            )
          )
          df_sex <- dplyr::add_row(df_sex,
                                   sex=rep(df_sex[[sex]], times=nrow(data)-1))
          msg_result("sex", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(sex))) {
    cat('\r')  # Clears the previous (progress) message in console
  }


  ### adm0 --------------------------------------------------------------------

  msg_progress("adm0")
  timestamp <- Sys.time()
  err_adm0_n <- -1

  if (is.name(substitute(adm0))) {
    col_name <- deparse(substitute(adm0))
    if (!col_name %in% names(data)) {
      msg_colNotFound("adm0", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("adm0", req_type = "char", mode = "col", name=col_name,
                      data = data, start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm0 <- dplyr::select(data, adm0 = {{adm0}})
        err_adm0 <- validate_data(data = df_adm0,
                                  rules = rules["adm0_presetVal"],
                                  mode = "col")
        err_adm0_n <- err_adm0_n + 1 + length(err_adm0)
        if (err_adm0_n > 0) {
          msg_result("adm0", err_adm0, start_time = timestamp)
          err_count <- err_count + err_adm0_n
        } else {
          msg_result("adm0", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(adm0)) && !is.null(substitute(adm0))) {
    if (!is.character(adm0)) {
      msg_wrongType("adm0", req_type = "char", mode = "val",
                    name = substitute(adm0), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(adm0)) {
        msg_wrongType("adm0", req_type = "char", mode = "no_typeof",
                      name = substitute(adm0), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm0 <- data.frame(adm0 = {{adm0}})
        err_adm0 <- validate_data(data = df_adm0,
                                  rules = rules["adm0_presetVal"],
                                  mode = "val")
        err_adm0_n <- err_adm0_n + 1 + length(err_adm0)
        if (err_adm0_n > 0) {
          msg_result("adm0", err_adm0, start_time = timestamp)
          err_count <- err_count + err_adm0_n
        } else {
          df_adm0 <- data.frame(adm0 = rep({{adm0}}, times = nrow(data)))
          msg_result("adm0", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(adm0))) {
    msg_wrongType("adm0", req_type = "char", mode = "no_typeof",
                  start_time = timestamp)
    err_count <- err_count + 1
  }


  ### adm1 --------------------------------------------------------------------

  msg_progress("adm1")
  timestamp <- Sys.time()
  err_adm1_n <- -1

  if (is.name(substitute(adm1))) {
    col_name <- deparse(substitute(adm1))
    if (!col_name %in% names(data)) {
      msg_colNotFound("adm1", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("adm1", req_type = "char", mode = "col", name=col_name,
                      data = data, start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm1 <- dplyr::select(data, adm1 = {{adm1}})
        err_adm1 <- validate_data(data = df_adm1,
                                  rules = rules["adm1_presetVal"],
                                  mode = "col")
        err_adm1_n <- err_adm1_n + 1 + length(err_adm1)
        if (err_adm1_n > 0) {
          msg_result("adm1", err_adm1, start_time = timestamp)
          err_count <- err_count + err_adm1_n
        } else {
          msg_result("adm1", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(adm1)) && !is.null(substitute(adm1))) {
    if (!is.character(adm1)) {
      msg_wrongType("adm1", req_type = "char", mode = "val",
                    name = substitute(adm1), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(adm1)) {
        msg_wrongType("adm1", req_type = "char", mode = "no_typeof",
                      name = substitute(adm1), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm1 <- data.frame(adm1 = {{adm1}})
        err_adm1 <- validate_data(data = df_adm1,
                                  rules = rules["adm1_presetVal"],
                                  mode = "val")
        err_adm1_n <- err_adm1_n + 1 + length(err_adm1)
        if (err_adm1_n > 0) {
          msg_result("adm1", err_adm1, start_time = timestamp)
          err_count <- err_count + err_adm1_n
        } else {
          df_adm1 <- data.frame(adm1 = rep({{adm1}}, times = nrow(data)))
          msg_result("adm1", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(adm1))) {
    cat('\r')  # Clears the previous (progress) message in console
  }


  ### adm2 --------------------------------------------------------------------

  msg_progress("adm2")
  timestamp <- Sys.time()
  err_adm2_n <- -1

  if (is.name(substitute(adm2))) {
    col_name <- deparse(substitute(adm2))
    if (!col_name %in% names(data)) {
      msg_colNotFound("adm2", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("adm2", req_type = "char", mode = "col", name=col_name,
                      data = data, start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm2 <- dplyr::select(data, adm2 = {{adm2}})
        err_adm2 <- validate_data(data = df_adm2,
                                  rules = rules["adm2_presetVal"],
                                  mode = "col")
        err_adm2_n <- err_adm2_n + 1 + length(err_adm2)
        if (err_adm2_n > 0) {
          msg_result("adm2", err_adm2, start_time = timestamp)
          err_count <- err_count + err_adm2_n
        } else {
          msg_result("adm2", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(adm2)) && !is.null(substitute(adm2))) {
    if (!is.character(adm2)) {
      msg_wrongType("adm2", req_type = "char", mode = "val",
                    name = substitute(adm2), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(adm2)) {
        msg_wrongType("adm2", req_type = "char", mode = "no_typeof",
                      name = substitute(adm2), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_adm2 <- data.frame(adm2 = {{adm2}})
        err_adm2 <- validate_data(data = df_adm2,
                                  rules = rules["adm2_presetVal"],
                                  mode = "val")
        err_adm2_n <- err_adm2_n + 1 + length(err_adm2)
        if (err_adm2_n > 0) {
          msg_result("adm2", err_adm2, start_time = timestamp)
          err_count <- err_count + err_adm2_n
        } else {
          df_adm2 <- data.frame(adm2 = rep({{adm2}}, times = nrow(data)))
          msg_result("adm2", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(adm2))) {
    cat('\r')  # Clears the previous (progress) message in console
  }


  ### collection_start_date ---------------------------------------------------

  msg_progress("collection_start_date")
  timestamp <- Sys.time()
  err_collection_start_date_n <- -1

  if (is.name(substitute(collection_start_date))) {
    col_name <- deparse(substitute(collection_start_date))
    if (!col_name %in% names(data)) {
      msg_colNotFound("collection_start_date", data_name, col_name,
                      start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!(is.character(data[[col_name]]) ||
            lubridate::is.Date(data[[col_name]]))) {
        msg_wrongType("collection_start_date", req_type="date_char", mode="col",
                      name=col_name, data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_collection_start_date <- dplyr::select(
          data, collection_start_date = {{collection_start_date}}
        )
        # TODO Separate validation of character and date columns for efficiency
        df_collection_start_date <- dplyr::mutate(
          df_collection_start_date,
          collection_start_date = as.character(collection_start_date)
        )
        err_collection_start_date <- validate_data(
          data = df_collection_start_date,
          rules = rules[c("start_date_isValidFrmt", "start_date_2000today")],
          mode = "col"
        )
        err_collection_start_date_n <- err_collection_start_date_n + 1 +
          length(err_collection_start_date)
        if (err_collection_start_date_n > 0) {
          msg_result("collection_start_date", err_collection_start_date,
                     start_time = timestamp)
          err_count <- err_count + err_collection_start_date_n
        } else {
          df_collection_start_date <- dplyr::mutate(
            df_collection_start_date,
            collection_start_date = lubridate::parse_date_time2(
              collection_start_date, c("dmY","Ymd")
            ),
            collection_start_date = as.Date(collection_start_date)
          )
          msg_result("collection_start_date", "is a valid column.",
                     error = FALSE, start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(collection_start_date)) &&
           !is.null(substitute(collection_start_date))) {
    if (!(is.character(collection_start_date) ||
          lubridate::is.Date(collection_start_date))) {
      msg_wrongType("collection_start_date", req_type = "date_char",
                    mode = "val", name = substitute(collection_start_date),
                    start_time = timestamp)
      err_count <- err_count + 1
    } else {
      # TODO Separate validation of character and date columns for efficiency
      collection_start_date <- as.character(collection_start_date)
      if (!rlang::is_string(collection_start_date)) {
        msg_wrongType("collection_start_date", req_type="date_char",
                      mode="no_typeof", name=substitute(collection_start_date),
                      start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_collection_start_date <- data.frame(
          collection_start_date = {{collection_start_date}}
        )
        err_collection_start_date <- validate_data(
          data = df_collection_start_date,
          rules = rules[c("start_date_isValidFrmt", "start_date_2000today")],
          mode = "val"
        )
        err_collection_start_date_n <- err_collection_start_date_n + 1 +
          length(err_collection_start_date)
        if (err_collection_start_date_n > 0) {
          msg_result("collection_start_date", err_collection_start_date,
                     start_time = timestamp)
          err_count <- err_count + err_collection_start_date_n
        } else {
          df_collection_start_date <- dplyr::mutate(
            df_collection_start_date,
            collection_start_date = lubridate::parse_date_time2(
              collection_start_date, c("dmY","Ymd")
            ),
            collection_start_date = as.Date(collection_start_date)
          )
          df_collection_start_date <- dplyr::add_row(
            df_collection_start_date,
            collection_start_date = rep(
              df_collection_start_date[[collection_start_date]],
              times = nrow(data) - 1
            )
          )
          msg_result("collection_start_date", "is a valid scalar.",
                     error = FALSE, start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(collection_start_date))) {
    msg_wrongType("collection_start_date", req_type = "date_char",
                  mode="no_typeof", name=substitute(collection_start_date),
                  start_time = timestamp)
    err_count <- err_count + 1
  }


  ### collection_end_date -----------------------------------------------------

  msg_progress("collection_end_date")
  timestamp <- Sys.time()
  err_collection_end_date_n <- -1

  if (is.name(substitute(collection_end_date))) {
    col_name <- deparse(substitute(collection_end_date))
    if (!col_name %in% names(data)) {
      msg_colNotFound("collection_end_date", data_name, col_name,
                      start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!(is.character(data[[col_name]]) ||
            lubridate::is.Date(data[[col_name]]))) {
        msg_wrongType("collection_end_date", req_type="date_char", mode="col",
                      name=col_name, data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_collection_end_date <- dplyr::select(
          data, collection_end_date = {{collection_end_date}}
        )
        # TODO Separate validation of character and date columns for efficiency
        df_collection_end_date <- dplyr::mutate(
          df_collection_end_date,
          collection_end_date = as.character(collection_end_date)
        )
        if(err_collection_start_date_n == 0) {
          df_collection_end_date <- cbind(
            dplyr::mutate(
              df_collection_start_date,
              collection_start_date = as.character(collection_start_date)
            ),
            df_collection_end_date
          )
        }
        err_collection_end_date <- validate_data(
          data = df_collection_end_date,
          rules = rules[c("end_date_isValidFrmt", "end_date_2000today",
                          "end_date_GTEstart")],
          mode = "col", col_to_validate = "collection_end_date"
        )
        if(err_collection_start_date_n == 0) {
          df_collection_end_date <-
            df_collection_end_date["collection_end_date"]
        }
        err_collection_end_date_n <- err_collection_end_date_n + 1 +
          length(err_collection_end_date)
        if (err_collection_end_date_n > 0) {
          msg_result("collection_end_date", err_collection_end_date,
                     start_time = timestamp)
          err_count <- err_count + err_collection_end_date_n
        } else {
          df_collection_end_date <- dplyr::mutate(
            df_collection_end_date,
            collection_end_date = lubridate::parse_date_time2(
              collection_end_date, c("dmY","Ymd")
            ),
            collection_end_date = as.Date(collection_end_date)
          )
          msg_result("collection_end_date", "is a valid column.",
                     error = FALSE, start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(collection_end_date)) &&
           !is.null(substitute(collection_end_date))) {
    if (!(is.character(collection_end_date) ||
          lubridate::is.Date(collection_end_date))) {
      msg_wrongType("collection_end_date", req_type = "date_char",
                    mode = "val", name = substitute(collection_end_date),
                    start_time = timestamp)
      err_count <- err_count + 1
    } else {
      # TODO Separate validation of character and date columns for efficiency
      collection_end_date <- as.character(collection_end_date)
      if (!rlang::is_string(collection_end_date)) {
        msg_wrongType("collection_end_date", req_type="date_char",
                      mode="no_typeof", name=substitute(collection_end_date),
                      start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_collection_end_date <- data.frame(
          collection_end_date=rep({{collection_end_date}}, times=nrow(data))
        )
        if(err_collection_start_date_n == 0) {
          df_collection_end_date <- cbind(
            dplyr::mutate(
              df_collection_start_date,
              collection_start_date = as.character(collection_start_date)
            ),
            df_collection_end_date
          )
        }
        err_collection_end_date <- validate_data(
          data = df_collection_end_date,
          rules = rules[c("end_date_isValidFrmt", "end_date_2000today",
                          "end_date_GTEstart")],
          mode = "val", col_to_validate = "collection_end_date"
        )
        if(err_collection_start_date_n == 0) {
          df_collection_end_date <-
            df_collection_end_date["collection_end_date"]
        }
        err_collection_end_date_n <- err_collection_end_date_n + 1 +
          length(err_collection_end_date)
        if (err_collection_end_date_n > 0) {
          msg_result("collection_end_date", err_collection_end_date,
                     start_time = timestamp)
          err_count <- err_count + err_collection_end_date_n
        } else {
          df_collection_end_date <- dplyr::mutate(
            df_collection_end_date,
            collection_end_date = lubridate::parse_date_time2(
              collection_end_date, c("dmY","Ymd")
            ),
            collection_end_date = as.Date(collection_end_date)
          )
          msg_result("collection_end_date", "is a valid scalar.",
                     error = FALSE, start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(collection_end_date))) {
    msg_wrongType("collection_end_date", req_type = "date_char",
                  mode = "no_typeof", name = substitute(collection_end_date),
                  start_time = timestamp)
    err_count <- err_count + 1
  }


  ### test_id -----------------------------------------------------------------

  msg_progress("test_id")
  timestamp <- Sys.time()
  err_test_id_n <- -1

  if (is.name(substitute(test_id))) {
    col_name <- deparse(substitute(test_id))
    if (!col_name %in% names(data)) {
      msg_colNotFound("test_id", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("test_id", req_type="char", mode="col", name=col_name,
                      data = data, start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_test_id <- dplyr::select(data, test_id = {{test_id}})
        err_test_id <- validate_data(data = df_test_id,
                                     rules = rules["test_id_presetVal"],
                                     mode = "col")
        err_test_id_n <- err_test_id_n + 1 + length(err_test_id)
        if (err_test_id_n > 0) {
          msg_result("test_id", err_test_id, start_time = timestamp)
          err_count <- err_count + err_test_id_n
        } else {
          msg_result("test_id", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(test_id)) && !is.null(substitute(test_id))) {
    if (!is.character(test_id)) {
      msg_wrongType("test_id", req_type = "char", mode = "val",
                    name = substitute(test_id), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(test_id)) {
        msg_wrongType("test_id", req_type = "char", mode = "no_typeof",
                      name = substitute(test_id), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_test_id <- data.frame(test_id = {{test_id}})
        err_test_id <- validate_data(data = df_test_id,
                                     rules = rules["test_id_presetVal"],
                                     mode = "val")
        err_test_id_n <- err_test_id_n + 1 + length(err_test_id)
        if (err_test_id_n > 0) {
          msg_result("test_id", err_test_id, start_time = timestamp)
          err_count <- err_count + err_test_id_n
        } else {
          df_test_id <- data.frame(test_id=rep({{test_id}}, times=nrow(data)))
          msg_result("test_id", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(test_id))) {
    msg_wrongType("test_id", req_type = "char", mode = "no_typeof",
                  start_time = timestamp)
    err_count <- err_count + 1
  }


  ### result ------------------------------------------------------------------

  msg_progress("result")
  timestamp <- Sys.time()
  err_result_n <- -1

  if (is.name(substitute(result))) {
    col_name <- deparse(substitute(result))
    if (!col_name %in% names(data)) {
      msg_colNotFound("result", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.numeric(data[[col_name]])) {
        msg_wrongType("result", req_type="num", mode="col", name=col_name,
                      data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_result <- dplyr::select(data, result = {{result}})
        err_result <- validate_data(data = df_result,
                                    rules = rules["result_GTE0"],
                                    mode = "col")
        err_result_n <- err_result_n + 1 + length(err_result)
        if (err_result_n > 0) {
          msg_result("result", err_result, start_time = timestamp)
          err_count <- err_count + err_result_n
        } else {
          msg_result("result", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  } else {
    msg_result("result", "must be an unquoted name of a numeric column.",
               start_time = timestamp)
    err_count <- err_count + 1
  }


  ### result_cat --------------------------------------------------------------

  msg_progress("result_cat")
  timestamp <- Sys.time()
  err_result_cat_n <- -1

  if (is.name(substitute(result_cat))) {
    col_name <- deparse(substitute(result_cat))
    if (!col_name %in% names(data)) {
      msg_colNotFound("result_cat", data_name, col_name, start_time=timestamp)
      err_count <- err_count + 1
    } else {
      if (!is.character(data[[col_name]])) {
        msg_wrongType("result_cat", req_type="char", mode="col", name=col_name,
                      data=data, start_time=timestamp)
        err_count <- err_count + 1
      } else {
        df_result_cat <- dplyr::select(data, result_cat = {{result_cat}})
        err_result_cat <- validate_data(data = df_result_cat,
                                        rules = rules["result_cat_presetVal"],
                                        mode = "col")
        err_result_cat_n <- err_result_cat_n + 1 + length(err_result_cat)
        if (err_result_cat_n > 0) {
          msg_result("result_cat", err_result_cat, start_time = timestamp)
          err_count <- err_count + err_result_cat_n
        } else {
          msg_result("result_cat", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (!is.name(substitute(result_cat)) &&
           !is.null(substitute(result_cat))) {
    if (!is.character(result_cat)) {
      msg_wrongType("result_cat", req_type = "char", mode = "val",
                    name = substitute(result_cat), start_time = timestamp)
      err_count <- err_count + 1
    } else {
      if (!rlang::is_string(result_cat)) {
        msg_wrongType("result_cat", req_type = "char", mode = "no_typeof",
                      name = substitute(result_cat), start_time = timestamp)
        err_count <- err_count + 1
      } else {
        df_result_cat <- data.frame(result_cat = {{result_cat}})
        err_result_cat <- validate_data(data = df_result_cat,
                                        rules = rules["result_cat_presetVal"],
                                        mode = "val")
        err_result_cat_n <- err_result_cat_n + 1 + length(err_result_cat)
        if (err_result_cat_n > 0) {
          msg_result("result_cat", err_result_cat, start_time = timestamp)
          err_count <- err_count + err_result_cat_n
        } else {
          df_result_cat <- data.frame(result_cat = rep({{result_cat}},
                                                       times = nrow(data)))
          msg_result("result_cat", "is a valid string.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  }
  else if (is.null(substitute(result_cat))) {
    cat('\r')  # Clears the previous (progress) message in console
  }


  ### dataset_id --------------------------------------------------------------

  msg_progress("dataset_id")
  timestamp <- Sys.time()

  if (is.name(substitute(dataset_id))) {
    col_name <- deparse(substitute(dataset_id))
    if (!col_name %in% names(data)) {
      msg_colNotFound("dataset_id", data_name, col_name, start_time=timestamp)
      err_count <- err_count + 1
    } else {
      df_dataset_id <- dplyr::select(data, dataset_id = {{dataset_id}})
      msg_result("dataset_id", "is a valid column.", error = FALSE,
                 start_time = timestamp)
    }
  }
  else if (!is.name(substitute(dataset_id)) &&
           !is.null(substitute(dataset_id)) &&
           length(dataset_id) == 1) {
    df_dataset_id <- data.frame(dataset_id = rep({{dataset_id}},
                                                 times = nrow(data)))
    msg_result("dataset_id", "is a valid scalar", error = FALSE,
               start_time = timestamp)
  }
  else {
    msg_result("dataset_id", "must be an unquoted column name or a scalar.",
               start_time = timestamp)
    err_count <- err_count + 1
  }


  ### id ----------------------------------------------------------------------

  msg_progress("id")
  timestamp <- Sys.time()
  err_id_n <- -1

  if (is.name(substitute(id))) {
    col_name <- deparse(substitute(id))
    if (!col_name %in% names(data)) {
      msg_colNotFound("id", data_name, col_name, start_time = timestamp)
      err_count <- err_count + 1
    } else {
      df_id <- dplyr::select(data, id = {{id}})
      if (!(err_collection_start_date_n == 0 &
            err_collection_end_date_n   == 0 &
            err_test_id_n               == 0 &
            err_result_n                == 0)) {
        msg_result(
          "id",
          paste("will be evaluated when `{.emph collection_start_date}`,",
                "`{.emph collection_end_date}`, `{.emph test_id}`, and",
                "`{.emph result}` arguments are error-free."),
          start_time = timestamp
        )
        err_count <- err_count + 1
      } else {
        df_uniqueness <- cbind(df_id, df_collection_start_date,
                               df_collection_end_date, df_test_id, df_result)
        err_id <- validate_data(data = df_uniqueness,
                                rules = rules["id_isUnique"], mode = "col",
                                col_to_validate = "id")
        err_id_n <- err_id_n + 1 + length(err_id)
        if (err_id_n > 0) {
          msg_result("id", err_id, start_time = timestamp)
          err_count <- err_count + err_id_n
        } else {
          msg_result("id", "is a valid column.", error = FALSE,
                     start_time = timestamp)
        }
      }
    }
  } else {
    msg_result("id", "must be an unquoted column name.", start_time=timestamp)
    err_count <- err_count + 1
  }


  ## Error -------------------------------------------------------------------

  cli::cli_h1(cli::col_cyan("Validation finished"))

  if (err_count > 0) {
    cli::cli_abort(
      paste(cli::col_red("{err_count} error{?s}!"),
            "Please address {?it/them} first. Validated data not created.")
    )
  }


  ## Success -----------------------------------------------------------------

  if (err_count == 0) {

    ### Column sets -------------------------------------------------------------

    # Column names defined by ST, except for adm0 which will be stored as
    # an attribute in the output data.frame
    preset_colnames <- c("dataset_id", "id", "age_group", "age", "sex",
                         "adm1", "adm2", "collection_start_date",
                         "collection_end_date", "test_id", "result",
                         "result_cat")

    raw_colnames_all <- names(data)
    raw_colnames_input <- sapply(
      substitute(c(dataset_id, id, age_group, age, sex, adm1, adm2,
                   collection_start_date, collection_end_date, result,
                   result_cat)),
      deparse
    )[-1]


    ### Merge dfs ---------------------------------------------------------------

    dfs_all <- paste0("df_", preset_colnames)
    dfs_to_bind <- dfs_all[which(dfs_all %in% ls())]

    # Remove raw columns with our predefined names to prevent conflict with
    # validated columns with the same names:
    data <- dplyr::select(data, -dplyr::any_of(c(preset_colnames, "adm0")))
    # Then, bind validated columns:
    # When df_xxx1 has 1 row and df_xxx2 has more, both dplyr::bind_cols()
    # and cbind() will recycle that single value in df_xxx1 to fill the rest.
    data <- dplyr::bind_cols(c(list(data), mget(dfs_to_bind, inherits = TRUE)))


    ### include_others ----------------------------------------------------------

    if(isFALSE(include_others)) {
      data <- dplyr::select(data, !!!dplyr::any_of(c(preset_colnames)))
    } else if (isTRUE(include_others)) {
      data <- dplyr::select(
        data,
        !!!dplyr::any_of(c(preset_colnames,
                           setdiff(raw_colnames_all, raw_colnames_input)))
        )
    }


    ### add attributes ---------------------------------------------------------

    data <- add_attr(
      data,
      pathogen = dplyr::pull(dplyr::filter(assays_df,
                                           test_id==unique(data$test_id)[1]),
                             pathogen),
      adm0 = unique(df_adm0$adm0)
    )


    ### Message success ---------------------------------------------------------

    cli::cli_text(paste(cli::col_green("Success!"), "Validated data created."))
    cli::cli_par(); cli::cli_end()
    return(dplyr::tibble(data))
  }
} # st_validate() closes



# Helper functions for st_validate() --------------------------------------

#' Assert a logical expression
#'
#' @param expr A logical expression to check
#' @param err_msg Error message for when the `expr` is FALSE
#'
#' @return If `expr` is FALSE, it stops the execution and returns the `err_msg`.
#'
#' @examples
#' f <- function(data) {
#'   assert("data.frame" %in% class(data),
#'          "Data must be of class 'data.frame'.")
#' }
#' f(sample_raw_data)
#'

assert <- function (expr, err_msg) {
  if(missing(err_msg)) err_msg <- paste("Condition",
                                        deparse(as.list(match.call())$expr),
                                        "is not TRUE")
  if (!expr) stop(err_msg, call. = FALSE)
}



#' Validate using predefined rules
#'
#' @param data A dataframe whose colnames are exactly those specified in rules
#' @param rules A validator object from the `validate` package
#' @param mode Defines if validation is done for a column (col) or for a single
#' value scalar (val). Th latter has shorter error messages.
#' @param col_to_validate Column that is the target of validation. Specify this
#' when validation of the target column requires other columns from data; e.g
#' to asses end date is after start date.
#' @param rules_msgCut A character vector of rule names for which the last part
#' of the returned message will be trimmed; e.g. for rule `is.infinite()`.
#'
#' @return A character vector of error messages based on rules applied. If no
#' errors occur, a vector of length zero is returned.
#' @import dplyr
#' @importFrom dplyr `%>%`
#'
#' @examples
#' err_msgs <- validate_data(sample_raw_data, rules["sex_presetVal"],
#'                           mode = "col", col_to_validate = "sex")
#' err_msgs

validate_data <- function(data, rules, mode = c("col", "val"),
                          col_to_validate = names(data), rules_msgCut = NULL) {
  values <- as.data.frame(validate::values(validate::confront(data, rules)))
  mode <- match.arg(mode)
  err_msg <- character()
  for (i in 1:ncol(values)) {
    err_index <- which(values[[i]] %in% FALSE)
    err_n <- length(err_index)
    err_unique_n <- length(unique(data[[col_to_validate]][err_index]))
    if (err_n > 0) {
      err_val_head <- cli::cli_vec(
        x = head(unique(data[[col_to_validate]][err_index])),
        style = list('vec-trunc'=3, 'vec-trunc-style'='head')
      )
      col_specific_msg <- cli::format_inline(
        paste0(
          " {.val {err_n}} record{?s} {?has/have} this issue.",
          ifelse(!is.null(rules_msgCut) && names(values)[i] %in% rules_msgCut,
                 "", paste(" Invalid {cli::qty(err_unique_n)}value{?s}",
                           " {?is/are} {.val {err_val_head}}."))
        )
      )
      err_msg <- c(
        err_msg,
        paste0(validate::description(rules[names(values)[i]]),
               ifelse(mode == "col", col_specific_msg, ""))
      )
    }
  }
  err_msg
}



#' Print message as progress step
#'
#' @param msg A string scalar.
#'
#' @return A message in the console.
#'
#' @examples
#' msg_progress("age_group")

msg_progress <- function(msg) {
  stopifnot(!is.null(msg))
  stopifnot(is.character(msg))
  cat(cli::col_yellow(cli::symbol$info), msg, "...")
}



#' Style and print a message
#'
#' @param arg Column name or argument to which the message belongs. This section
#' will be highlighted in green or red, based on the `error` argument.
#' @param msg A character vector for the body of the massage(s) that is/are
#' printed after `arg`. This section will be dimmed if `error` is set as FALSE.
#' @param error If TRUE (default), message starts with a red cross symbol and
#' `arg` will be highlighted in red. If FALSE, message starts with a green
#' tick symbol and `arg` will be highlighted in green.
#' @param start_time A POSIXct object used to add code execution time to the
#' end of the printed message.
#'
#' @return A styled message. If the length of `msg` is more than 1, the message
#' will be broken down in bullet points.
#'
#' @examples
#' timestamp <- Sys.time()
#' msg_result("age_group", "is a valid column.", error = FALSE,
#'            start_time = timestamp)

msg_result <- function(arg, msg, error = TRUE, start_time = NULL) {
  if(!is.null(start_time)) {
    time_elapsed <- Sys.time() - start_time
    pretty_time <- prettyunits::pretty_sec(as.numeric(time_elapsed))
  }

  cat('\r')  # Clears the previous (progress) message in console

  if (length(msg) == 1) {
    cli::cli({
      cli::cli_div(theme = list(div = list(`margin-left`=0, `text-exdent`=2)))
      cli::cli_text(
        ifelse(error,
               paste(cli::col_red(cli::symbol$cross), cli::bg_red(arg)),
               paste(cli::col_green(cli::symbol$tick), cli::bg_green(arg))), " ",
        ifelse(error,
               cli::col_none(cli::format_inline(msg)),
               cli::col_grey(cli::format_inline(msg))),
        if(!is.null(start_time)) {
          cli::col_cyan(cli::format_inline(" [{pretty_time}]"))
        }
      )
      cli::cli_end()
    })
  }
  else if (length(msg) > 1) {
    # Always error
    cli::cli({
      cli::cli_text(cli::col_red(cli::symbol$cross), " ", cli::bg_red(arg),
                    if(!is.null(start_time)) {
                      cli::col_cyan(cli::format_inline(" [{pretty_time}]"))
                    }
      )
      cli::cli_div(theme = list(ul = list(`margin-left`=2, `text-exdent`=2)))
      cli::cli_ul(msg)
    })
  }
}



#' Message when a column was not found
#'
#' @description This is a wrapper around `msg_result()`. Only to be used inside
#' another function.
#'
#' @param arg Preset column name or argument for which the validation is run.
#' @param data_name Name of the raw data object supplied by user. Can be
#' supplied using `deparse(substitute(data))`
#' @param col_name Column name supplied by user for this `arg`. Can be supplied
#' using `deparse(substitute(argument))`.
#' @param ... Additional argument passed to `msg_result()`.
#'
#' @return Prints a message to the console.
#'

msg_colNotFound <- function(arg, data_name, col_name, ...) {
  msg_result(arg,
             paste0("Column `{.emph ", col_name, "}` ",
                    "doesn't exist in `{.emph ", data_name, "}`."),
             ...)
}



#' Message when supplied type is wrong
#'
#' @param arg Preset column name or argument for which the validation is run.
#' @param req_type Type that was expected for this `arg`.
#' @param mode Defines if validation is done for a column (col) or for a single
#' value scalar (val). This will determine how the current wrong type of the
#' supplied column or vector should be evaluated. The `no_typeof` option is for
#' when we do not need the message to print the type of the supplied input.
#' @param name Only for modes `col` and `val`. The column name, supplied as
#' `deparse(substitute(argument))` or input value as `substitute(argument)`.
#' @param data A dataframe. Only for mode `col`.
#' @param ... Additional argument passed to `msg_result()`.
#'
#' @return Prints a message to the console.
#'
#' @examples
#' msg_wrongType("age", req_type = "num", mode = "no_typeof")

msg_wrongType <- function(arg, req_type = c("char", "num", "date_char"),
                          mode = c("col", "val", "no_typeof"), name = NULL,
                          data = NULL, ...) {
  req_type <- match.arg(req_type)
  if (req_type == "char") {
    req_type <- list("character", "string")
  } else if (req_type == "num") {
    req_type <- list("numeric", "number")
  } else {
    req_type <- list("date or character", "date or string")
  }

  mode <- match.arg(mode)
  if (mode == "col") {
    typeof <- paste0(", not ", typeof(data[[name]]), ".")
  } else if (mode == "val") {
    typeof <- paste0(", not ", typeof(name), ".")
  } else {
    typeof <- "."
  }

  msg <- paste0("must be an unquoted name of a ",req_type[[1]]," column ",
                "or a ", req_type[[2]], " scalar", typeof)
  msg_result(arg, msg, ...)
}



#' Generate object ID
#'
#' @param digits Number of digits of the generated random ID.
#'
#' @return A random number
#'
#' @examples
#' set.seed(4567)
#' generate_obj_id(5)

generate_obj_id <- function(digits) {
  obj_id <- sample(1:9, 1)
  for (i in 2:digits) {
    obj_id[i] <- sample(0:9, 1)
  }
  as.numeric(paste(obj_id, collapse = ""))
}



#' Add specific attributes to a dataframe
#'
#' @param data A dataframe to which attributes will be added.
#' @param pathogen Name of pathogen passed to an attribute of the same name.
#' This will be retrieved from `assays_df` based on `test_id` provided by user.
#' @param adm0 Unique code of the country passed to an attribute of the same
#' name. Code is based on `serotrackr::regions` object.
#'
#' @return A dataframe with additional attributes.
#' @note An `id` attribute is also added using `generate_obj_id()`.
#'
#' @examples
#' df <- add_attr(sample_raw_data, pathogen = "SARS-CoV-2",
#'                adm0 = regions$adm0$Canada)
#' attributes(df)

add_attr <- function(data, pathogen, adm0) {
  stopifnot(is.data.frame(data), is.character(pathogen), is.character(adm0))
  attr(data, "id") <- generate_obj_id(digits = 5)
  attr(data, "pathogen") <- pathogen
  attr(data, "adm0") <- adm0
  # attr(data, "class") <- c("st_data", "data.frame")
  return(data)
}

