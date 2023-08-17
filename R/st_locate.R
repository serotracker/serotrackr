
# TODO The ... argument in st_locate() currently does not check if hierarchy
# of adm1 and adm2 is valid. st_locate() only checks that in its `automatic`
# section (left_join).
# FIXME The ... argument in st_locate() must run only if adm1 and/or adm2 are
# column names. Temporary fix is applied. Improve this.
# TODO `st_locate()`'s reporting message can be confusing when an adm1 region
# has errors, its corresponding adm2 is okay, and there are other erroneous
# adm2 values. In this case, user needs to run `st_locate()` twice, first to
# address adm1, and then to address adm2.


#' @title Convert region names to codes
#' @description
#' `r lifecycle::badge("experimental")`
#' This function helps you convert your region names to region codes acceptable
#'  by `serotrackr`. It is usually used when you get an error for
#'  either `adm1` or `adm2` arguments in `st_validate()`. In that case, you
#'  can use this function to convert your region names to codes and make sure
#'  they pass the validation by `st_validate()`.
#'
#'  `st_locate()` uses its first three (or four) arguments to automatically
#'  match your region names with `serotrackr`'s predefined region codes, which
#'  are stored in `serotrackr::regions_df`. It performs exact case insensitive
#'  matching and also ignores accented letters. It then produces a report of
#'  your region names that could not be matched. You can then use
#'  `st_locate()`'s ... (ellipsis) argument to define those unmatched region
#'  names.
#' @param data A dataframe.
#' @param adm0 A string representing one country (adm0) code. Use
#'  `serotrackr::regions$adm0$YourCountry` to select it.
#' @param adm1,adm2 a string or an unquoted name of a character column that
#'  contains the state/province (adm1), or district/municipality
#'  (adm2) names or codes. If your study is conducted in only one adm1 or adm2
#'  region, use `serotrackr::regions` to select them.
#' @param ... A sequence of two-sided assignments as in
#'  `"region_name" = region_code`. `region_name` is your unmatched region name.
#'  `region_code` must be in the form of `regions$adm1$YourCountry$YourState`
#'  for unmatched adm1 regions and
#'  `regions$adm2$YourCountry$YourState$YourDistrict` for unmatched adm2
#'  regions. `st_locate()` uses `dplyr::case_when()` internally for this part.
#' @param into A character vector of length 1 or 2, specifying the name(s) of
#'  the new columns that are added to your data for adm1 (and adm2) region
#'  codes. If length is one, name of the adm1 column will be assigned. If
#'  length is two, names of adm1 and adm2 columns will be assigned,
#'  respectively.
#' @param n_unmatched_printed A single number, indicating the quantity of
#'  unmatched unique region names that are printed to the console. This argument
#'  is implemented to prevent potentially flooding your console with unmatched
#'  region names. The default value is 20. If there are more than 20 unmatched
#'  region names in your data, increase this number to see the rest.
#'
#' @return A data.frame that consists of the input data.frame plus one (or two)
#'  new columns containing adm1 (and adm2) region codes. You can then use these
#'  two new columns for adm1 and/or adm2 arguments of `st_validate()`.
#'
#' @export
#'
#' @examples
#' st_locate(
#'   data = sample_raw_data,
#'   adm0 = regions$adm0$Canada,
#'   adm1 = state,
#'   adm2 = city,
#'   "Toronoto" = regions$adm2$Canada$Ontario$Toronto,
#'   "Calagry"  = regions$adm2$Canada$Alberta$Calgary
#' )
#'
st_locate <- function(data, adm0, adm1, adm2 = NULL, ...,
                      into = c("adm1", "adm2"), n_unmatched_printed = 20) {

  ## Missing req args --------------------------------------------------------

  required_args <- c("data", "adm0", "adm1")
  index <- c(missing(data), missing(adm0), missing(adm1))
  if(any(index)) {
    missing_arg <- required_args[index]
    cli::cli_abort(paste("{.arg {missing_arg}} argument{?s} {?is/are}",
                         "required but missing."))
  }

  ## Check arguments ---------------------------------------------------------

  assert("data.frame" %in% class(data), "`data` must be of class 'data.frame'.")

  assert(length(adm0) == 1, "Only one adm0 (country) value is acceptable.")
  assert({{adm0}} %in% regions_df$shapeID_v5[regions_df$shapeType=="ADM0"],
         paste0("`adm0` must be a region code. Use `serotrackr::regions$",
                "adm0$YourCountry` to assign your country code."))

  assert(is.character(into) && length(into) <= 2,
         "`into` must be a character vector of length 1 or 2.")

  assert(rlang::is_bare_numeric(n_unmatched_printed, n=1),
         "`n_unmatched_printed` must be a number.")

  ## Col vs single val -------------------------------------------------------

  data_name <- deparse(substitute(data))

    ### adm1 --------------------------------------------------------------------

    if (is.name(substitute(adm1))) {
      col_name <- deparse(substitute(adm1))
      if (!col_name %in% names(data)) {
        cli::cli_abort(colNotFound(col_name, data_name))
      } else {
        if (!is.character(data[[col_name]])) {
          cli::cli_abort(wrongType("adm1"))
        } else {adm1_df <- dplyr::mutate(data, adm1 = {{adm1}}, .keep = "none")}
      }
    }
    else if (!is.name(substitute(adm1)) && !is.null(substitute(adm1))) {
      if (!rlang::is_string(adm1)) {
        cli::cli_abort(wrongType("adm1"))
      } else {
        adm1_df <- dplyr::tibble(adm1 = rep({{adm1}}, times = nrow(data)))
      }
    }
    else if (is.null(substitute(adm1))) {
      cli::cli_abort("`adm1` can't be NULL.")
    }

    ### adm2 --------------------------------------------------------------------

    if (is.name(substitute(adm2))) {
      col_name <- deparse(substitute(adm2))
      if (!col_name %in% names(data)) {
        cli::cli_abort(colNotFound(col_name, data_name))
      } else {
        if (!is.character(data[[col_name]])) {
          cli::cli_abort(wrongType("adm2"))
        } else {adm2_df <- dplyr::mutate(data, adm2 = {{adm2}}, .keep = "none")}
      }
    }
    else if (!is.name(substitute(adm2)) && !is.null(substitute(adm2))) {
      if (!rlang::is_string(adm2)) {
        cli::cli_abort(wrongType("adm2"))
      } else {
        adm2_df <- dplyr::tibble(adm2 = rep({{adm2}}, times = nrow(data)))
      }
    }
    else if (is.null(substitute(adm2))) {
      adm2_df <- dplyr::tibble(adm2 = NA_character_)
    }


  adm_df <- dplyr::bind_cols(adm1_df, adm2_df)


  ## Match with `regions_df` -------------------------------------------------

  adm_df <- add_joined_region_codes(adm_df, adm0_code = adm0)

  ## Ellipsis ----------------------------------------------------------------

  ellipsis <- list(...)
  lhs <- names(ellipsis)
  rhs <- unname(unlist(ellipsis))

  if (length(lhs) > 0) {
    if (is.name(substitute(adm1)) || is.name(substitute(adm2))) {
      adm_df <- evaluate_lhs_rhs(
        data = adm_df, lhs = lhs, rhs = rhs, adm0_code = adm0,
        adm1 = deparse(substitute(adm1)), adm2 = deparse(substitute(adm2)),
        error_call = rlang::current_env()
      )
    } else {
      cli::cli_abort(paste("When using the ellipsis argument, `adm1` or `adm2`",
                           "must be a column name, not a single region code."))
    }
  }

  ## Index sets --------------------------------------------------------------

  # Error types:
  # 1) adm1 is not among predefined adm1s for this adm0
  indx_adm1_notPreset <- which(!is.na(adm_df$adm1) & is.na(adm_df$adm1_code))
  # 2) adm2 cannot be processed until adm1 is provided
  indx_adm1_mising <- which(is.na(adm_df$adm1) & !is.na(adm_df$adm2))
  # 3) adm2 is not among predefined adm2s for this adm1
  indx_adm2_notPreset <- which(
    !is.na(adm_df$adm1) & !is.na(adm_df$adm2) & !is.na(adm_df$adm1_code) &
      is.na(adm_df$adm2_code)
  )

  ## Final message -----------------------------------------------------------

  msg_final <- msg_adm(
    adm_df, indx_adm1_notPreset = indx_adm1_notPreset,
    indx_adm1_mising=indx_adm1_mising, indx_adm2_notPreset=indx_adm2_notPreset,
    into = into, n_unmatched_printed = n_unmatched_printed
  )
  cli::cli_verbatim(msg_final)

  ## Output data -------------------------------------------------------------

  if (is.null(substitute(adm2))) {
    if (length(indx_adm1_notPreset) == 0) {
      dplyr::mutate(data, "{into[1]}" := adm_df$adm1_code)
    }
  } else {
    if (length(indx_adm1_notPreset) == 0 &&
        length(indx_adm1_mising)    == 0 &&
        length(indx_adm2_notPreset) == 0) {
      dplyr::mutate(data,
                    "{into[1]}" := adm_df$adm1_code,
                    "{into[2]}" := adm_df$adm2_code)
    }
  }
}




# Helper functions --------------------------------------------------------

#' Text when column is not found
#'
#' @param col_name Column name supplied by user for this `arg`. Can be supplied
#' using `deparse(substitute(argument))`.
#' @param data_name Name of the raw data object supplied by user. Can be
#' supplied using `deparse(substitute(data))`.
#'
#' @return A string saying that the column was not found.
#' @noRd
#'
colNotFound <- function(col_name, data_name) {
  paste0("Column `", col_name, "` doesn't exist in `", data_name, "`.")
}




#' Text when the data type is incorrect
#'
#' @param arg A string, either "adm1" or "adm2".
#'
#' @return A string saying that the data type is incorrect.
#' @noRd
#'
wrongType <- function(arg = c("adm1", "adm2")) {
  arg <- match.arg(arg)
  paste(arg, "must be an unquoted name of a character column or a single",
        "region code defined by `serotrackr::region`.")
}




#' @title Match region names and add corresponding region codes
#' @description
#' This function gets the `adm1` and `adm2` columns of the `data`, tries to
#'  match their region names (and also codes) to the predefined region names in
#'  `serotrackr::regions_df`, and then adds two columns containing the
#'  corresponding region codes of those region names. This matching is exact
#'  and case insensitive. It also works around accented letters.
#'
#' @param data A dataframe, containing at least two columns naming `adm1` and
#'  `adm2`. These two columns can containg region names or region codes.
#' @param adm0_code A string representing one country (adm0) code. Use
#'  `serotrackr::regions` to select it.
#'
#' @return A data.frame that consists of the input data.frame plus two new
#'  columns containing adm1 (and adm2) region codes. The new columns are
#'  named `adm1_code` and `adm2_code`. If the input `adm2` column is all NA,
#'  the `adm2_code` column will also be all NA.
#' @noRd
#'
add_joined_region_codes <- function(data, adm0_code) {

  assert(all(c("adm1", "adm2") %in% names(data)),
         "`data` must have at least two columns naming `adm1` and `adm2`.")

  country <- regions_df$NAME_0[regions_df$shapeID_v5 == adm0_code]

  ## adm1 --------------------------------------------------------------------

  adm1_source_df <- dplyr::filter(regions_df, NAME_0==country, shapeType=="ADM1")

  adm1_joined_df <- dplyr::left_join(
    dplyr::mutate(
      data,
      # Convert strings to lower case and accents to non-accented letters:
      adm1 = stringi::stri_trans_general(adm1, id = "Lower; Latin-ASCII"),
      .keep = "none"
    ),
    dplyr::mutate(
      adm1_source_df,
      NAME_1 = stringi::stri_trans_general(NAME_1, id = "Lower; Latin-ASCII")
    ),
    by = c("adm1" = "NAME_1")
  )
  adm1_already_a_code <- data$adm1 %in% adm1_source_df$shapeID_v5
  adm1_joined_df <- dplyr::mutate(
    adm1_joined_df,
    shapeID_v5 = dplyr::if_else(adm1_already_a_code, data$adm1, shapeID_v5)
  )
  adm1_code <- dplyr::select(adm1_joined_df, adm1_code = shapeID_v5)

  data_w_adm1_code <- dplyr::bind_cols(data, adm1_code)

  ## adm2 --------------------------------------------------------------------

  data_w_adm1_code_id <- dplyr::mutate(data_w_adm1_code, id=dplyr::row_number())

  unique_adm1_code <- unique(stats::na.omit(data_w_adm1_code_id$adm1_code))
  adm2_code <- dplyr::tibble()

  for (i in 1:length(unique_adm1_code)) {
    unique_adm1_name <-
      regions_df$NAME_1[regions_df$shapeID_v5 == unique_adm1_code[i]]

    adm2_source_df <- dplyr::filter(regions_df, NAME_0 == country,
                                    NAME_1==unique_adm1_name, shapeType=="ADM2")

    i_data_w_adm1_code_id <- dplyr::select(
      dplyr::filter(data_w_adm1_code_id, adm1_code == unique_adm1_code[i]),
      adm2, id
    )

    adm2_joined_df <- dplyr::left_join(
      dplyr::mutate(
        i_data_w_adm1_code_id,
        adm2 = stringi::stri_trans_general(adm2, id = "Lower; Latin-ASCII")
      ),
      dplyr::mutate(
        adm2_source_df,
        NAME_2 = stringi::stri_trans_general(NAME_2, id = "Lower; Latin-ASCII")
      ),
      by = c("adm2"="NAME_2")
    )

    adm2_already_a_code <- i_data_w_adm1_code_id$adm2 %in% adm2_source_df$shapeID_v5
    adm2_joined_df <- dplyr::mutate(
      adm2_joined_df,
      shapeID_v5 = dplyr::if_else(adm2_already_a_code,
                                  i_data_w_adm1_code_id$adm2, shapeID_v5)
    )

    adm2_code <- dplyr::bind_rows(adm2_code,
                                  dplyr::select(adm2_joined_df, id, shapeID_v5))
  } # for loop closes

  adm2_code <- dplyr::bind_rows(
    adm2_code,
    dplyr::tibble(id = setdiff(data_w_adm1_code_id$id, adm2_code$id),
                  shapeID_v5 = NA_character_)
  )
  adm2_code <- dplyr::select(dplyr::arrange(adm2_code,id), shapeID_v5)

  ## bind data ---------------------------------------------------------------

  dplyr::mutate(data_w_adm1_code, adm2_code = adm2_code$shapeID_v5)
}




#' @title Evaluate LHS and RHS
#' @description
#' This function first validates LHS and RHS values and outputs error messages
#'  if they are invalid. If both LHS and RHS are valid, this function adds the
#'  region codes defined in RHS to their corresponding region names, defined in
#'  LHS, to the input data.
#'
#' @param data A dataframe containing at least four columns naming `adm1`,
#'  `adm2`, `adm1_code`, and `adm2_code`. It is meant to be the output of
#'  `add_joined_region_codes()`.
#' @param lhs A character vector of all left-hand side values given to
#'  `st_validate()`'s ellipsis argument, i.e. all `region_names` entered there.
#' @param rhs A character vector of all right-hand side values given to
#'  `st_validate()`'s ellipsis argument, i.e. all `region_codes` entered there.
#' @param adm0_code A string representing one country (adm0) code. Use
#'  `serotrackr::regions` to select it.
#' @param adm1,adm2 A string, representing user's adm1 and/or adm2 column
#'  names. These can be provides as `deparse(substitute(adm1))`.
#' @param error_call The environment in which this function must be assessed.
#'  Use `rlang::current_env()` to execute the error call stack at the level of
#'  the running function. If not specified, the `cli::cli_abort()` error
#'  messages refer to the internal function, instead of the exported function.
#'
#' @return A data.frame similar to the input data.frame but with added adm1 and
#'  adm2 codes only for the region names specified by the `lhs` argument.
#'
#' @noRd
#'
evaluate_lhs_rhs <- function(data, lhs, rhs, adm0_code, adm1, adm2,
                             error_call) {

  ## Check arguments ---------------------------------------------------------

  assert("data.frame" %in% class(data), "`data` must be of class 'data.frame'.")
  assert(all(c("adm1", "adm2", "adm1_code", "adm2_code") %in% names(data)),
         paste("`data` must have at least four columns naming `adm1`, `adm2`,",
               "`adm1_code`, and `adm2_code`."))
  assert(is.character(lhs) && is.character(rhs),
         "`lhs` and `rhs` each must be a character vector.")

  assert(rlang::is_string(adm0_code),
         "`adm0_code must be a string containing the country code.`")

  assert(rlang::is_string(adm1),
         "`adm1` must be the name of the column containing adm1 values.")
  assert(rlang::is_string(adm2) || is.null(adm2),
         "`adm2` must be the name of the column containing adm2 values.")

  country <- regions_df$NAME_0[regions_df$shapeID_v5 == adm0_code]

  ## Check rhs is valid ------------------------------------------------------

  rhs_ref <- dplyr::filter(
    regions_df, NAME_0 == country, shapeType %in% c("ADM1", "ADM2")
  )[["shapeID_v5"]]

  rhs_is_valid <- rhs %in% rhs_ref
  rhs_invalid_index <- which(!rhs_is_valid)
  rhs_invalid_n <- length(rhs_invalid_index)
  if (rhs_invalid_n > 0) {
    cli::cli_abort(
      paste("Failed to evaluate the right-hand side (rhs) of the ellipsis",
            "argument. Use `serotrackr::regions` to define each rhs value in",
            "your country. {rhs_invalid_n} invalid value{?s}:",
            "{.val {rhs[rhs_invalid_index]}}"),
      call = error_call
    )
  } else {
    rhs_adm_level <- character(0)
    for (i in 1:length(rhs)) {
      rhs_adm_level[i] <- tolower(
        dplyr::filter(regions_df, shapeID_v5 == rhs[i])[["shapeType"]]
      )
    }
    if ("adm2" %in% rhs_adm_level && is.null(substitute(adm2))) {
      cli::cli_abort(paste("`adm2` argument must be provided when an adm2",
                           "region is defined in the ellipsis argument."),
                     call = error_call)
    }
  }

  ## Check lhs is valid ------------------------------------------------------

  if (rhs_invalid_n == 0) {
    lhs_is_valid <- logical(0)
    for (i in 1:length(lhs)) {
      lhs_is_valid[i] <- lhs[i] %in% data[[rhs_adm_level[i]]]
    }

    lhs_invalid_index <- which(!lhs_is_valid)
    lhs_invalid_n <- length(lhs_invalid_index)
    if (lhs_invalid_n > 0) {
      lhs_invalid_col <- unlist(mget(unique(rhs_adm_level[lhs_invalid_index])))
      cli::cli_abort(
        paste("Failed to evaluate the left-hand side of the ellipsis",
              "argument. Couldn't find {cli::qty(lhs_invalid_n)} {?this/these}",
              "{.val {lhs_invalid_n}} value{?s} in the",
              "`{.emph {lhs_invalid_col}}` column{?s}:",
              "{.val {lhs[lhs_invalid_index]}}"),
        call = error_call
      )
    }
  }

  ## Add new vals to data ----------------------------------------------------

  if (rhs_invalid_n == 0 && lhs_invalid_n == 0) {
    for (i in 1:length(lhs)) {
      data <- dplyr::mutate(
          data,
          "{rhs_adm_level[i]}_code" := dplyr::case_when(
            get(rhs_adm_level[i]) == lhs[i] ~ rhs[i],
            TRUE ~ get(paste0(rhs_adm_level[i], "_code"))
          )
        )
    }
  }
  data
}




#' Text for final adm1 and adm2 messages
#'
#' @param data A dataframe containing at least four columns naming `adm1`,
#'  `adm2`, `adm1_code`, and `adm2_code`.
#' @param indx_adm1_notPreset An integer vector containing row numbers where
#'  the user's adm1 is not among the predefined adm1s (for the specified adm0).
#' @param indx_adm1_mising An integer vector containing row numbers where the
#'  user's adm2 cannot be processed because user's data is missing adm1.
#' @param indx_adm2_notPreset An integer vector containing row numbers where
#'  the user's adm2 is not among the predefined adm2s for the specified adm1.
#' @param into A character vector of length 1 or 2, specifying the name(s) of
#'  the new columns that are added to your data for adm1 (and adm2) region
#'  codes. If length is one, name of the adm1 column will be assigned. If
#'  length is two, names of adm1 and adm2 columns will be assigned,
#'  respectively.
#' @param n_unmatched_printed A single number, indicating the quantity of
#'  unmatched unique region names that are printed to the console. This argument
#'  is implemented to prevent potentially flooding your console with unmatched
#'  region names. The default value is 20. If there are more than 20 unmatched
#'  region names in your data, increase this number to see the rest.
#'
#' @return A character vector containing the styled final message (success or
#' error) for adm1 and adm2. Use `cli::cli_verbatim()` to print this message
#' to the console while preserving its formatting.
#' @noRd
#'
msg_adm <- function(data, indx_adm1_notPreset, indx_adm1_mising,
                    indx_adm2_notPreset, into, n_unmatched_printed) {
  assert("data.frame" %in% class(data), "`data` must be of class 'data.frame'.")
  assert(all(c("adm1", "adm2", "adm1_code", "adm2_code") %in% names(data)),
         paste("`data` must have at least four columns naming `adm1`, `adm2`,",
               "`adm1_code`, and `adm2_code`."))

  ## msg adm1 ----------------------------------------------------------------

  if (length(indx_adm1_notPreset) == 0) {
    msg_adm1 <- cli::format_inline(
      paste("region names/codes were successfully matched. `{into[1]}`",
            "column was added.")
    )
  } else {
    list_adm1_notPreset <- cli::cli_vec(
      x = unique(data$adm1[indx_adm1_notPreset]),
      style = list('vec-trunc'=n_unmatched_printed, 'vec-trunc-style'='head')
    )

    n_unique_adm1 <- length(unique(stats::na.omit(data$adm1)))
    n_unique_adm1_notPreset <- length(unique(data$adm1[indx_adm1_notPreset]))

    msg_adm1 <- cli::format_inline(
      paste("{.val {n_unique_adm1_notPreset}} of {.val {n_unique_adm1}} unique",
            "adm1 region name{?s}/code{?s} {cli::qty(n_unique_adm1_notPreset)}",
            "{?was/were} not matched. Use the ... argument in {.fun st_locate}",
            "to define {?it/them}. Unmatched value{?s} {?is/are}:",
            "{.val {list_adm1_notPreset}}.")
    )
  }

  ## msg adm2 ----------------------------------------------------------------

  if (sum(is.na(data$adm2)) != nrow(data)) {
    if (length(indx_adm1_mising) == 0 && length(indx_adm2_notPreset) == 0) {
      msg_adm2 <- cli::format_inline(
        paste("region names/codes were successfully matched. `{into[2]}`",
              "column was added.")
      )
    } else {
      msg_adm2 <- character(0)

      if (length(indx_adm1_mising) > 0) {
        list_adm1_mising <- cli::cli_vec(
          x = indx_adm1_mising,
          style = list('vec-trunc'=n_unmatched_printed, 'vec-trunc-style'='head')
        )
        msg_adm2 <- cli::format_inline(
          paste("`adm2` values in the following row numbers cannot be evaluated",
                "because their corresponding `adm1` is missing:",
                "{.val {indx_adm1_mising}}.")
        )
      }

      if (length(indx_adm2_notPreset) > 0) {
        list_adm2_notPreset <- cli::cli_vec(
          x = unique(data$adm2[indx_adm2_notPreset]),
          style = list('vec-trunc'=n_unmatched_printed, 'vec-trunc-style'='head')
        )

        n_unique_adm2 <- length(unique(stats::na.omit(data$adm2)))
        n_unique_adm2_notPreset <- length(unique(data$adm2[indx_adm2_notPreset]))

        msg_adm2 <- c(
          msg_adm2,
          cli::format_inline(
            paste(
              "{.val {n_unique_adm2_notPreset}} of {.val {n_unique_adm2}} unique",
              "adm2 region name{?s}/code{?s} {cli::qty(n_unique_adm2_notPreset)}",
              "{?was/were} not matched. Use the ... argument in {.fun st_locate}",
              "to define {?it/them}. Unmatched value{?s} {?is/are}:",
              "{.val {list_adm2_notPreset}}."
            )
          )
        )
      }
    }
  }

  ## Merge both msgs ---------------------------------------------------------

  msg_final <- c(
    if (length(indx_adm1_notPreset) == 0) {
      msg_result("adm1", msg_adm1, error = FALSE)
    } else {msg_result("adm1", msg_adm1)},

    if (sum(is.na(data$adm2)) != nrow(data)) {
      if (length(indx_adm1_mising) == 0 && length(indx_adm2_notPreset) == 0) {
        msg_result("adm2", msg_adm2, error = FALSE)
      } else {msg_result("adm2", msg_adm2)}
    }
  )
  msg_final
}

