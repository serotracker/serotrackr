
# Prerequisites -----------------------------------------------------------

library(dplyr)
library(stringr)
library(devtools)
load_all()
sample_raw_data


# TO DO -------------------------------------------------------------------

# TODO
# Allow these varaibles to be assigned one value each:
#  adm1, adm2, age_group, age, sex, collection_start_date, collection_end_date
# Check if test_id is among the allowed values
# Check if test_id is among the allowed values
# If multiple tests are used, make sure same id is used for each unique person
# Add progress bar
# Explore creating a specific class to make sure next function directly runs
# the output of this function

# TODO new process:
# 1) Combined map_cols() with clean().
# 2) User first calls map_cols and gets a detailed explanation about each
# argument/column in the console with green check marks and red alert signs.
# 3) User can go back and use regions_when() and assays_when() functions to
# clean their regions and assays.
# 4) User calls map_cols() again. When all argument/columns are checked, a
# message saying 'the validated dataset is created' will be showed.
# 5) Next functions, generate_estimates() and save_xlsx(), will check for all
# specifications of a validated dataset first, and then go ahead with their
# tasks.


# map_cols() --------------------------------------------------------------

# TODO id must be required!
map_cols <- function(data,
                     adm0,
                     collection_start_date,
                     collection_end_date,
                     test_id,
                     result,
                     dataset_id = NULL,
                     id = NULL,
                     age_group = NULL,
                     age = NULL,
                     sex = NULL,
                     adm1 = NULL,
                     adm2 = NULL,
                     result_cat = NULL,
                     # test_cutoff = NULL,
                     include_others = TRUE) {
  cli::cli_h1("Mapping columns")

  stopifnot("Data must be of class 'data.frame'" = "data.frame" %in% class(data))

  cli:: cli_progress_bar("Mapping columns", total = 100)


  # Check if required arguments are missing ---------------------------------

  required_args <- c("data", "adm0", "collection_start_date",
                     "collection_end_date", "test_id", "result")
  index <- c(missing(data), missing(adm0), missing(collection_start_date),
             missing(collection_end_date), missing(test_id), missing(result))
  if(any(index)) {
    missing_arg <- required_args[index]
    cli::cli_abort(paste("{.field {missing_arg}} argument{?s} {?is/are}",
                         "required but missing."))
  }


  # Define sets of columns --------------------------------------------------

  cli:: cli_progress_step("Define sets of columns", spinner = TRUE)

  raw_cols <- names(data)

  arg_cols <- sapply(
    substitute(c(dataset_id, id, age_group, age, sex, adm1, adm2,
                 collection_start_date, collection_end_date, result, result_cat
                 # test_cutoff
    )),
    deparse
  )[-1]

  # Null columns. 'dataset_id' and 'id' will be generated if NULL, thus they're
  # not included in the list below:
  NULL_cols <- arg_cols[which(
    sapply(arg_cols, function(x) x == "NULL") &
      !names(arg_cols) %in% c("dataset_id", "id")
  )]

  main_cols <- c("dataset_id", "id", "age_group", "age", "sex", "adm0", "adm1",
                 "adm2", "collection_start_date", "collection_end_date",
                 "test_id", "result", "result_cat")


  country <- regions_df %>%
    dplyr::filter(shapeID_v5 == {{adm0}}) %>%
    dplyr::pull(NAME_0)

  # To prevent conflict between the Template's column names with the user's
  # column names, each renamed or mutated column will be stored separately
  # and column-bound in the end:

  # age_group ---------------------------------------------------------------

  cli:: cli_progress_step("age_group", spinner = TRUE)

  # age_group as a column name:
  if (is.name(substitute(age_group))) {

    # Add typeof(x)

    df_age_group <- data %>% dplyr::select({{age_group}}) %>%
      dplyr::rename(age_group = {{age_group}})
  }
  # age_group as a string:
  else if (is.character(substitute(age_group))) {
    # Check if structure is like 18-64:
    if (grepl("^\\d+-\\d+$", {{age_group}})) {
      upper_age <- stringr::str_extract({{age_group}}, "(?<=\\-).*$") %>%
        as.numeric()
      lower_age <- stringr::str_extract({{age_group}}, "^.*(?=(\\-))") %>%
        as.numeric()
      if (upper_age < lower_age) {
        cli::cli_abort(c("x" = paste("In `.emph age_group`, upper bound should",
                                     "be larger than the lower bound")))
      }
      else {
        df_age_group <- data %>% dplyr::mutate(age_group = {{age_group}},
                                               .keep = "none")
      }
    }
    # Check if structure is like 65+:
    else if (grepl("^\\d+\\+$", {{age_group}})) {
      df_age_group <- data %>% dplyr::mutate(age_group = {{age_group}},
                                             .keep = "none")
    }
    else {
      cli::cli_abort(c("x" = paste("When `{.emph age_group}` is a string, it",
                                   "should have one of these two structures:",
                                   "`{.emph number-number}` or",
                                   "`{.emph number+}`; e.g `18-64` or `65+`")))
    }
  }
  # When age_group is not a column name, string, or NULL:
  else if (!is.null(age_group)) {
    cli::cli_abort(c("x" = paste("`{.emph age_group}` must be an unquoted",
                                 "column name or a string")))
  }


  # age ---------------------------------------------------------------------

  cli:: cli_progress_step("age", spinner = TRUE)

  # age as a column name:
  if (is.name(substitute(age))) {
    if (deparse(substitute(age)) %in% names(data)) {
      if (is.numeric(data[[deparse(substitute(age))]])) {
        n_age120 <- data %>% filter({{age}} > 120) %>% nrow()
        if (n_age120 == 0) {
          df_age <- data %>% dplyr::select({{age}}) %>% dplyr::rename(age={{age}})
        }
        else {
          cli::cli_abort(c("x"=paste("{.emph {n_age120}} value{?s} in the",
                                     "`{.emph {deparse(substitute(age))}}`",
                                     "column {cli::qty(n_age120)} {?is/are}",
                                     "above 120 years. You have probably",
                                     "chosen the wrong column.")))
        }
      }
      else {cli::cli_abort(c("x" = paste("`{.emph {deparse(substitute(age))}}`",
                                         "must be a numeric column.")))}
    }
    else {
      cli::cli_abort(c("x"=paste("Column `{.emph {deparse(substitute(age))}}`",
                                 "doesn't exist in",
                                 "`{.emph {deparse(substitute(data))}}`.")))
    }
  }
  # age as a number:
  else if (is.numeric(substitute(age))) {
    if (is.infinite(substitute(age))) {
      cli::cli_abort(c("x" = "`age` can't be infinite!"))
    }
    else if ({{age}} > 120) {cli::cli_abort(c("x" = "`age` is too large!"))}
    else {df_age <- data %>% dplyr::mutate(age={{age}}, .keep = "none")}
  }
  # When age is not a column name, number, or NULL:
  else if (!is.null(age)) {
    cli::cli_abort(c("x" = paste("`age` must be a column name or a single",
                                 "positive number")))
  }


  # Template ----------------------------------------------------------------

  # # age as a column name:
  # if (is.name(substitute(age))) {
  #   data <- data %>% dplyr::rename(age = {{age}})
  # }
  # # age as a number:
  # else if (is.numeric(substitute(age))) {
  #   data <- data %>% dplyr::mutate(age = {{age}})
  # }
  # # When age is not a column name, number, or NULL:
  # else if (!is.null(age)) {
  #   cli::cli_abort(c("x" = paste("`age` must be a column name or a single",
  #                                "positive number")))
  # }

  cli:: cli_progress_step("Generating data", spinner = TRUE)

  dfs_all <- c("df_age_group", "df_age")
  dfs_to_bind <- dfs_all[which(dfs_all %in% ls())]

  data <- data %>%
    dplyr::mutate(
      # ifelse() can't be used below as it returns output of same length as condition
      dataset_id = if(is.null({{dataset_id}})) 1L else {{dataset_id}},
      id = if(is.null({{id}})) dplyr::row_number() else {{id}},
      # age_group = {{age_group}},
      # age = {{age}},
      sex = {{sex}},
      adm0 = country,
      adm1 = {{adm1}},
      adm2 = {{adm2}},
      collection_start_date = {{collection_start_date}},
      collection_end_date = {{collection_end_date}},
      test_id = {{test_id}},
      result = {{result}},
      result_cat = {{result_cat}},
      # test_cutoff = {{test_cutoff}}
    ) %>%
    dplyr::select(-any_of(c("age_group", "age"))) %>%
    list() %>% c(mget(dfs_to_bind, inherits = TRUE)) %>% dplyr::bind_cols()



  states <- regions_df %>%
    dplyr::filter(NAME_0 == country, shapeType == "ADM1") %>%
    dplyr::pull(NAME_1)

  cities <- regions_df %>%
    dplyr::filter(NAME_0 == country, shapeType == "ADM2") %>%
    dplyr::pull(NAME_2)

  # if (!"adm1" %in% names(NULL_cols)) {
  # if (!is.null(adm1)) {
  #   warn_states <- setdiff(unique(data$adm1), states)
  # }
  # # if (!"adm2" %in% names(NULL_cols)) {
  # if (!is.null(adm2)) {
  #   warn_cities <- setdiff(unique(data$adm2), cities)
  # }

  warn_states <- setdiff(unique(data$adm1), states)
  warn_cities <- setdiff(unique(data$adm2), cities)

  # Warning messages for region names
  # if (exists("warn_states") | exists("warn_cities")) {

    if (length(warn_states) > 0L | length(warn_cities) > 0L) {
      cli::cli_warn(c(
        "!" = paste(
          "The following administrative",
          paste(na.omit(c(
            ifelse(length(warn_states)>0L, "level 1 (state/province)", NA),
            ifelse(length(warn_cities)>0L, "level 2 (district/municipality)", NA)
          )), collapse = " and "),
          "regions in your raw data do not match SeroTracker's predefined region",
          "names. Please use the `{.pkg seotrackr}::regions` object and",
          "functions like `ifelse()` or `{.pkg dplyr}::case_when()` to",
          "rename your regions according to names acceptable by SeroTracker:"
        ),
        "x" = if (length(warn_states) > 0L) {
          "ADM1 regions not matching: {.field {warn_states}}"
        },
        "x" = if (length(warn_cities) > 0L) {
          "ADM2 regions not matching: {.field {warn_cities}}"
        }
      ))
    }

  # }

  cli:: cli_progress_step("include_others", spinner = TRUE)

  if(isFALSE(include_others)) {
    data <- data %>%
      # dplyr::select(!!!main_cols)
      dplyr::select(!!!any_of(c(setdiff(main_cols, NULL_cols))))
  } else if (isTRUE(include_others)) {
    data <- data %>%
      # dplyr::select(any_of(c(!!!main_cols, setdiff(raw_cols, arg_cols))))
      dplyr::select(!!!any_of(c(setdiff(main_cols, NULL_cols),
                                setdiff(raw_cols, arg_cols))))
  }

  return(tibble(data))
}


mydf <- sample_raw_data %>% dplyr::mutate(new = NA) %>%
  dplyr::mutate(state = dplyr::case_when(
    state == "bc" ~ "British Columbia", state == "Québec" ~ "Quebec", TRUE~state
  )) %>%
  dplyr::mutate(city = dplyr::case_when(
    city == "calgary" ~ "Calgary", city == "Metro vancouver" ~ "Toronto",
    city == "Montréal" ~ "MontrÃ©al"
  )) %>%
  dplyr::mutate(dataset_id = 1234, age_indiv = c(120, 120, 121))

# d <- rep("mydf", times = 1000000)
# mylargedf <- bind_rows(mget(d))


# Example run -------------------------------------------------------------

map_cols(a, #dataset_id = dataset_id, id = id,
         # age_group = "18-64", # sex = sex,
         # age = 14,
         adm0 = regions$adm0$Canada,
         # adm1 = state, adm2 = city,
         collection_start_date = collection_start_date,
         collection_end_date = collection_end_date,
         test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
         result = result,
         # result_cat = result_cat,
         include_others = FALSE)

map_cols(a, adm0 = regions$adm0$Iraq)


# Testing -----------------------------------------------------------------


# mydf2 <- tibble(orig_age = c(23, 45, 67), orig_sex = c("M", "F", "F"))
mydf2 <- tibble(
  orig_age = sample.int(100, 100000000, replace = TRUE),
  orig_sex = sample(c("Male", "Female", "Other"), 100000000, replace = TRUE)
)


custom_msg <- function(arg, msg, error = FALSE) {
  paste(
    ifelse(error,
           paste(cli::col_red(cli::symbol$cross), cli::bg_red(arg)),
           paste(cli::col_green(cli::symbol$tick), cli::bg_green(arg))),
    ifelse(error,
           cli::col_none(cli::format_inline(msg)),
           cli::col_grey(cli::format_inline(msg)))
  )
}

# Based on cli::cli_progress_step(). Changes are commented below:
# Reza changed the line below:
my_progress <- function (msg, msg_done, error = FALSE, msg_failed = msg,
                         spinner = FALSE, class = if (!spinner) ".alert-info",
                         current = TRUE, .auto_close = TRUE,
                         .envir = parent.frame(), ...)
{
  format <- paste0(if (!is.null(class)) paste0("{", class, " "),
                   if (spinner) "{cli::pb_spin} ",
                   msg, " ...",
                   if (!is.null(class)) "}")
  # Reza changed the two lines below:
  ts <- cli::col_cyan(" [{cli::pb_elapsed}]")
  # format_done <- paste0(msg_done, ts)
  format_done <- paste0(custom_msg(msg, msg_done, error), ts)
  format_failed <- paste0("{.alert-danger ", msg_failed, ts, "}")
  opt <- options(cli.progress_show_after = 0)
  on.exit(options(opt), add = TRUE)
  id <- cli::cli_progress_bar(
    type = "custom", format = format, format_done = format_done,
    format_failed = format_failed, clear = FALSE, current = current,
    .auto_close = .auto_close, .envir = .envir, ...
  )
  cli::cli_progress_update(id = id, force = TRUE, .envir = .envir)
  invisible(id)
}


# REPLACE `is.name(substitute(age))` WITH `quo_is_symbol()`
# because myfun() currently takes a long time to assess `age`.

myfun <- function(data, age, sex) {

  cli::cli_h1(cli::col_cyan("Mapping and validating columns"))
  error_count <- 0

  # age ---------------------------------------------------------------------

  if (rlang::is_symbol(age)) {
  # if (is.name(substitute(age))) {
    data <- data %>% dplyr::rename(new_age = {{age}})
    my_progress('age', 'is a column name')

  } else if (rlang::is_double(age, n=1)) {
  # } else if (is.numeric(substitute(age))) {
    data <- data %>% dplyr::mutate(new_age = {{age}})
    my_progress('age', 'is a number')

  } else if (!is.null(age)) {
    my_progress('age', paste0('must be a number or column name; not ',
                              typeof(age), '.'), error = TRUE)
    error_count <- error_count + 1
  }
  cli::cli_progress_update()

  # sex ---------------------------------------------------------------------

  if (is.name(substitute(sex))) {
    data <- data %>% dplyr::rename(new_sex = {{sex}})
    my_progress("sex", "is a column name")

  } else if (is.character(substitute(sex))) {
    data <- data %>% dplyr::mutate(new_sex = {{sex}})
    my_progress("sex", "is a string")

  } else if (!is.null(sex)) {
    my_progress("sex", "must be a string or column name.", error = TRUE)
    error_count <- error_count + 1
  }
  cli::cli_progress_update()

  # Result ------------------------------------------------------------------

  cli::cli_progress_done()
  cli::cli_h1(cli::col_cyan("Validation finished"))
  if (error_count > 0) {
    cli::cli_abort(
      paste(cli::col_red("{error_count} error{?s}!"),
            "Please address {?it/them} first. Validated df not created.")
    )
  }
  else {
    cli::cli_text(paste(cli::col_green("Success!"), "Validated df created."))
    cli::cli_par(); cli::cli_end()
    return(data)
  }
}


myfun(mydf2, age = "32", sex = "F") %>% dplyr::select(orig_age)
myfun(mydf2, sex = "Male")
myfun(mydf2)

rlang::quo_is_symbol()
rlang::is_string(c("t", "r"))
rlang::is_double(c(2, 3), n=1)

a <- function(age) {
  # age <- enquo(age)
  # if (rlang::is_double(age, n=1)) {print("Working")}
  if (rlang::is_symbol(age)) {print("Working")}
  else {print("Not working")}
}
a(age = c(32, 2))
a(age = v)

# Add github logo and url to pkgdown
# Add source link to each vignette
# Add progress bar or equivalent

filter
abort()
quo_is_null()
rlang::quo_is_call()


demo_spinners(which = "simpleDots")



# S3 and S4 ---------------------------------------------------------------

setClass("Person", representation(name = "character", age = "numeric"))
hadley <- new("Person", name = "Hadley", age = 31)
class(hadley)
typeof(hadley)
rules
slot(hadley, "age")
