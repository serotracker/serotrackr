
# TODO Address R CMD check warnings and notes
# TODO add functionality to `test_combination` and `ci_type` arguments
# TODO fix case insensitivity for result_cat in st_validate()
# TODO drop_na for `result` should be separated and done at a later stage
# FIXME Should ab_titer_unit remain an optional field?
# FIXME when there's only one age_group/sex, st_aggregate should not generate
# age_group/sex subgroups.


#' Aggregate validated data
#' @description
#' `r lifecycle::badge("experimental")`
#' Generate aggregate estimates based on validated individual level data.
#' @param data A validated data.frame that is the output of `st_validate()`
#' @param subgroup A character vector of subgrouping variables. By default,
#'  aggregate estimates are generated for the overall data, as well as
#'  age group, sex, and age group + sex subgroups.
#' @param borderline How should borderline results be treated? Default is as
#'  negative.
#' @param add_ci Boolean. Whether to add binomial proportion confidence interval.
#'  It is calculated using the Wilson score interval method through the
#'  `binom::binom.confint()` function.
#' @param round_digits Integer indicating the number of decimal places of the
#'  estimate. It is passed to the digits argument of `base::round()`.
#' @param test_combination Not functional yet. When data is based on more than
#'  one assay, what is the relationship between those assays?
#'
#' @return A summarized data.frame
#' @export
#'
#' @examples
#' new_raw_data <- dplyr::mutate(
#'   sample_raw_data,
#'   age_group = rep(c("0-9", "10-19", "20-29", "30-39", "40+"), each=20),
#'   age = c(sample(0:9, 20, replace=TRUE), sample(10:19, 20, replace=TRUE),
#'           sample(20:29, 20, replace=TRUE), sample(30:39, 20, replace=TRUE),
#'           sample(40:120, 20, replace=TRUE)),
#'   sex = c(rep("f", 40), rep("m", 40), rep("o", 20)),
#'   result_cat = dplyr::case_when(result_cat == "neg" ~ "negative",
#'                                 result_cat == "pos" ~ "positive",
#'                                 TRUE ~ result_cat)
#' )
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
#'   include_others = TRUE,
#'   rmd_safe = TRUE
#' )
#'
#' st_aggregate(validated_df)


st_aggregate <- function(
    data, subgroup = c("age_group", "sex", "age_group + sex"),
    borderline = c("negative", "positive", NA), add_ci = TRUE,
    round_digits = 4, test_combination = NULL
) {

  ## Extract attributes ------------------------------------------------------

  obj_id <- attr(data, "id")
  pathogen <- attr(data, "pathogen")
  adm0 <- attr(data, "adm0")


  ## Match arguments ---------------------------------------------------------

  subgroup <- match.arg(subgroup, several.ok = TRUE)
  borderline <- match.arg(borderline)


  ## Borderline & drop NA ----------------------------------------------------

  data <- dplyr::mutate(
    data,
    result_cat = dplyr::if_else(result_cat == "borderline", borderline,
                                result_cat),
    result_cat_pos = dplyr::case_when(result_cat == "positive" ~ 1,
                                      is.na(result_cat) ~ NA_integer_,
                                      TRUE ~ 0)
  )
  # TODO drop_na for `result` should be at separated and done at a later stage
  data <- tidyr::drop_na(data, dataset_id, id, result, result_cat)


  ## Summarize ---------------------------------------------------------------

  summarised_age_group <- data.frame()
  summarised_sex       <- data.frame()
  summarised_agesex    <- data.frame()

    ### Overall -----------------------------------------------------------------

    summarised_overall <- group_summarise(data, add_ci = add_ci,
                                          round_digits = round_digits)
    summarised_overall <- dplyr::mutate(summarised_overall,
                                        subgroup = "overall", strata = NA,
                                        .after=dataset_id)

    ### age_group ---------------------------------------------------------------

    if (("age_group" %in% names(data)) && ("age_group" %in% subgroup)) {
      summarised_age_group <- group_summarise(data, "age_group",
                                              add_ci = add_ci,
                                              round_digits = round_digits)
      summarised_age_group <- dplyr::mutate(summarised_age_group,
                                            subgroup = "age_group",
                                            strata=age_group, .after=dataset_id)
    }

    ### sex ---------------------------------------------------------------------

    if (("sex" %in% names(data)) && ("sex" %in% subgroup)) {
      summarised_sex <- group_summarise(data, "sex", add_ci = add_ci,
                                        round_digits=round_digits)
      summarised_sex <- dplyr::mutate(summarised_sex,
                                      subgroup = "sex", strata = sex,
                                      .after = dataset_id)
    }

    ### age_group + sex ---------------------------------------------------------

    if (all(c("age_group", "sex") %in% names(data)) &&
        all(c("age_group", "sex") %in% subgroup)) {
      summarised_agesex <- group_summarise(data, c("age_group", "sex"),
                                           add_ci = add_ci,
                                           round_digits = round_digits)
      summarised_agesex <- dplyr::mutate(summarised_agesex,
                                         subgroup = "age_group + sex",
                                         strata=paste(age_group, sex, sep=", "),
                                         .after = dataset_id)
    }


  ## Bind rows ---------------------------------------------------------------

  final <- dplyr::bind_rows(summarised_overall, summarised_age_group,
                            summarised_sex, summarised_agesex)


  ## Add attributes back -----------------------------------------------------

  attr(final, "id") <- obj_id
  attr(final, "pathogen") <- pathogen
  attr(final, "adm0") <- adm0

  return(final)
}



# Helper functions --------------------------------------------------------

#' Performs `dplyr::summarise()` for each specified group
#'
#' @param data A validated dataframe, output of `st_validate()`.
#' @param group Group for which `dplyr::summarize()` is perfomed. If left as
#'  NULL, it will calculate overall summarized values.
#' @param add_ci Boolean. Whether to add binomial proportion confidence interval.
#'  It is calculated using the Wilson score interval method through the
#'  `binom::binom.confint()` function.
#' @param round_digits Integer indicating the number of decimal places of the
#'  estimate. It is passed to the digits argument of `base::round()`.
#' @noRd
group_summarise <- function(data, group = NULL, add_ci, round_digits) {
  options(dplyr.summarise.inform = FALSE)
  data_grouped <- dplyr::group_by(data,
                                  dplyr::across(dplyr::any_of(c("dataset_id",
                                                                group))))
  data_summarised <- dplyr::summarise(
    data_grouped,
    pop_adj = FALSE,
    test_adj = FALSE,
    start_date = min(collection_start_date),
    end_date = max(collection_end_date),
    test_id_1 = unique(data_grouped$test_id)[1],
    test_id_2 = unique(data_grouped$test_id)[2],
    test_id_3 = unique(data_grouped$test_id)[3],
    test_combination = NA,
    numerator = sum(result_cat_pos, na.rm = TRUE),
    denominator = sum(!is.na(result_cat)),
    seroprev = round(numerator/denominator, round_digits),
    seroprev_95_ci_lower = dplyr::if_else(
      isTRUE(add_ci),
      round(
        binom::binom.confint(numerator, denominator, methods = "wilson")$lower,
        round_digits
      ),
      NA
    ),
    seroprev_95_ci_upper = dplyr::if_else(
      isTRUE(add_ci),
      round(
        binom::binom.confint(numerator, denominator, methods = "wilson")$upper,
        round_digits
      ),
      NA
    ),
    # seroprev_95_ci_lower = round(
    #   as.data.frame(Hmisc::binconf(numerator, denominator))$Lower,
    #   round_digits
    # ),
    # seroprev_95_ci_upper = round(
    #   as.data.frame(Hmisc::binconf(numerator, denominator))$Upper,
    #   round_digits
    # ),
    # seroprev_95_ci_lower = NA,
    # seroprev_95_ci_upper = NA,
    ab_denominator = sum(!is.na(result)),
    ab_titer_min = min(result),
    ab_titer_max = max(result),
    ab_titer_mean = mean(result),
    ab_titer_sd = stats::sd(result)
  )

  # Add columns that are not among grouping variables with "All" as only value
  subgroups_all <- c(sex = "All", age_group = "All")
  data_summarised <- tibble::add_column(
    data_summarised,
    !!!subgroups_all[setdiff(names(subgroups_all), names(data_summarised))]
  )

  data_summarised <- dplyr::bind_cols(data_summarised,
                                      summarize_age_min_max(data_grouped),
                                      summarize_adm1_adm2(data_grouped))
  dplyr::select(
    data_summarised,
    dataset_id, age_group, age_min, age_max, sex, pop_adj, test_adj, adm1,
    adm2, start_date, end_date, test_id_1, test_id_2, test_id_3,
    test_combination, numerator, denominator, seroprev, seroprev_95_ci_lower,
    seroprev_95_ci_upper, ab_denominator, ab_titer_min, ab_titer_max,
    ab_titer_mean, ab_titer_sd
  )
}



#' Summarize age min and max
#'
#' @param data_grouped A grouped dataframe.
#' @noRd
summarize_age_min_max <- function(data_grouped) {
  if ("age" %in% names(data_grouped)) {
    data_summarised_age <- dplyr::summarise(data_grouped,
                                            age_min = min(age),
                                            age_max = max(age))
  } else if ("age_group" %in% names(data_grouped)) {
    data_grouped <- dplyr::mutate(
      data_grouped,
      lower_age=as.numeric(stringr::str_extract(age_group,"^.*(?=((\\-|\\+)))")),
      upper_age = as.numeric(stringr::str_extract(age_group, "(?<=\\-).*$"))
    )
    data_summarised_age <- dplyr::summarise(data_grouped,
                                            age_min = min(lower_age),
                                            age_max = max(upper_age))
  } else {
    data_summarised_age <- dplyr::summarise(data_grouped, age_min = NA, age_max = NA)
  }
  data_summarised_age <- dplyr::ungroup(data_summarised_age)
  dplyr::select(data_summarised_age, age_min, age_max)
}



#' Summarize adm1 and adm2
#'
#' @param data_grouped A grouped dataframe.
#' @noRd
summarize_adm1_adm2 <- function(data_grouped) {
  if ("adm1" %in% names(data_grouped)) {
    data_summarised_adm1 <- dplyr::summarise(
      data_grouped, adm1 = paste(unique(data_grouped$adm1), collapse = ", ")
    )
  } else {data_summarised_adm1 <- dplyr::summarise(data_grouped, adm1 = NA_character_)}
  if ("adm2" %in% names(data_grouped)) {
    data_summarised_adm2 <- dplyr::summarise(
      data_grouped, adm2 = paste(unique(data_grouped$adm2), collapse = ", ")
    )
  } else {data_summarised_adm2 <- dplyr::summarise(data_grouped, adm2 = NA_character_)}

  data_summarised_adm1 <- dplyr::ungroup(data_summarised_adm1)
  data_summarised_adm2 <- dplyr::ungroup(data_summarised_adm2)
  data_summarised_adm1_adm2 <- dplyr::bind_cols(
    dplyr::select(data_summarised_adm1, adm1),
    dplyr::select(data_summarised_adm2, adm2)
  )
  data_summarised_adm1_adm2
}
