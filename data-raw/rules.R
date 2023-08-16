## code to prepare (validation) `rules` goes here. It is an internal object.

# Approximate runtime of these rules:
# Dataset with 1,000 rows:     00.13 sec
# Dataset with 10,000 rows:    00.24 sec
# Dataset with 100,000 rows:   01.60 sec
# Dataset with 1,000,000 rows: 14.40 sec

# TODO check if adm1 is a region within adm0. Same for adm2.
# TODO For each test_id, check that min result of 'positive' result_cats is
# larger than the max result of 'negative' (or 'borderline') result_cats.
# TODO Check all assays are for one pathogen only
# TODO adm0 columns must only contain one country code
# TODO Add validation rules for date-type dates. It's just char-type dates rn.


# Validation rules --------------------------------------------------------

rules <- validate::validator(

  ## dataset_id --------------------------------------------------------------
  # dataset_id_isNum = is.numeric(dataset_id),

  ## id ----------------------------------------------------------------------
  id_isUnique = validate::is_unique(
    id, collection_start_date, collection_end_date, test_id, result
  ) %in% TRUE,

  ## age_group ---------------------------------------------------------------
  age_group_dashPlus = if(!is.na(age_group)) grepl(
    paste0("^[0-9]\\d*(\\.\\d+)?-[0-9]\\d*(\\.\\d+)?$", # 18-64 or 0.5-10.5
           "|", "^[0-9]\\d*(\\.\\d+)?\\+$"), # 65+ or 65.5+
    age_group
  ) else NA,
  # `lower_age` catches number before both dash and plus signs:
  lower_age := stringr::str_extract(age_group, "^.*(?=((\\-|\\+)))") %>%
    as.numeric(),
  upper_age := stringr::str_extract(age_group, "(?<=\\-).*$") %>% as.numeric(),
  # Rules below are specific to dash structure like 18-64 or 0.5-10.5:
  age_group_upLTE120 = if(
    grepl("^[0-9]\\d*(\\.\\d+)?-[0-9]\\d*(\\.\\d+)?$", age_group) == TRUE
  ) upper_age <= 120 else NA,
  age_group_lowLTEup = if(
    grepl("^[0-9]\\d*(\\.\\d+)?-[0-9]\\d*(\\.\\d+)?$", age_group) == TRUE
  ) lower_age <= upper_age else NA,
  # Rule below is specific to plus structure like 65+ or 65.5+:
  age_group_plusInRange = if(grepl("^[0-9]\\d*(\\.\\d+)?\\+$", age_group)==TRUE)
    validate::in_range(lower_age, 0, 120, strict = TRUE) == TRUE else NA,

  ## age ---------------------------------------------------------------------
  # age_isNum = is.numeric(age),
  age_notInf = !is.infinite(age),
  age_GTE0 = if(is.numeric(age) & !is.infinite(age)) age >= 0 else NA,
  age_LTE120 = if(is.numeric(age) & !is.infinite(age)) age <= 120 else NA,
  age_GTElowerAge = if(is.numeric(age) & !is.infinite(age)) age>=lower_age else NA,
  age_LTEupperAge = if(is.numeric(age) & !is.infinite(age)) age<=upper_age else NA,

  ## sex ---------------------------------------------------------------------
  sex_presetVal = if(!is.na(sex))
    grepl("^f$|^female$|^m$|^male$|^o$|^other$", sex,
          ignore.case = TRUE) else NA,

  ## adm0 --------------------------------------------------------------------
  # adm0_singleVal = dplyr::if_else(!any(is.na(adm0)) & length(unique(adm0)) == 1,
  #                                 TRUE, FALSE) == TRUE,
  adm0_presetVal = adm0 %in%
    dplyr::pull(dplyr::filter(regions_df, shapeType=="ADM0"), shapeID_v5),

  ## adm1 --------------------------------------------------------------------
  adm1_presetVal = adm1 %in%
    dplyr::pull(dplyr::filter(regions_df, shapeType=="ADM1"), shapeID_v5),

  ## adm2 --------------------------------------------------------------------
  adm2_presetVal = adm2 %in%
    dplyr::pull(dplyr::filter(regions_df, shapeType=="ADM2"), shapeID_v5),

  ## collection_start_date ---------------------------------------------------
  start_date_isValidFrmt = dplyr::if_else(
    !is.na(collection_start_date),
    dplyr::if_else(is.na(lubridate::parse_date_time2(collection_start_date,
                                                     c("dmY","Ymd"))),
                   FALSE, TRUE),
    NA
  ) == TRUE,

  start_date_2000today =
    lubridate::parse_date_time2(collection_start_date, c("dmY", "Ymd")) >=
    as.Date("2000-01-01") &
    lubridate::parse_date_time2(collection_start_date, c("dmY", "Ymd")) <=
    Sys.Date(),

  ## collection_end_date -----------------------------------------------------
  end_date_isValidFrmt = dplyr::if_else(
    !is.na(collection_end_date),
    dplyr::if_else(is.na(lubridate::parse_date_time2(collection_end_date,
                                                     c("dmY","Ymd"))),
                   FALSE, TRUE),
    NA
  ) == TRUE,

  end_date_2000today =
    lubridate::parse_date_time2(collection_end_date, c("dmY", "Ymd")) >=
    as.Date("2000-01-01") &
    lubridate::parse_date_time2(collection_end_date, c("dmY", "Ymd")) <=
    Sys.Date(),

  end_date_GTEstart = dplyr::if_else(
    (
      (lubridate::parse_date_time2(collection_start_date, c("dmY", "Ymd")) >=
         as.Date("2000-01-01") &
         lubridate::parse_date_time2(collection_start_date, c("dmY", "Ymd")) <=
         Sys.Date()) == TRUE &
        (lubridate::parse_date_time2(collection_end_date, c("dmY", "Ymd")) >=
           as.Date("2000-01-01") &
           lubridate::parse_date_time2(collection_end_date, c("dmY", "Ymd")) <=
           Sys.Date()) == TRUE
    ),
    (
      lubridate::parse_date_time2(collection_end_date, c("dmY", "Ymd")) >=
        lubridate::parse_date_time2(collection_start_date, c("dmY", "Ymd"))
    ),
    NA
  ) == TRUE,

  ## test_id -----------------------------------------------------------------
  test_id_presetVal = test_id %in% dplyr::pull(assays_df, test_id),

  ## result ------------------------------------------------------------------
  # result_isNum = is.numeric(result),
  result_GTE0 = if(is.numeric(result)) result >= 0 else NA,

  ## result_cat --------------------------------------------------------------
  result_cat_presetVal = if(!is.na(result_cat))
    grepl("^positive$|^borderline$|^negative$", result_cat,
          ignore.case = TRUE) else NA

) # validator() closes


names(rules["V03"]) <- "lower_age"
names(rules["V04"]) <- "upper_age"


# Error messages ----------------------------------------------------------

# Using the `description` metadata of the "validator" object to store the body
# of error messages that, if invoked, the st_validate() will show. The header
# (argument name) of these error messages will be entered separately in the
# my_progress().
validate::description(rules) <- c(
  # dataset_id_isNum
  # "must be a numeric column, not ",
  # id_isUnique
  paste("is not unique. Some records have identical values for all these",
        "arguments: {.emph id}, {.emph collection_start_date},",
        "{.emph collection_end_date}, {.emph test_id}, and {.emph result}."),
  # age_group_dashPlus
  paste("must have one of these two structures: `{.emph number-number}` or",
        "`{.emph number+}`; e.g `18-64` or `65+`."),
  # lower_age
  NA,
  # upper_age
  NA,
  # age_group_upLTE120
  paste("upper bound can't be larger than 120 years."),
  # age_group_lowLTEup
  paste("lower bound can't be larger than upper bound."),
  # age_group_plusInRange
  paste("must be between 0 and 120 not inclusive, when it has a `{.emph",
        "number+}` structure, like 65+."),
  # age_isNum
  # NA,
  # age_notInf
  "can't be infinite.",
  # age_GTE0
  "can't be negative.",
  # age_LTE120
  "can't be larger than 120 years.",
  # age_GTElowerAge
  paste("can't be less than the lower bound of its corresponding `age_group`",
        "value."),
  # age_LTEupperAge
  paste("can't be greater than the upper bound of its corresponding `age_group`",
        "value."),
  # sex_presetVal
  paste("must only be one of these values, ignoring case: `{.emph f}`,",
        "`{.emph m}`, `{.emph o}`, `{.emph female}`, `{.emph male}`, or",
        "`{.emph other}`."),
  # adm0_singleVal
  # "can't be more than one value.",
  # adm0_presetVal
  paste("code was not found. Use `{.pkg serotrackr}::regions$adm0${.emph",
        "YourCountry}`."),
  # adm1_presetVal
  paste("codes were not found. Use `st_locate()` or `{.pkg",
        "serotrackr}::regions$adm1${.emph YourCountry}${.emph YourState}`."),
  # adm2_presetVal
  paste("codes were not found. Use `st_locate()` or",
        "`{.pkg serotrackr}::regions$adm2${.emph YourCountry}${.emph",
        "YourState}${.emph YourDistrict}`."),
  # start_date_isValidFrmt
  paste("must be `{.emph yyyy-mm-dd}` or `{.emph dd-mm-yyyy}`. Instead of dash,",
        "any arbitrary non-digit separator or blank space are also acceptable.",
        "{.emph mm} can also be abbreviated or full months names."),
  # start_date_2000today
  paste("can't be before `2000-01-01` or in the future."),
  # end_date_isValidFrmt
  paste("must be `{.emph yyyy-mm-dd}` or `{.emph dd-mm-yyyy}`. Instead of dash,",
        "any arbitrary non-digit separator or blank space are also acceptable.",
        "{.emph mm} can also be abbreviated or full months names."),
  # end_date_2000today
  paste("can't be before `2000-01-01` or in the future."),
  # end_date_GTEstart
  "can't be before the start date.",
  # test_id_presetVal
  paste("was not found. Use `{.pkg serotrackr}::assays${.emph",
        "YourPathogen}${.emph YourTestID}`."),
  # result_isNum
  # "must be a numeric column, not ",
  # result_GTE0
  "can't be negative.",
  # result_cat_presetVal
  paste("must only be one of these values, ignoring case: `{.emph positive}`,",
        "`{.emph borderline}`, or `{.emph negative}`.")

)


usethis::use_data(rules, internal = TRUE, overwrite = TRUE)



# Runtime -----------------------------------------------------------------

# library(dplyr)
# library(validate)
# library(lubridate)
# library(devtools)
# load_all()
#
# makedf <- function(t) {
#   tibble(
#     dataset_id = 1,
#     id = rep(c(1, 1, 2), times = t),
#     age_group = rep(c("12-120", "12-8.5", NA), times = t),
#     age = rep(c(NA, NA, NA), times = t),
#     sex = rep(c("m", "m", "o"), times = t),
#     adm0 = regions$adm0$Canada,
#     adm1 = NA,
#     adm2 = regions$adm2$Canada$Alberta$Calgary,
#     state = rep(c("Alberta", "bc", "Québec"), times = t),
#     city = rep(c("calgary", "Metro vancouver", "Montréal"), times = t),
#     collection_start_date = rep(c("2009/03/11", NA, "2008-03-11"), times = t),
#     collection_end_date=rep(c("2019-14-01", "2019-04-01", "2007-04-01"),times=t),
#     test_id = assays$`SARS-CoV-2`$`AESKU - IgG - SARS-CoV-2 NP IgG`,
#     result = "9",
#     result_cat = rep(c("negative", "positive", NA), times = t)
#   )
# }
#
# testdf <- makedf(t = 1)
#
# start.time <- Sys.time()
# summary(confront(testdf, rules, raise="all"))[1:7]
# end.time <- Sys.time()
# end.time - start.time
#
# # Dataset with 1,000 rows:     00.13 sec
# # Dataset with 10,000 rows:    00.24 sec
# # Dataset with 100,000 rows:   01.60 sec
# # Dataset with 1,000,000 rows: 14.40 sec
#
#
# # validate::confront(testdf, rules, raise="all") %>% summary() %>%
# #   select(-expression)
# # cf <- confront(testdf, rules)
# # values(cf)[[2]] %>% as.data.frame() %>% select(adm1_presetVal) %>% is.na()
# # values(cf, simplify = FALSE)$adm0
#
# # confront(testdf, rules[grep("age_group|V09|V10",names(rules))]) %>%
# #   summary() %>% select(-expression)
#
# # names(rules)
# # label(rules)
# # description(rules)
# # meta(rules)
# # length(rules)
# # variables(rules)
# # View(validate::as.data.frame(rules))

