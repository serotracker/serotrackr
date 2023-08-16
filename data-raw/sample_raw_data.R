## code to prepare `sample_raw_data` dataset goes here

# Will update this sample data over time to showcase different cleaning and
# validation considerations. This sample raw data is included in the package
# for users.


# Prerequisites -----------------------------------------------------------

devtools::load_all()
# Note: dataset from `Less random` section is exported as a package object.


# More random -------------------------------------------------------------

## Generate random age groups ----------------------------------------------

generate_age <- function(n, age_grouping = 10) {
  lower <- sample(seq(0, 120, by = age_grouping), n, replace = TRUE)
  dash <- paste0(
    lower, "-", lower + sample(c(-age_grouping, age_grouping), n,
                               prob = c(0.3, 0.7), replace = TRUE)
  )
  plus <- paste0(sample(seq(60, 120, by=age_grouping), n, replace = TRUE), "+")
  random_age_group <- sample(
    c(dash, plus), n, replace = TRUE,
    prob = c(rep(0.8/n, times = n), rep(0.2/n, times = n))
  )
  extracted_lower <- stringr::str_extract(random_age_group,
                                          "^.*(?=((\\-|\\+)))") |>
    as.numeric()
  random_age <- round(
    rnorm(n, mean = extracted_lower+age_grouping/2, sd = age_grouping/3)
  )
  list(random_age_group = random_age_group, random_age = random_age)
}


## Generate random dates ---------------------------------------------------

generate_date <- function(n, year_start = 1990, year_end = 2030,
                          month_max = 14, day_max = 34,
                          separators = c("-", "/", " ", "|", ".")) {
  stopifnot(!is.null(n))
  year_range <- year_start:year_end
  month_abbr <- levels(lubridate::month(Sys.Date(), label = TRUE))
  month_full <- levels(lubridate::month(Sys.Date(), label = TRUE, abbr = FALSE))
  month_max <- month_max
  day_max <- day_max
  lhs <- sample(c(1:day_max, year_range), n, replace = TRUE)
  separators <- separators
  paste0(
    lhs,
    sample(separators, n, replace = TRUE),
    sample(c(1:month_max, month_abbr, month_full), n, replace = TRUE),
    sample(separators, n, replace = TRUE),
    ifelse(lhs > day_max,
           sample(1:day_max, n, replace = TRUE),
           sample(year_range, n, replace = TRUE))
  )
}


## Generate sample raw data ------------------------------------------------

makedf <- function(n, adm0 = "Canada", adm1 = "Alberta",
                   pathogen = "SARS-CoV-2", age_grouping = 10,
                   year_start = 1990, year_end = 2030, month_max = 14,
                   day_max = 34, separators = c("-", "/", " ", "|", ".")) {
  stopifnot(n >= 3)
  stopifnot(length(adm0) == 1)
  stopifnot(length(adm1) == 1)
  stopifnot(length(pathogen) == 1)

  ## Tweak this section for specific tests -----------------------------------

  head_dataset_id <- rep(1, times = 3)
  head_id <- c(1, 2, 3)
  head_age_group <- c("12-120", "12-8.5", NA)
  head_age <- c(6, NA, 23)
  head_sex <- c("m", "m", "o")
  # adm0: only one value is acceptable
  head_adm1 <- c("Alberta", "bc", "Québec")
  head_adm2 <- rep(regions$adm2$Canada$Alberta$Calgary, times = 3)
  head_start_date <- c("2009/03/11", NA, "2008-03-11")
  head_end_date <- c("2019-12-01", "2019-04-01", "2009-04-01")
  head_test_id <- assays$`SARS-CoV-2`$`AESKU - IgG - SARS-CoV-2 NP IgG`
  head_result <- sample(0.01:199.99, size = 3)
  head_result_cat <- c("negative", "positive", NA)

  ## Random generation -------------------------------------------------------

  if (n > 3) {
    random_dataset_id <- sample(1:2, n-length(head_dataset_id), replace = TRUE)
    random_id <- sample(1:n, n-length(head_id), replace = TRUE)
    generated_age <- generate_age(n = n-length(head_age_group), age_grouping)
    random_age_group <- generated_age$random_age_group
    random_age <- generated_age$random_age
    # random_age_group <- generate_age_group(n = n-length(head_age_group))
    # random_age <- sample(0:130, n-length(head_age), replace = TRUE)
    random_sex <- sample(c("f","m","o","female","male","other","queer","LGBTQ"),
                         n-length(head_age), replace = TRUE)
    # adm0: only one value is acceptable
    random_adm1 <- sample(
      c(adm1, unique(regions_df$NAME_1)),
      prob = c(0.95, rep(0.05/length(unique(regions_df$NAME_1)),
                         times = length(unique(regions_df$NAME_1)))),
      n-length(head_adm1), replace = TRUE
    )
    random_adm2 <- sample(
      c(regions_df$NAME_2[which(regions_df$NAME_1==adm1)],
        unique(regions_df$NAME_2)),
      prob = c(
        rep(0.95/length(regions_df$NAME_2[which(regions_df$NAME_1==adm1)]),
            times=length(regions_df$NAME_2[which(regions_df$NAME_1==adm1)])),
        rep(0.05/length(unique(regions_df$NAME_2)),
            times = length(unique(regions_df$NAME_2)))
      ),
      n-length(head_adm2), replace = TRUE
    )
    random_start_date <- generate_date(n = n-length(head_start_date),
                                       year_start, year_end, month_max,
                                       day_max, separators)
    random_end_date <- generate_date(n = n-length(head_end_date),
                                     year_start, year_end, month_max,
                                     day_max, separators)
    random_test_id <- sample(
      sample(
        unique(assays_df[which(assays_df$pathogen==pathogen),][["test_id"]]),
        2, replace = TRUE
      ),
      n-length(head_test_id), replace = TRUE
    )
    random_result <- sample(seq(0, 400, by = 0.01), n-length(head_result),
                            replace = TRUE)
    random_result_cat <- sample(c("negative","borderline","positive","neg","pos"),
                                n-length(head_result_cat), replace = TRUE)
  } else {
    empty_list <- c("random_dataset_id", "random_id", "random_age_group",
                    "random_age", "random_sex", "random_adm1", "random_adm2",
                    "random_start_date", "random_end_date", "random_test_id",
                    "random_result", "random_result_cat")
    list2env(sapply(empty_list, function(x) {x <- character()}),
             envir = environment())
  }

  ## Generate dataset --------------------------------------------------------

  dplyr::tibble(
    dataset_id = c(head_dataset_id, random_dataset_id),
    id = c(head_id, random_id),
    age_group = c(head_age_group, random_age_group),
    age = c(head_age, random_age),
    sex = c(head_sex, random_sex),
    adm0 = adm0,
    adm1 = c(head_adm1, random_adm1),
    adm2 = c(head_adm2, random_adm2),
    collection_start_date = c(head_start_date, random_start_date),
    collection_end_date = c(head_end_date, random_end_date),
    test_id = c(head_test_id, random_test_id),
    result = c(head_result, random_result),
    result_cat = c(head_result_cat, random_result_cat),
    # Other columns:
    another_col = "Another column"
  )
}


## Run random data ---------------------------------------------------------

set.seed(98765)
sample_raw_data <- makedf(n = 100)



# Less random -------------------------------------------------------------

# This section makes a less random sample data as an alternative to the one
# created in the `More random` section. This version is preferred as it makes
# package examples simpler.

makedf_simple <- function(n) {
  set.seed(98765)

  # age_group & age
  age_group <- sample(c("0-17", "18-64", "65+"), n, replace = TRUE)
  lower_age <- as.numeric(stringr::str_extract(age_group, "^.*(?=((\\-|\\+)))"))
  age <- numeric(0)
  for (i in 1:length(lower_age)) {
    age[i] <- dplyr::case_when(
      lower_age[i] == 0  ~ lower_age[i]+sample(0:17, 1),
      lower_age[i] == 18  ~ lower_age[i]+sample(0:46, 1),
      lower_age[i] == 65 ~ sample(c(lower_age[i]+sample(0:35, 1), 999), 1,
                                  prob = c(0.95, 0.05))
    )
  }
  age_group[4] <- NA_character_
  age[4] <- -999

  # state & city
  state <- sample(c("Alberta", "ontario"), n, replace = TRUE)
  city <- character(0)
  for (i in 1:length(state)) {
    city[i] <- dplyr::case_when(
      state[i] == "Alberta" ~ sample(c("Calgary", "Calagry", "Edmonton"), 1),
      state[i] == "ontario" ~ sample(c("Toronoto", "toronto", "London"), 1)
    )
  }

  # result
  result <- sample(seq(0, 400, by = 0.01), n, replace = TRUE)

  dplyr::tibble(
    dataset_id = rep(1:2, each = n/2),
    id = 1:n,
    age_group = age_group,
    age = age,
    sex = sample(c("f", "m"), n, replace = TRUE),
    country = "Canada",
    state = state,
    city = city,
    start_date = rep(c("2023/Jan/01", "01-03-2020"), each = n/2),
    end_date = rep(c("15-8-2023", "2021-dec-31"), each = n/2),
    test_id = rep(c(assays$`SARS-CoV-2`$`ID.Vet - IgG - ID Screen`,
                    assays$`SARS-CoV-2`$`AESKU - IgG - SARS-CoV-2 NP IgG`),
                  each = n/2),
    result = result,
    result_cat = ifelse(result > 50,
                        "positive",
                        ifelse(result < 30, "negative", "borderline"))
  )
}

sample_raw_data <- makedf_simple(n = 100)



# Use data ----------------------------------------------------------------

usethis::use_data(sample_raw_data, overwrite = TRUE)
