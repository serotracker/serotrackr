
# Prerequisites -----------------------------------------------------------

library(dplyr)
library(stringr)
library(devtools)
load_all()
# TODO add warning for dropping NAs
# TODO add CIs, both binomial and bootstrapping.
# TODO add wrapper function for regions
# TODO test the package against other datasets

# TO DO -------------------------------------------------------------------

# TODO Add github logo and url to pkgdown
# TODO On `Reference` page of pkgdown website, put objects in meaningful groups
# TODO Remove `clean()`
# TODO Add R CMD Check and other banners to README
# TODO Add experimental, ... tags to pkgdown website
# TODO Add source link to each vignette
# TODO Explore creating a specific class for the output
# TODO Consider using readr::parse_number() for `age` and `result` values
# TODO Add another function to evaluate assays, including their test_cutoff

# FIXME Should ab_titer_unit remain an optional field?


# Prep data ---------------------------------------------------------------

new_raw_data <- sample_raw_data %>%
  dplyr::mutate(
    age_group = rep(c("0-9", "10-19", "20-29", "30-39", "40+"), each=20),
    age = c(sample(0:9, 20, replace=TRUE), sample(10:19, 20, replace=TRUE),
            sample(20:29, 20, replace=TRUE), sample(30:39, 20, replace=TRUE),
            sample(40:120, 20, replace=TRUE)),
    sex = sample(c(rep("f", 40), rep("m", 40), rep("o", 20))),
    result_cat = dplyr::case_when(result_cat == "neg" ~ "negative",
                                  result_cat == "pos" ~ "positive",
                                  TRUE ~ result_cat)
  )


validated_df <- st_validate(
  new_raw_data,
  dataset_id = dataset_id,
  id = id,
  age_group = age_group,
  age = age,
  sex = sex,
  adm0 = regions$adm0$Canada,
  adm1 = regions$adm1$Canada$Alberta,
  adm2 = regions$adm2$Canada$Alberta$Calgary,
  collection_start_date = "2023-01-01",
  collection_end_date = "2023-02-01",
  test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
  result = result,
  result_cat = result_cat,
  include_others = TRUE
)

estimates <- st_aggregate(validated_df, add_ci = TRUE)
# st_save(validated_df, estimates, path = "test.xlsx")



# 95% CI - bootstrap ------------------------------------------------------

dat_ci <- dat %>%
  tidyr::drop_na(result_cat) %>%
  mutate(result_cat_pos = case_when(result_cat == "positive" ~ 1,
                                    is.na(result_cat) ~ NA_integer_,
                                    TRUE ~ 0))

prevalence_fun <- function(data, indices, infected_dummy) {
  # select resampled data
  d <- data[indices, ]
  # calculate prevalence as proportion of infected cases
  prev <- mean(d[[{{infected_dummy}}]])
  return(prev)
}
boot_ci <- function(data, statistic, ...,
                    type=c("boot_norm", "boot_basic", "boot_perc", "boot_bca")) {
  type <- match.arg(type)
  if (type == "boot_norm") {
    type_short <- "norm"; type_long <- "normal"; indx_low <- 2; indx_up <- 3
  } else if (type == "boot_basic") {
    type_short <- "basic"; type_long <- "basic"; indx_low <- 4; indx_up <- 5
  } else if (type == "boot_perc") {
    type_short <- "perc"; type_long <- "percent"; indx_low <- 4; indx_up <- 5
  } else if (type == "boot_bca") {
    type_short <- "bca"; type_long <- "bca"; indx_low <- 4; indx_up <- 5
  }
  boot_result <- boot::boot(data = data, statistic = statistic, R = 1000,
                            # strata = strata,
                            ...)
  ci <- boot::boot.ci(boot_result, type = type_short)
  lower <- ci[[type_long]][indx_low]
  upper <- ci[[type_long]][indx_up]
  list("lower" = lower, "upper" = upper)
}

b <- boot_ci(data=dat_ci, statistic=prevalence_fun, type = "boot_bca",
             infected_dummy = "result_cat_pos")
b



# st_locate() -------------------------------------------------------------

st_locate(sample_raw_data, adm0 = regions$adm0$Canada,
          adm1 = adm1, #adm2 = adm2,
          "bc" = regions$adm1$Canada$`British Columbia`,
          "Chernihiv Oblast" = regions$adm1$Canada$`British Columbia`,
          "Tarxien" = regions$adm1$Canada$`British Columbia`,
          "Chari-Baguirmi" = regions$adm1$Canada$`British Columbia`,
          "Blida" = regions$adm1$Canada$`British Columbia`
          )

st_locate(sample_raw_data, adm0 = regions$adm0$Canada,
          adm1 = regions$adm1$Canada$Alberta, adm2 = adm2,
          # "Isles of Scilly" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Malheur" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Go Cong Tay" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Reduit Park" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Rondón" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Um Bada" = regions$adm2$Canada$Manitoba$Winnipeg,
          # "Kasan" = regions$adm2$Canada$Manitoba$Winnipeg,
          into = c("adm1_new", "adm2_new"),
          n_unmatched_printed = 3.00001)


st_locate(sample_raw_data, adm0 = regions$adm0$Canada,
          adm1 = regions$adm1$Canada$Alberta,
          adm2 = regions$adm2$Canada$Alberta$Calgary,
          "ab" = regions$adm1$Canada$Yukon)

st_locate(sample_raw_data,
          adm0 = regions$adm0$Canada,
          adm1 = regions$adm1$Canada$Alberta,
          adm2 = regions$adm2$Canada$Alberta$Calgary)


zzz <- st_locate(sample_raw_data, adm0 = regions$adm0$Canada,
                 adm1 = adm1, adm2 = adm2)
mydata <- tibble(
  adm1 = c("alberta", "california", "bc"),
  adm2 = c(NA, NA, "Vancouver"),
  adm1_code = NA_character_, adm2_code = NA_character_
)


country <- "Canada"
lhs <- c("alberta", "Vancouver")
rhs <- c(regions$adm1$Canada$Alberta,
         regions$adm2$Canada$`British Columbia`$`Vancouver Island and Coast / *`)

evaluate_lhs_rhs(data = mydata, lhs = lhs, rhs = rhs, adm0_code = regions$adm0$Canada,
                 adm1 = "state", adm2 = "district")

st_locate(mydata, adm0 = regions$adm0$Canada, adm1 = adm1, adm2 = adm2,
          "california" = regions$adm1$Canada$Alberta,
          "bc" = regions$adm1$Canada$`British Columbia`,
          "Vancouver" = regions$adm2$Canada$`British Columbia`$Nechako,
          into = c("adm1_code", "adm2_code"))

