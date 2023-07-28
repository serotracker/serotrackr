
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



# 95% CI ------------------------------------------------------------------

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



# Binomial 95% CI ---------------------------------------------------------

validated_df <- validated_df %>%
  tidyr::drop_na(result) %>%
  dplyr::mutate(
    lower = as.data.frame(Hmisc::binconf(4, 22))$Lower,
    upper = as.data.frame(Hmisc::binconf(4, 22))$Upper
  )

a <- Hmisc::binconf(22, 45) %>% as.data.frame()

as.data.frame(Hmisc::binconf(22, 45))$Lower
as.data.frame(Hmisc::binconf(22, 45))$Upper

num <- c(22, 33, 44)
denom <- c(45, 55, 65)
binom.test(num, denom)
Hmisc::binconf(num, denom)
binom::binom.confint(num, denom, methods = "wilson")$lower
binom::binom.confint(num, denom, methods = "wilson")$upper


p <- num/denom
p + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/denom)*p*(1-p))


b <- binom.test(22, 45)
attributes(b)
b$conf.int[1]
b$conf.int[2]

22/45
