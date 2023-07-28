
# Install & attach --------------------------------------------------------

# devtools::install_github("serotracker/serotrackr")
# install.packages("serotrackr")

# library(serotrackr)
library(dplyr)

head(sample_raw_data)

# Main function:
# st_validate()
# st_aggregate()
# st_save()

# 1. Validate data --------------------------------------------------------

validated_df <- st_validate(
  sample_raw_data,
  dataset_id = dataset_id,
  id = id,
  age_group = age_group,
  age = age,
  sex = sex,
  adm0 = regions$adm0$Canada,
  adm1 = adm1,
  adm2 = regions$adm2$Canada$Alberta$Calgary,
  collection_start_date = "2023-01-01",
  collection_end_date = "2023-02-01",
  test_id = assays$`SARS-CoV-2`$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
  result = result,
  result_cat = result_cat,
  include_others = TRUE
)

## 1.1. Address issues -----------------------------------------------------

set.seed(4567)
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


## 1.2. Re-run validation --------------------------------------------------

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

attributes(validated_df)
head(validated_df)


# 2. Generate estimates ---------------------------------------------------

estimates <- st_aggregate(validated_df)
head(estimates)


# 3. Save -----------------------------------------------------------------

st_save(validated_df, estimates, path = "submission.xlsx")
