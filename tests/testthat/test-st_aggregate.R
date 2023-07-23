
new_raw_data <- sample_raw_data %>%
  dplyr::mutate(
    age_group = rep(c("0-9", "10-19", "20-29", "30-39", "40+"), each=20),
    age = c(sample(0:9, 20, replace=TRUE), sample(10:19, 20, replace=TRUE),
            sample(20:29, 20, replace=TRUE), sample(30:39, 20, replace=TRUE),
            sample(40:120, 20, replace=TRUE)),
    sex = c(rep("f", 40), rep("m", 40), rep("o", 20)),
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

estimates <- st_aggregate(validated_df)

test_that("there are 27 columns", {
  expect_equal(length(names(estimates)), 27)
})