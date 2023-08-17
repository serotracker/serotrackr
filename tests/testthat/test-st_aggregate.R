
mydata <- dplyr::mutate(
  sample_raw_data,
  age = ifelse(age %in% c(-999, 999), NA, age)
)

validated_df <- st_validate(
  mydata,
  dataset_id = dataset_id,
  id = id,
  age_group = age_group,
  age = age,
  sex = sex,
  adm0 = regions$adm0$Canada,
  adm1 = regions$adm1$Canada$Alberta,
  adm2 = regions$adm2$Canada$Alberta$Calgary,
  collection_start_date = "2020-Mar-01",
  collection_end_date = "15/8/2023",
  test_id = assays$`SARS-CoV-2`$`ID.Vet - IgG - ID Screen`,
  result = result,
  result_cat = result_cat,
  include_others = TRUE,
  rmd_safe = TRUE
)

estimates <- st_aggregate(validated_df)

test_that("there are 27 columns", {
  expect_equal(length(names(estimates)), 27)
})
