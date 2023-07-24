
validated_df <- st_validate(
  sample_raw_data,
  dataset_id = dataset_id,
  id = id,
  age_group = "12-17",
  sex = "m",
  adm0 = regions$adm0$Canada,
  adm1 = regions$adm1$Canada$Alberta,
  adm2 = regions$adm2$Canada$Alberta$Calgary,
  collection_start_date = "2023-01-01",
  collection_end_date = "2023-02-01",
  test_id = assays$`SARS-CoV-2`$`AAZ LMB - IgG, IgM - COVID-PRESTO®`,
  result = result,
  result_cat = "negative",
  include_others = TRUE
)

estimates <- st_aggregate(validated_df)

test_that("file is written to tempfile()", {
  st_save(validated_df, estimates, path = tempfile())
  file <-  tempfile()
  expect_false(file.exists(file))
  on.exit(unlink(file))
})
