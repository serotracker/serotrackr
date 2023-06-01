
df_orig <- data.frame(dataset_id = 1,
                      id = 1:3,
                      age = c("0 - 10", "20-30", "30,40"),
                      sex = c("f", "Male", "binary"),
                      collection_start_date = "2020-03-01",
                      collection_end_date = "2020-04-01",
                      result = c("'2.4'", "3.5", "9"),
                      result_cat = rep("negative", times = 3))

df_out <- map_cols(
  df_orig,
  dataset_id = dataset_id,
  id = id,
  age_group = age,
  sex = sex,
  country = "Canada",
  collection_start_date = collection_start_date,
  collection_end_date = collection_end_date,
  test_id = assays$`SARS-CoV-2`$`EUROIMMUN - IgG - Anti-SARS-CoV-2 ELISA IgG`,
  result = result,
  result_cat = result_cat,
  include_others = FALSE
) %>% clean()

test_that("file is written to tempfile()", {
  save_xlsx(df_out, path = tempfile())
  file <-  tempfile()
  expect_false(file.exists(file))
  on.exit(unlink(file))
})
