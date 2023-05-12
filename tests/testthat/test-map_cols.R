
df_orig <- data.frame(
  dataset_id = 1,
  id = 1:3,
  age = c("0 - 10", "20-30", "30,40"),
  sex = c("f", "Male", "binary"),
  collection_start_date = "2020-03-01",
  collection_end_date = "2020-04-01",
  result = c("'2.4'", "3.5", "9"),
  result_cat = rep("negative", times = 3)
)

df_out <- map_cols(df_orig,
                   dataset_id = dataset_id,
                   id = id,
                   age_group = age,
                   sex = sex,
                   country = "Earth!",
                   collection_start_date = collection_start_date,
                   collection_end_date = collection_end_date,
                   result = result,
                   result_cat = result_cat,
                   include_others = FALSE)


df_out_should_be <- data.frame(
  dataset_id = 1,
  id = 1:3,
  age_group = c("0-10", "20-30", "30-40"),
  sex = c("female", "male", "other"),
  state = NA, county = NA, city = NA,
  collection_start_date = as.Date("2020-03-01"),
  collection_end_date = as.Date("2020-04-01"),
  test_id = NA,
  result = c(2.4, 3.5, 9.0),
  result_cat = rep("negative", times = 3)
)

test_that("mapping columns and validation works", {
  expect_identical(df_out, df_out_should_be)
})
