## code to prepare `sample_raw_data` dataset goes here

# Will update this sample data over time to showcase different cleaning and
# validation considerations. This sample raw data is included in the package
# for users.

sample_raw_data <- tibble(
  dataset_id = 1,
  id = 1:3,
  age = c("0 - 10", "20-30", "30,40"),
  sex = c("f", "Male", "binary"),
  state = c("Alberta", "bc", "Québec"),
  city = c("calgary", "Metro vancouver", "Montréal"),
  collection_start_date = "2020-03-01",
  collection_end_date = "2020-04-01",
  result = c("'2.4'", "3.5", "9"),
  result_cat = rep("negative", times = 3)
)

usethis::use_data(sample_raw_data, overwrite = TRUE)
