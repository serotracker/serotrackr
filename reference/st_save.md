# Export data to Excel

**\[experimental\]** Organizes individual level data and aggregate
estimates into a single xlsx file to be uploaded on the
[SeroTracker](https://serotracker.com) website.

## Usage

``` r
st_save(data, estimates, path)
```

## Arguments

- data:

  A validated data.frame, output of
  [`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)

- estimates:

  a data.frame, output of
  [`st_aggregate()`](https://serotracker.github.io/serotrackr/reference/st_aggregate.md)

- path:

  where to save the xlsx file

## Value

An xlsx file saved to disc

## Examples

``` r
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
#> ── Mapping columns and validating data ─────────────────────────────────────────
#> ✔ age_group is a valid column. [14ms]
#> ✔ age is a valid column. [22ms]
#> ✔ sex is a valid column. [8ms]
#> ✔ adm0 is a valid string. [5ms]
#> ✔ adm1 is a valid string. [5ms]
#> ✔ adm2 is a valid string. [8ms]
#> ✔ collection_start_date is a valid scalar. [7ms]
#> ✔ collection_end_date is a valid scalar. [12ms]
#> ✔ test_id is a valid string. [4ms]
#> ✔ result is a valid column. [13ms]
#> ✔ result_cat is a valid column. [6ms]
#> ✔ dataset_id is a valid column. [2ms]
#> ✔ id is a valid column. [7ms]
#> ── Validation finished ─────────────────────────────────────────────────────────
#> Success! Validated data created.

estimates <- st_aggregate(validated_df)

st_save(validated_df, estimates, path = tempdir())
```
