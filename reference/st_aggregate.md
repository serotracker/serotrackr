# Aggregate validated data

**\[experimental\]** Generate aggregate estimates based on validated
individual level data.

## Usage

``` r
st_aggregate(
  data,
  subgroup = c("age_group", "sex", "age_group + sex"),
  borderline = c("negative", "positive", NA),
  add_ci = TRUE,
  round_digits = 4,
  test_combination = NULL
)
```

## Arguments

- data:

  A validated data.frame that is the output of
  [`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)

- subgroup:

  A character vector of subgrouping variables. By default, aggregate
  estimates are generated for the overall data, as well as age group,
  sex, and age group + sex subgroups.

- borderline:

  How should borderline results be treated? Default is as negative.

- add_ci:

  Boolean. Whether to add binomial proportion confidence interval. It is
  calculated using the Wilson score interval method through the
  [`binom::binom.confint()`](https://rdrr.io/pkg/binom/man/binom.confint.html)
  function.

- round_digits:

  Integer indicating the number of decimal places of the estimate. It is
  passed to the digits argument of
  [`base::round()`](https://rdrr.io/r/base/Round.html).

- test_combination:

  Not functional yet. When data is based on more than one assay, what is
  the relationship between those assays?

## Value

A summarized data.frame

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
#> ✔ age_group is a valid column. [328ms]
#> ✔ age is a valid column. [16ms]
#> ✔ sex is a valid column. [8ms]
#> ✔ adm0 is a valid string. [61ms]
#> ✔ adm1 is a valid string. [6ms]
#> ✔ adm2 is a valid string. [10ms]
#> ✔ collection_start_date is a valid scalar. [116ms]
#> ✔ collection_end_date is a valid scalar. [22ms]
#> ✔ test_id is a valid string. [5ms]
#> ✔ result is a valid column. [6ms]
#> ✔ result_cat is a valid column. [6ms]
#> ✔ dataset_id is a valid column. [2ms]
#> ✔ id is a valid column. [8ms]
#> ── Validation finished ─────────────────────────────────────────────────────────
#> Success! Validated data created.

st_aggregate(validated_df)
#> # A tibble: 26 × 27
#>    dataset_id subgroup  strata age_group age_min age_max sex    pop_adj test_adj
#>         <int> <chr>     <chr>  <chr>       <dbl>   <dbl> <chr>  <lgl>   <lgl>   
#>  1          1 overall   NA     All            NA      NA All    FALSE   FALSE   
#>  2          2 overall   NA     All            NA      NA All    FALSE   FALSE   
#>  3          1 age_group 0-17   0-17            0      17 All    FALSE   FALSE   
#>  4          1 age_group 18-64  18-64          18      64 All    FALSE   FALSE   
#>  5          1 age_group 65+    65+            NA      NA All    FALSE   FALSE   
#>  6          1 age_group NA     NA             NA      NA All    FALSE   FALSE   
#>  7          2 age_group 0-17   0-17            1      17 All    FALSE   FALSE   
#>  8          2 age_group 18-64  18-64          21      57 All    FALSE   FALSE   
#>  9          2 age_group 65+    65+            NA      NA All    FALSE   FALSE   
#> 10          1 sex       Female All            NA      NA Female FALSE   FALSE   
#> # ℹ 16 more rows
#> # ℹ 18 more variables: adm1 <chr>, adm2 <chr>, start_date <date>,
#> #   end_date <date>, test_id_1 <chr>, test_id_2 <chr>, test_id_3 <chr>,
#> #   test_combination <lgl>, numerator <dbl>, denominator <int>, seroprev <dbl>,
#> #   seroprev_95_ci_lower <dbl>, seroprev_95_ci_upper <dbl>,
#> #   ab_denominator <int>, ab_titer_min <dbl>, ab_titer_max <dbl>,
#> #   ab_titer_mean <dbl>, ab_titer_sd <dbl>
```
