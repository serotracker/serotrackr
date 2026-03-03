# Validate individual level data

**\[experimental\]** Locates the required and optional columns in your
data and validates them.

## Usage

``` r
st_validate(
  data,
  dataset_id,
  id,
  adm0,
  adm1 = NULL,
  adm2 = NULL,
  collection_start_date,
  collection_end_date,
  test_id,
  result,
  result_cat = NULL,
  age_group = NULL,
  age = NULL,
  sex = NULL,
  include_others = TRUE,
  rmd_safe = FALSE
)
```

## Arguments

- data:

  a data.frame.

- dataset_id:

  An unquoted column name or a length-one vector that differentiates the
  data collection event(s).

- id:

  column for anonymized individual level IDs. This column will be used
  to generate aggregate estimates.

- adm0, adm1, adm2:

  a string or an unquoted name of a character column that contains the
  country (adm0), state/province (adm1), or district/municipality (adm2)
  codes. Use
  [`serotrackr::regions`](https://serotracker.github.io/serotrackr/reference/regions.md)
  to select these. Only one adm0 is acceptable. adm1 and adm2 can be
  more.

- collection_start_date, collection_end_date:

  Unquoted name of a date or character column or a date or string scalar
  (vector of length one) for sampling start and end dates.
  [`lubridate::parse_date_time2()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
  is used to parse dates. Only `yyyy-mm-dd` or `dd-mm-yyyy` structures
  are acceptable. It recognize arbitrary non-digit separators as well as
  no separator. Month Can be entered as a digit or a full or abbreviated
  name.

- test_id:

  a string or an unquoted name of a character column that contains the
  test IDs. Use
  [`serotrackr::assays`](https://serotracker.github.io/serotrackr/reference/assays.md)
  to select these.

- result:

  Unquoted name of a numeric column containing test results.

- result_cat:

  Unquoted name of a character column with values of `positive`,
  `borderline`, or `negative`, ignoring case. A single string is also
  acceptable.

- age_group:

  Unquoted name of a character column or a string containing age
  group(s). The only structures acceptable are `number-number` or
  `number+`. E.g. 18-64, and 65+.

- age:

  Unquoted name of a numeric column or a single number. Acceptable
  values are between 0 and 120 inclusive.

- sex:

  Unquoted name of a character column or a string. Acceptable values
  are: `f`, `m`, `o`, `female`, `male`, or `other` ignoring case.

- include_others:

  include additional columns or not

- rmd_safe:

  Logical. If TRUE, the output message will be appropriate for R
  markdown, i.e. progress indicators are removed and all the messages
  are printed at the same time, making only one chunk in the R
  markdown's knitted output. If FALSE (default), the progress indicators
  and messages are printed for each argument one by one, making it
  appropriate for interactive use.

## Value

A validated data.frame

## Examples

``` r
st_validate(
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
  include_others = TRUE,
  rmd_safe = TRUE
)
#> ── Mapping columns and validating data ─────────────────────────────────────────
#> ✔ age_group is a valid string. [16ms]
#> ✔ sex is a valid string. [7ms]
#> ✔ adm0 is a valid string. [5ms]
#> ✔ adm1 is a valid string. [6ms]
#> ✔ adm2 is a valid string. [9ms]
#> ✔ collection_start_date is a valid scalar. [8ms]
#> ✔ collection_end_date is a valid scalar. [13ms]
#> ✔ test_id is a valid string. [4ms]
#> ✔ result is a valid column. [6ms]
#> ✔ result_cat is a valid string. [5ms]
#> ✔ dataset_id is a valid column. [2ms]
#> ✔ id is a valid column. [8ms]
#> ── Validation finished ─────────────────────────────────────────────────────────
#> Success! Validated data created.
#> # A tibble: 100 × 16
#>    dataset_id    id age_group sex   adm1             adm2  collection_start_date
#>         <int> <int> <chr>     <chr> <chr>            <chr> <date>               
#>  1          1     1 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  2          1     2 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  3          1     3 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  4          1     4 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  5          1     5 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  6          1     6 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  7          1     7 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  8          1     8 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#>  9          1     9 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#> 10          1    10 12-17     Male  4576071B9681799… 7649… 2023-01-01           
#> # ℹ 90 more rows
#> # ℹ 9 more variables: collection_end_date <date>, test_id <chr>, result <dbl>,
#> #   result_cat <chr>, country <chr>, state <chr>, city <chr>, start_date <chr>,
#> #   end_date <chr>
```
