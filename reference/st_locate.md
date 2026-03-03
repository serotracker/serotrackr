# Convert region names to codes

**\[experimental\]** This function helps you convert your region names
to region codes acceptable by `serotrackr`. It is usually used when you
get an error for either `adm1` or `adm2` arguments in
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).
In that case, you can use this function to convert your region names to
codes and make sure they pass the validation by
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).

`st_locate()` uses its first three (or four) arguments to automatically
match your region names with `serotrackr`'s predefined region codes,
which are stored in
[`serotrackr::regions_df`](https://serotracker.github.io/serotrackr/reference/regions_df.md).
It performs exact case insensitive matching and also ignores accented
letters. It then produces a report of your region names that could not
be matched. You can then use `st_locate()`'s ... (ellipsis) argument to
define those unmatched region names.

## Usage

``` r
st_locate(
  data,
  adm0,
  adm1,
  adm2 = NULL,
  ...,
  into = c("adm1", "adm2"),
  n_unmatched_printed = 20
)
```

## Arguments

- data:

  A dataframe.

- adm0:

  A string representing one country (adm0) code. Use
  `serotrackr::regions$adm0$YourCountry` to select it.

- adm1, adm2:

  a string or an unquoted name of a character column that contains the
  state/province (adm1), or district/municipality (adm2) names or codes.
  If your study is conducted in only one adm1 or adm2 region, use
  [`serotrackr::regions`](https://serotracker.github.io/serotrackr/reference/regions.md)
  to select them.

- ...:

  A sequence of two-sided assignments as in
  `"region_name" = region_code`. `region_name` is your unmatched region
  name. `region_code` must be in the form of
  `regions$adm1$YourCountry$YourState` for unmatched adm1 regions and
  `regions$adm2$YourCountry$YourState$YourDistrict` for unmatched adm2
  regions. `st_locate()` uses
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  internally for this part.

- into:

  A character vector of length 1 or 2, specifying the name(s) of the new
  columns that are added to your data for adm1 (and adm2) region codes.
  If length is one, name of the adm1 column will be assigned. If length
  is two, names of adm1 and adm2 columns will be assigned, respectively.

- n_unmatched_printed:

  A single number, indicating the quantity of unmatched unique region
  names that are printed to the console. This argument is implemented to
  prevent potentially flooding your console with unmatched region names.
  The default value is 20. If there are more than 20 unmatched region
  names in your data, increase this number to see the rest.

## Value

A data.frame that consists of the input data.frame plus one (or two) new
columns containing adm1 (and adm2) region codes. You can then use these
two new columns for adm1 and/or adm2 arguments of
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).

## Examples

``` r
st_locate(
  data = sample_raw_data,
  adm0 = regions$adm0$Canada,
  adm1 = state,
  adm2 = city,
  "Toronoto" = regions$adm2$Canada$Ontario$Toronto,
  "Calagry"  = regions$adm2$Canada$Alberta$Calgary
)
#> ✔ adm1 region names/codes were successfully matched. `adm1` column was added.
#> ✔ adm2 region names/codes were successfully matched. `adm2` column was added.
#> # A tibble: 100 × 15
#>    dataset_id    id age_group   age sex   country state   city     start_date 
#>         <int> <int> <chr>     <dbl> <chr> <chr>   <chr>   <chr>    <chr>      
#>  1          1     1 65+         999 m     Canada  ontario Toronoto 2023/Jan/01
#>  2          1     2 0-17          8 f     Canada  ontario London   2023/Jan/01
#>  3          1     3 65+          67 m     Canada  Alberta Calagry  2023/Jan/01
#>  4          1     4 NA         -999 f     Canada  ontario toronto  2023/Jan/01
#>  5          1     5 0-17          9 m     Canada  ontario Toronoto 2023/Jan/01
#>  6          1     6 18-64        43 m     Canada  Alberta Edmonton 2023/Jan/01
#>  7          1     7 18-64        18 f     Canada  Alberta Edmonton 2023/Jan/01
#>  8          1     8 65+          83 f     Canada  Alberta Calgary  2023/Jan/01
#>  9          1     9 65+          77 f     Canada  Alberta Calgary  2023/Jan/01
#> 10          1    10 65+          77 m     Canada  Alberta Calagry  2023/Jan/01
#> # ℹ 90 more rows
#> # ℹ 6 more variables: end_date <chr>, test_id <chr>, result <dbl>,
#> #   result_cat <chr>, adm1 <chr>, adm2 <chr>
```
