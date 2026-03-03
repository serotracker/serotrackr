# serotrackr

*serotrackr* helps you prepare your raw data for submission to
[SeroTracker](https://serotracker.com). I will use parts of a sample raw
dataset included in *serotrackr* to demonstrate the workflow of the
package:

``` r
library(serotrackr)
```

``` r
mydata <- dplyr::select(
  sample_raw_data,
  dataset_id, id, age_group, age, sex, state, city, result, result_cat
)

mydata
```

    #> # A tibble: 100 × 9
    #>    dataset_id    id age_group   age sex   state   city     result result_cat
    #>         <int> <int> <chr>     <dbl> <chr> <chr>   <chr>     <dbl> <chr>     
    #>  1          1     1 65+         999 m     ontario Toronoto   20.5 negative  
    #>  2          1     2 0-17          8 f     ontario London    148.  positive  
    #>  3          1     3 65+          67 m     Alberta Calagry    13.5 negative  
    #>  4          1     4 NA         -999 f     ontario toronto   101.  positive  
    #>  5          1     5 0-17          9 m     ontario Toronoto   88.2 positive  
    #>  6          1     6 18-64        43 m     Alberta Edmonton  147.  positive  
    #>  7          1     7 18-64        18 f     Alberta Edmonton  213.  positive  
    #>  8          1     8 65+          83 f     Alberta Calgary   364.  positive  
    #>  9          1     9 65+          77 f     Alberta Calgary   227.  positive  
    #> 10          1    10 65+          77 m     Alberta Calagry   271.  positive  
    #> # ℹ 90 more rows

Here are the steps you should follow:

## 1) Validate your data

The first step is to use
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).
It checks your individual-level raw data for common issues, prints a
detailed explanation of any errors, and if all is good, outputs a
cleaned and validated dataframe. Most of the arguments of this function
accept both an unquoted column name or a length-one vector, the value of
which will be applied to all records in your data after validation.

``` r
validated_df <- st_validate(
  sample_raw_data,
  dataset_id = dataset_id,
  id = id,
  age_group = age_group,
  age = age,
  sex = sex,
  adm0 = regions$adm0$Canada,
  adm1 = state,
  adm2 = city,
  collection_start_date = "2020-Mar-01",
  collection_end_date = "15/8/2023",
  test_id = assays$`SARS-CoV-2`$`ID.Vet - IgG - ID Screen`,
  result = result,
  result_cat = result_cat,
  include_others = TRUE,
  rmd_safe = TRUE
)
```

    #> ── Mapping columns and validating data ─────────────────────────────────────────
    #> ✔ age_group is a valid column. [220ms]
    #> ✖ age [40ms]
    #>   • can't be negative. 1 record has this issue. Invalid value is -999.
    #>   • can't be larger than 120 years. 3 records have this issue. Invalid value is
    #>     999.
    #> ✔ sex is a valid column. [11ms]
    #> ✔ adm0 is a valid string. [73ms]
    #> ✖ adm1 codes were not found. Use `st_locate()` or
    #>   `serotrackr::regions$adm1$YourCountry$YourState`. 100 records have this
    #>   issue. Invalid values are "ontario" and "Alberta". [31ms]
    #> ✖ adm2 codes were not found. Use `st_locate()` or
    #>   `serotrackr::regions$adm2$YourCountry$YourState$YourDistrict`. 100 records
    #>   have this issue. Invalid values are "Toronoto", "London", "Calagry", ….
    #>   [109ms]
    #> ✔ collection_start_date is a valid scalar. [134ms]
    #> ✔ collection_end_date is a valid scalar. [13ms]
    #> ✔ test_id is a valid string. [5ms]
    #> ✔ result is a valid column. [6ms]
    #> ✔ result_cat is a valid column. [6ms]
    #> ✔ dataset_id is a valid column. [1ms]
    #> ✔ id is a valid column. [7ms]
    #> ── Validation finished ─────────────────────────────────────────────────────────
    #> Error in `st_validate()`:
    #> ! 4 errors! Please address them first. Validated data not created.
    #> Run `rlang::last_trace()` to see where the error occured.

  
As you can see above,
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
found 4 issues which must be addressed before proceeding.

## 2) Address issues

### 2.1) General errors

I start with correcting the `age` issues, both of which seem to result
from indicating missing values with either `-999` or `999`. In general,
any missingness must be represented by `NA` values before running
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).
Here is a quick fix:

``` r
mydata <- dplyr::mutate(
  mydata,
  age = ifelse(age %in% c(-999, 999), NA, age)
)
```

### 2.2) Errors in region data

The `adm1` (state/province) and `adm2` (district/municipality) errors
happened because
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
expects standardized region codes, not region names, for these two
arguments. These region codes are stored in the
[`serotrackr::regions`](https://serotracker.github.io/serotrackr/reference/regions.md)
object, which is a named list.

- If you conducted your study in only one region, simply use `regions`
  to get its region code.
- If you conducted your study in multiple regions and have a column in
  your data for it, similar to our case here, then use
  [`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md).
  It will automatically convert as much of your region names as possible
  to region codes. The rest must be defined by you, as below.

First, run
[`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md)
with its first four arguments to see what is wrong with your region
data:

``` r
st_locate(data = mydata,
          adm0 = regions$adm0$Canada,
          adm1 = state,
          adm2 = city)
```

    #> ✔ adm1 region names/codes were successfully matched. `adm1` column was added.
    #> ✖ adm2 2 of 6 unique adm2 region names/codes were not matched. Use the ...
    #>   argument in `st_locate()` to define them. Unmatched values are: "Toronoto"
    #>   and "Calagry".

Ah, typos! Next, use the … argument to define the unmatched ones:

``` r
mydata <- st_locate(
  data = mydata,
  adm0 = regions$adm0$Canada,
  adm1 = state,
  adm2 = city,
  "Toronoto" = regions$adm2$Canada$Ontario$Toronto,
  "Calagry"  = regions$adm2$Canada$Alberta$Calgary
)
```

    #> ✔ adm1 region names/codes were successfully matched. `adm1` column was added.
    #> ✔ adm2 region names/codes were successfully matched. `adm2` column was added.

[`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md)
added two new columns to the data, named `adm1` and `adm2`, containing
standardized region codes. Now that all issues are resolved, let’s rerun
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md):

``` r
validated_df <- st_validate(
  mydata,
  dataset_id = dataset_id,
  id = id,
  age_group = age_group,
  age = age,
  sex = sex,
  adm0 = regions$adm0$Canada,
  adm1 = adm1,
  adm2 = adm2,
  collection_start_date = "2020-Mar-01",
  collection_end_date = "15/8/2023",
  test_id = assays$`SARS-CoV-2`$`ID.Vet - IgG - ID Screen`,
  result = result,
  result_cat = result_cat,
  include_others = TRUE,
  rmd_safe = TRUE
)
```

    #> ── Mapping columns and validating data ─────────────────────────────────────────
    #> ✔ age_group is a valid column. [14ms]
    #> ✔ age is a valid column. [15ms]
    #> ✔ sex is a valid column. [7ms]
    #> ✔ adm0 is a valid string. [6ms]
    #> ✔ adm1 is a valid column. [7ms]
    #> ✔ adm2 is a valid column. [11ms]
    #> ✔ collection_start_date is a valid scalar. [7ms]
    #> ✔ collection_end_date is a valid scalar. [12ms]
    #> ✔ test_id is a valid string. [4ms]
    #> ✔ result is a valid column. [6ms]
    #> ✔ result_cat is a valid column. [6ms]
    #> ✔ dataset_id is a valid column. [1ms]
    #> ✔ id is a valid column. [7ms]
    #> ── Validation finished ─────────────────────────────────────────────────────────
    #> Success! Validated data created.

  
Perfect! Here is a glimpse at the validated data:

``` r
validated_df
```

    #> # A tibble: 100 × 14
    #>    dataset_id    id age_group   age sex    adm1      adm2  collection_start_date
    #>         <int> <int> <chr>     <dbl> <chr>  <chr>     <chr> <date>               
    #>  1          1     1 65+          NA Male   4576071B… 7649… 2020-03-01           
    #>  2          1     2 0-17          8 Female 4576071B… 7649… 2020-03-01           
    #>  3          1     3 65+          67 Male   4576071B… 7649… 2020-03-01           
    #>  4          1     4 NA           NA Female 4576071B… 7649… 2020-03-01           
    #>  5          1     5 0-17          9 Male   4576071B… 7649… 2020-03-01           
    #>  6          1     6 18-64        43 Male   4576071B… 7649… 2020-03-01           
    #>  7          1     7 18-64        18 Female 4576071B… 7649… 2020-03-01           
    #>  8          1     8 65+          83 Female 4576071B… 7649… 2020-03-01           
    #>  9          1     9 65+          77 Female 4576071B… 7649… 2020-03-01           
    #> 10          1    10 65+          77 Male   4576071B… 7649… 2020-03-01           
    #> # ℹ 90 more rows
    #> # ℹ 6 more variables: collection_end_date <date>, test_id <chr>, result <dbl>,
    #> #   result_cat <chr>, state <chr>, city <chr>

## 3) Generate aggregate estimates

Now, use
[`st_aggregate()`](https://serotracker.github.io/serotrackr/reference/st_aggregate.md)
to generate aggregated estimates from the validated data:

``` r
estimates <- st_aggregate(validated_df)
```

And here are the generated estimates:

``` r
estimates
```

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

## 4) Export to Excel for submission

Finally, use
[`st_save()`](https://serotracker.github.io/serotrackr/reference/st_save.md)
to export both validated data and generated estimates to an Excel
document:

``` r
st_save(validated_df, estimates, path = "submission.xlsx")
```

The saved Excel document has six sheets, including `Instructions`,
`Study Metadata`, `Age Groups`, `Immunoassays`, `Estimates`, and `Data`.

- The output of
  [`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
  fills in the *Age Groups* and *Data* sheets completely. It also fills
  in parts of the *Study Metadata* and *Immunoassays* sheets, as much as
  possible.
- The output of
  [`st_aggregate()`](https://serotracker.github.io/serotrackr/reference/st_aggregate.md)
  fills in the *Estimates* sheet.

Open the Excel document and specifically look for empty mandatory
columns, indicated with colour, in *Study Metadata* and *Immunoassays*
sheets, and fill them in. When you are done, the document is ready to be
submitted on [SeroTracker.com](https://serotracker.com).
