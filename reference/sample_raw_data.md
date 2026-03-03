# Example fictitious data

Example dataset to showcase the package's expectations from a raw
dataset and the package's workflow to make the raw data ready for
submission to SeroTracker 2.0.

## Usage

``` r
sample_raw_data
```

## Format

`sample_raw_data` A data frame with 100 rows and 13 columns:

- dataset_id:

  ID of each collection period

- id:

  Anonimized ID of each participant or sample

- age_group:

  Age group

- age:

  Age

- sex:

  Sex

- country:

  Administative level 0 (country) region names

- state:

  Administative level 1 (state/province) region names

- city:

  Administative level 2 (district/municipalities) region names

- start_date:

  Collection start date

- end_date:

  Collection end date

- test_id:

  Test ID

- result:

  Test results

- result_cat:

  Interpretation of test results; e.g. positive, negative, or borderline

## Source

Fictitious data
