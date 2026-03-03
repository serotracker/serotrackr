# Assays dataframe

Assay data for different pathogens.

## Usage

``` r
assays_df
```

## Format

`assays_df` A data frame with 309 rows and 16 columns:

- pathogen:

  Pathogen; currently only SARS-CoV-2 is available

- test_id:

  Unique descriptive ID of the test

- test_name:

  Test name

- isotype:

  Isotype

- test_type:

  Test type

- antibody_target:

  Antibody target

- rdt_test:

  RDT test: a boolean variable

- manufacturer:

  Manufacturer

- multiplex_detection:

  Multiplex detection: a boolean variable

- spike_antibody_target:

  Spike antibody target

- manufacturer_sensitivity:

  Manufacturer sensitivity

- manufacturer_specificity:

  Manufacturer specificity

- unique_identifier:

  Unique identifier

- info_page_url:

  Info page URL

- quantitative_qualitative:

  Quantitative and/or qualitative

- who_doherty_find_verified:

  WHO Doherty find verified

## Source

Based on the list of assays gathered by SeroTracker for SARS-CoV-2.
