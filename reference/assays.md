# Assays list

A list of assays for different pathogens. This list can be used in the
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
function to select assays.

## Usage

``` r
assays
```

## Format

`assays` A nested named list

## Source

Based on the list of assays gathered by SeroTracker for SARS-CoV-2.

## Examples

``` r
assays$`SARS-CoV-2`$`EUROIMMUN - IgG - Anti-SARS-CoV-2 ELISA IgG`
#> [1] "EUROIMMUN - IgG - Anti-SARS-CoV-2 ELISA IgG"
```
