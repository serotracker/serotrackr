# Regions list

A nested named list of regions at three administrative (ADM) levels.
This list can be used in the
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
or
[`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md)
functions to rename geographic areas based on SeroTracker's predefined
list of region names. This will help automate the generation of
visualizations and analyses.

## Usage

``` r
regions
```

## Format

`regions` A list. Each element of the list returns a unique ID.

## Source

Based on the CGAZ dataset from
[geoBoundaries](https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/CGAZ).

## Details

adm0: Use this level to select your study's country.

adm1: Use this level to select your study's state/province.

adm2: Use this level to select your study's district/municipality or
equivalent division.

## Examples

``` r
regions$adm2$Canada$Alberta$Calgary
#> [1] "76498100B36899265297983"
```
