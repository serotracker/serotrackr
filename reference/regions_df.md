# Regions dataframe

Regions data at three administrative levels

## Usage

``` r
regions_df
```

## Format

`regions_df` A data frame with 53087 rows and 6 columns:

- shapeGroup:

  ISO 3166 code for each country

- shapeType:

  Aministrative (ADM) level of the region. ADM0: country; ADM1:
  state/province; ADM2: district/municipality/or equivalent

- NAME_0:

  Country name of the region

- NAME_1:

  State/province name of the region

- NAME_2:

  District/municipality/ or equivalent name of the region

- shapeID_v5:

  Unique ID for the region and administartive level

## Source

Based on the CGAZ dataset from
[geoBoundaries](https://github.com/wmgeolab/geoBoundaries/tree/main/releaseData/CGAZ).
