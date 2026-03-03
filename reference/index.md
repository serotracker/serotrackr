# Package index

## Main verbs

Use these functions, in the order below, to make your data ready for
submission.

- [`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
  **\[experimental\]** : Validate individual level data
- [`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md)
  **\[experimental\]** : Convert region names to codes
- [`st_aggregate()`](https://serotracker.github.io/serotrackr/reference/st_aggregate.md)
  **\[experimental\]** : Aggregate validated data
- [`st_save()`](https://serotracker.github.io/serotrackr/reference/st_save.md)
  **\[experimental\]** : Export data to Excel

## Built-in data

### Regions

Standard predefined regions, from country to municipality level. Both
objects below contain the same data in different formats. You should
mostly use `regions` in
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md)
and
[`st_locate()`](https://serotracker.github.io/serotrackr/reference/st_locate.md).
`regions_df` is only for your easy exploration of region options.

- [`regions`](https://serotracker.github.io/serotrackr/reference/regions.md)
  : Regions list
- [`regions_df`](https://serotracker.github.io/serotrackr/reference/regions_df.md)
  : Regions dataframe

### Assays

Standard predefined assays for different pathogens. Options for in-house
assays are also available. Both objects below contain the same data in
different formats. You should mostly use `assays` in
[`st_validate()`](https://serotracker.github.io/serotrackr/reference/st_validate.md).
`assays_df` is only for your easy exploration of assay options.

- [`assays`](https://serotracker.github.io/serotrackr/reference/assays.md)
  : Assays list
- [`assays_df`](https://serotracker.github.io/serotrackr/reference/assays_df.md)
  : Assays dataframe

### Sample data

- [`sample_raw_data`](https://serotracker.github.io/serotrackr/reference/sample_raw_data.md)
  : Example fictitious data
