# serotrackr (development version)

* `st_save()` replaced `save_xlsx()`. Apart from a more consistent naming scheme, the new function uses the output of both `st_validate()` and `st_aggregate()` functions to fill in as much cells as possible in the Excel template.
* Added initial version of `st_aggregate()` to generate aggregate estimates from validated individual level data.
* `st_validate()` replaced `map_cols()`. Naming of functions will be more consistent, while trying to avoid namespace conflicts. The new function accepts both columns and single values, checks for required input types (character, numeric, or date), applies several validation `rules` for each argument, and generates descriptive messages in the console about what parts of data are okay and what parts have issues.
* Added validation `rules` as an internal object. These rules will be used at several stages to check data.
* Added website using `pkgdown`. 
* Added a logo to the `README.md` file. It will be used in the pkgdown website too.

# serotrackr 0.1.0

* Initial release
* Added `map_cols()` function for mapping columns
* Added `clean()` function for cleaning and validating data
* Added `save_xlsx()` function for exporting data into the standard Excel template
* Assays and regions are organized in two nested named lists, so users can take advantage of RStudio to search their assays and regions in a dropdown-like behaviour
* Added documention to the current functions and data objects
* Added sample data to showcase what raw data is expected and how the package handles it
* Added package lifecycle information
