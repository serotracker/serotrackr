
# Prerequisites -----------------------------------------------------------

library(dplyr)
library(stringr)
library(devtools)
load_all()
# TODO add warning for dropping NAs
# TODO add CIs, both binomial and bootstrapping.
# TODO add wrapper function for regions
# TODO test the package against other datasets

# TO DO -------------------------------------------------------------------

# TODO Add github logo and url to pkgdown
# TODO On `Reference` page of pkgdown website, put objects in meaningful groups
# TODO Remove `clean()`
# TODO Add R CMD Check and other banners to README
# TODO Add experimental, ... tags to pkgdown website
# TODO Add source link to each vignette
# TODO Explore creating a specific class for the output
# TODO Consider using readr::parse_number() for `age` and `result` values
# TODO Add another function to evaluate assays, including their test_cutoff

# FIXME Should ab_titer_unit remain an optional field?


