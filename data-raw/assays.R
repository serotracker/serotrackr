## code to prepare `assays_df` and `assays` datasets goes here

# Removed the source "inst/extdata/Immunoassays.csv" file after running this
# script to keep the packages size under 5 Mb. This "Immunoassays.csv" file
# was exported directly from Airtable.

library(tidyverse)
library(janitor)


# Clean assay data from Airtable ------------------------------------------

assays_df <- read_csv("inst/extdata/Immunoassays.csv") %>%
  clean_names() %>%
  select(-c(rapid_review_estimates, studies, admin_checked)) %>%
  mutate(
    pathogen = "SARS-CoV-2", .before = 0,
    across(c("rdt_test", "multiplex_detection"),
           ~ case_when(. == "checked" ~ TRUE,
                       is.na(.) ~ FALSE))
  )


# Generate nested named list of assays ------------------------------------

assays <- list()
for (i in 1:length(unique(assays_df$pathogen))) {
  temp <- assays_df %>%
    filter(pathogen == unique(assays_df$pathogen)[i]) %>%
    select(test_id) %>%
    pivot_wider(names_from = test_id, values_from = test_id) %>%
    as.list()
  assays[[unique(assays_df$pathogen)[i]]] <- c(assays[[unique(assays_df$pathogen)[i]]],
                                               temp)
}


# Export data -------------------------------------------------------------

usethis::use_data(assays_df, overwrite = TRUE)
usethis::use_data(assays, overwrite = TRUE)
