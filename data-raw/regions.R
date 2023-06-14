## code to prepare `regions` dataset goes here

# Prerequisites -----------------------------------------------------------

library(tidyverse)
library(rgeoboundaries)
library(sf)
library(openxlsx)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# I preferred geographic data (polygons) from geoBoundaries, instead of GADM,
# as it is getting frequent updates and their shapes are more simplified
# (lighter to run).
# geoBoundaries v5.0.0; github commit: 9de7206

adm0_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM0.zip"
adm1_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM1.zip"
adm2_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM2.zip"
meta_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/geoBoundariesOpen-meta.csv"



# Metadata ----------------------------------------------------------------

meta <- read_csv(meta_url) %>%
  select(boundaryID, boundaryName, boundaryISO, boundaryYearRepresented,
         boundaryType, boundaryCanonical, Continent,
         UNSDG_region = `UNSDG-region`, UNSDG_subregion = `UNSDG-subregion`,
         worldBankIncomeGroup, admUnitCount) %>%
  # There are two mistakes in metadata for boundaryType:
  mutate(boundaryType = case_when(boundaryID == "LBN-ADM0-11630461" ~ "ADM0",
                                  boundaryID == "AUS-ADM1-45165765" ~ "ADM1",
                                  TRUE ~ boundaryType))



# geoBoundaries v4 --------------------------------------------------------

# The rgeoboundaries package only is updated until version 4 of the maps, while
# their github has been updated to version 5, with more accurate regions. It
# also adds two separate lines in the adm0 sf object for disputed areas. On the
# hand version 4 has hierarchy info for each regions. Thus, I will join versions
# 4 and 5 to import as much possible as those hierarchy info into version 5 maps.
adm0_gb_v4 <- gb_adm0(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)
adm1_gb_v4 <- gb_adm1(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)
adm2_gb_v4 <- gb_adm2(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)




# geoBoundaries v5 --------------------------------------------------------
# ADM 0 -------------------------------------------------------------------

download_shp <- function(url) {
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(url, temp)
  unzip(zipfile = temp, exdir = temp2)
  data <- read_sf(temp2)
  # %>% tibble() %>% select(-geometry)
  unlink(c(temp, temp2))
  return(data)
}

# SeroTracker's map does not show any data for disputed territories, although
# there is data available for some of them in Airtable.

# Contested areas are based on US Department of State definition:
# 2023-06-03
# Geoboundaries v5.0
adm0_gb_v5_temp <- download_shp(adm0_url) %>%
  left_join(meta %>% filter(boundaryType == "ADM0"),
            by = c("shapeGroup" = "boundaryISO",
                   "shapeType" = "boundaryType")) %>%
  mutate(shapeGroup = ifelse(row_number() == nrow(.), "ESH", shapeGroup),
         shapeType = ifelse(is.na(shapeType), "ADM0", shapeType),
         boundaryName = case_when(row_number() == nrow(.) ~
                                    "Disputed territory of Western Sahara",
                                  row_number() == nrow(.) - 1 ~
                                    "Other disputed territories",
                                  TRUE ~ boundaryName),
         boundaryCanonical = ifelse(row_number() == nrow(.),
                                    "Western Sahara",
                                    boundaryCanonical),
         Continent = case_when(row_number() == nrow(.) ~ "Africa",
                               row_number() == nrow(.) - 1 ~ "Mixed",
                               TRUE ~ boundaryName)) %>%
  left_join(adm0_gb_v4 %>% select(shapeGroup, shapeID_v4 = shapeID)) %>%
  mutate(boundaryName = ifelse(boundaryName == "Holy See",
                               "Vatican City", boundaryName),
         across(c(boundaryName, Continent),
                ~ ifelse(. == "Antartica", "Antarctica", .)))



# ADM 1 -------------------------------------------------------------------

adm1_gb_v5_temp <- download_shp(adm1_url) %>%
  mutate(shapeName = case_when(row_number() == nrow(.) ~
                                 "Disputed territory of Western Sahara",
                               row_number() == nrow(.) - 1 ~
                                 "Other disputed territories",
                               TRUE ~ shapeName),
         shapeGroup = ifelse(row_number() == nrow(.), "ESH", shapeGroup),
         shapeType = ifelse(is.na(shapeType), "ADM1", shapeType)) %>%
  rename(shapeID_v5 = shapeID) %>%
  left_join(adm1_gb_v4 %>% rename(shapeID_v4 = shapeID, shapeISO_v4 = shapeISO),
            by = c("shapeName", "shapeGroup", "shapeType")) %>%
  mutate(
    # There are 2 ADM0s (Antarctica and Vatican City) in this ADM1 object.
    # Replacing both with ADM1:
    shapeType = "ADM1"
  ) %>%
  relocate(geometry, .after = last_col())


# The ADM1 data has a mistake between two adjacent provinces of Iran. The
# shapeID_v5 '17685810B88307134464360' is labeled as Mazandaran province but
# it is a subset of the Golestan province and should be combined with it:
Golestan_Iran <- adm1_gb_v5_temp %>%
  filter(shapeName == "Golestan") %>%
  tibble() %>% select(-geometry)

adm1_gb_v5 <- adm1_gb_v5_temp %>%
  filter(shapeID_v5 == "17685810B88307134464360" | shapeName == "Golestan") %>%
  st_union() %>% st_as_sf() %>% rename(geometry = x) %>%
  mutate(shapeName = "Golestan") %>%
  left_join(Golestan_Iran, by = "shapeName") %>%
  bind_rows(
    adm1_gb_v5_temp %>%
      filter(
        (shapeID_v5 != "17685810B88307134464360" | is.na(shapeID_v5)) &
          shapeName != "Golestan"
      )
  ) %>%
  tibble() %>% select(-geometry)


# 'Antarctica' and 'Vatican City' have ADM1 shapes but not ADM0; Adding them:
adm0_gb_v5 <- adm0_gb_v5_temp %>%
  left_join(
    by = join_by("boundaryName" == "shapeName"),
    adm1_gb_v5 %>%
      filter(shapeName %in% c("Antarctica", "Vatican City")) %>%
      tibble() %>% select(shapeName, shapeID_v5)
  ) %>%
  tibble() %>% select(-geometry)



# ADM 2 -------------------------------------------------------------------

# adm2_gb_v5 does not have info about adm1 while some adm2s have the same name
# in different adm1s. Thus, to join adm2_gb_v5 with adm2_gb_v4, there is no way
# other than removing all 'duplicate' adm2s from adm2_gb_v4 (which has adm1 info).
adm2_gb_v4_duplicates <- adm2_gb_v4 %>%
  group_by(shapeGroup, shapeName) %>%
  filter(n() > 1) %>%
  pull(shapeName)


adm2_gb_v5_temp <- download_shp(adm2_url) %>%
  mutate(shapeName = ifelse(!is.na(`_NAME`), str_to_title(`_NAME`), shapeName),
         shapeGroup = ifelse(!is.na(`_NAME`), "NPL", shapeGroup),
         shapeType = ifelse(!is.na(`_NAME`), "ADM2", shapeType)) %>%
  select(-`_NAME`) %>%
  mutate(
    shapeName = case_when(
      # Correcting names of disputed areas:
      row_number() == nrow(.) ~
        "Disputed territory of Western Sahara",
      row_number() == nrow(.) - 1 ~
        "Other disputed territories",
      # Correcting regions with empty shapeNames:
      # China:
      is.na(shapeName) & shapeGroup=="CHN" ~ "Coloane",
      # Japan:
      shapeID == "22064153B75262279397781" ~ "Fudai",
      shapeID == "22064153B12842637075102" ~ "Kuzumaki",
      shapeID == "22064153B92573372325719" ~ "Ichinohe",
      shapeID == "22064153B72513259792767" ~ "Kunohe",
      shapeID == "22064153B5973750369308" ~ "Sugito",
      shapeID == "22064153B75464560081216" ~ "Miyashiro",
      shapeID == "22064153B17480769802436" ~ "Tsuihiji",
      shapeID == "22064153B18942901939115" ~ "Ina",
      shapeID == "22064153B47047765932767" ~ "Kawajima",
      shapeID == "22064153B9982991100284" ~ "Horai", # May be duplicate
      shapeID == "22064153B50938787142947" ~ "Hatoyama",
      shapeID == "22064153B99043530024040" ~ "Moroyama",
      shapeID == "22064153B26815433111761" ~ "Ogose",
      shapeID == "22064153B84605806147634" ~ "Tokigawa",
      shapeID == "22064153B64517460299658" ~ "Yokoze",
      shapeID == "22064153B21581741830998" ~ "Ranzan",
      shapeID == "22064153B89628470633091" ~ "Namegawa",
      shapeID == "22064153B8757606797576" ~ "Ogawa",
      shapeID == "22064153B75672377109819" ~ "Higashichichibu",
      shapeID == "22064153B51907857293316" ~ "Minano",
      shapeID == "22064153B54074786432153" ~ "Nagatoro",
      shapeID == "22064153B41987381853177" ~ "Yorii",
      shapeID == "22064153B99185493380850" ~ "Misato",
      shapeID == "22064153B37976585868832" ~ "Ogano",
      # Turkmenistan:
      shapeID == "10190150B97616474528788" ~ "Untitled near Serhetabat",
      shapeID == "10190150B77295551410861" ~ "Untitled near Dostluk",
      shapeID == "10190150B32178227833242" ~ "Untitled near Halac",
      shapeID == "10190150B60217400651168" ~ "Untitled near Hojambaz",
      shapeID == "10190150B83944292759317" ~ "Untitled near Sayat",
      shapeID == "10190150B6764412650644" ~ "Untitled near Yoloten",
      shapeID == "10190150B28802506706390" ~ "Untitled near Turkmengala",
      shapeID == "10190150B67149572938666" ~ "Untitled near Murgap",
      shapeID == "10190150B35521000351468" ~ "Untitled near Bayramaly",
      shapeID == "10190150B10245353541745" ~ "Untitled near Wekilbazar",
      shapeID == "10190150B76598701083647" ~ "Untitled near Sakarcage",
      shapeID == "10190150B82053390161036" ~ "Untitled near Darganata",
      TRUE ~ shapeName
    ),
    shapeGroup = ifelse(row_number() == nrow(.), "ESH", shapeGroup),
    # There are 2 ADM0s and 312 ADM1s in this ADM2 object. Replacing
    # all with ADM2, but DOUBLE CHECK later (likely correct):
    shapeType = ifelse(is.na(shapeType), "ADM2", "ADM2")
  ) %>%
  rename(shapeID_v5 = shapeID) %>%
  left_join(
    by = c("shapeName", "shapeGroup", "shapeType"),
    adm2_gb_v4 %>%
      rename(shapeID_v4 = shapeID, shapeISO_v4 = shapeISO) %>%
      filter(!(shapeName %in% adm2_gb_v4_duplicates))
  )

# Remove version 4 data from those entries in version 5 that repeat 2 or more
# times. This is done because it is unclear to which ADM1s those repeated
# (duplicated) ADM2s belong. Therefore, they are removed here and should be
# checked later while completeing the hierarchy info by hand:
adm2_gb_v5_duplicates <- adm2_gb_v5_temp %>%
  group_by(shapeGroup, shapeName) %>%
  filter(n() > 1) %>%
  pull(shapeName) %>%
  unique()

adm2_gb_v5 <- adm2_gb_v5_temp %>%
  mutate(
    across(
      c(shapeISO_v4, shapeID_v4, ADM1_shape, ADM0_shape, ADMHIERARC),
      ~ ifelse(shapeName %in% adm2_gb_v5_duplicates, NA, .)
    )
  ) %>%
  tibble() %>% select(-geometry)


format(object.size(adm2_gb_v5), units = "Mb")



# Binding rows of ADM0, ADM1, and ADM2 ------------------------------------

merged_temp <- adm0_gb_v5 %>%
  mutate(shapeISO_v4 = NA, ADM0_shape = NA, ADM1_shape = NA, ADMHIERARC = NA) %>%
  select(shapeName = boundaryName, shapeID_v5, shapeGroup, shapeType,
         shapeISO_v4, shapeID_v4, ADM1_shape, ADM0_shape, ADMHIERARC) %>%
  bind_rows(
    adm1_gb_v5 %>%
      mutate(ADM1_shape = NA) %>%
      select(shapeName, shapeID_v5, shapeGroup, shapeType,
             shapeISO_v4, shapeID_v4, ADM1_shape, ADM0_shape, ADMHIERARC),
    adm2_gb_v5
  ) %>%
  group_by(shapeGroup) %>%
  mutate(
    ADMHIERARC2 = ifelse(
      !is.na(ADMHIERARC),
      sapply(ADMHIERARC, function(x){
        paste(shapeName[which(shapeID_v4 %in%
                                unlist(strsplit(x, "|", fixed = TRUE)))],
              collapse = "|")}),
      NA
    )
  ) %>%
  separate_wider_delim(cols = ADMHIERARC2, delim = "|", too_few = "align_start",
                       names = c("NAME_0", "NAME_1", "NAME_2")) %>%
  ungroup() %>%
  # For some ADM2 records, corresponding ADM1s were not found during joining
  # version 5 data with version 4. As a result, after doing separate_wider_delim(),
  # their ADM2 name was written to NAME_1, instead of NAME_2. To correct that:
  mutate(
    NAME_2_dup = NAME_2,
    NAME_2 = ifelse(shapeType=="ADM2" & !is.na(NAME_1) & is.na(NAME_2),
                    NAME_1, NAME_2),
    NAME_1 = ifelse(shapeType=="ADM2" & !is.na(NAME_1) & is.na(NAME_2_dup),
                    NA, NAME_1),
  ) %>%
  select(-NAME_2_dup) %>%
  # Fill in missing shapeIDs (unique ID of each shape):
  mutate(shapeID_v5 = ifelse(is.na(shapeID_v5),
                             as.character(row_number()), shapeID_v5))


# For regions with the same shapeName across ADM levels, make their shapeIDs unique:
shapeID_v5_duplicate <- merged_temp %>%
  group_by(shapeID_v5) %>%
  filter(n() > 1) %>%
  pull(shapeID_v5)


merged_final <- merged_temp %>%
  left_join(adm0_gb_v5 %>% select(shapeGroup, boundaryName_adm0 = boundaryName),
            by = "shapeGroup") %>%
  mutate(
    shapeID_v5 = ifelse(shapeID_v5 %in% shapeID_v5_duplicate,
                        paste0(shapeID_v5, shapeType),
                        shapeID_v5),
    NAME_0 = ifelse(is.na(NAME_0), boundaryName_adm0, NAME_0),
    NAME_1 = ifelse(shapeType=="ADM1" & is.na(NAME_1), shapeName, NAME_1),
    NAME_2 = ifelse(shapeType=="ADM2" & is.na(NAME_2), shapeName, NAME_2)
  ) %>%
  select(-boundaryName_adm0)


regions_df <- merged_final %>%
  select(shapeGroup, shapeType, NAME_0, NAME_1, NAME_2, shapeID_v5)



# Write to xlsx for external hierarchy checking ---------------------------

# adm1_hierar_checking <- merged_final %>%
#   filter(shapeType == "ADM1") %>%
#   select(country = NAME_0, adm_level_1 = NAME_1, shapeID_v5) %>%
#   arrange(country, adm_level_1)
#
# adm2_hierar_checking <- merged_final %>%
#   filter(shapeType == "ADM2") %>%
#   select(country = NAME_0, adm_level_1 = NAME_1, adm_level_2 = NAME_2,
#          shapeID_v5) %>%
#   arrange(country, adm_level_1, adm_level_2)

# write.xlsx(adm1_hierar_checking, file = "adm1_hierar_checking.xlsx")
# write.xlsx(adm2_hierar_checking, file = "adm2_hierar_checking.xlsx")



# Regions nested list -----------------------------------------------------

regions_adm0 <- merged_final %>%
  filter(shapeType == "ADM0") %>%
  arrange(NAME_0) %>%
  select(NAME_0, shapeID_v5) %>%
  pivot_wider(names_from = NAME_0, values_from = shapeID_v5) %>%
  as.list()



regions_adm1 <- list()
for (i in 1:length(unique(merged_final$NAME_0))) {
  adm1_list <- merged_final %>%
    filter(shapeType == "ADM1") %>%
    arrange(NAME_0, NAME_1) %>%
    filter(NAME_0 == sort(unique(merged_final$NAME_0))[i]) %>%
    select(NAME_1, shapeID_v5) %>%
    pivot_wider(names_from = NAME_1, values_from = shapeID_v5) %>%
    as.list()
  regions_adm1[[sort(unique(merged_final$NAME_0))[i]]] <-
    c(regions_adm1[[sort(unique(merged_final$NAME_0))[i]]], adm1_list)
}



regions_adm2 <- list()
merged_final_noNA <- merged_final %>%
  filter(shapeType == "ADM2") %>%
  drop_na(NAME_1)
for (i in 1:length(unique(merged_final_noNA$NAME_0))) {
  per_adm0 <- merged_final_noNA %>%
    # filter(shapeType == "ADM2") %>%
    arrange(NAME_0, NAME_1, NAME_2) %>%
    filter(NAME_0 == sort(unique(merged_final_noNA$NAME_0))[i])

  adm1_adm2_list <- list()
  for (j in 1:length(unique(per_adm0$NAME_1))) {
    adm2_list <- per_adm0 %>%
      filter(NAME_1 == unique(per_adm0$NAME_1)[j]) %>%
      select(NAME_2, shapeID_v5) %>%
      pivot_wider(names_from = NAME_2, values_from = shapeID_v5) %>%
      as.list()
    adm1_adm2_list[[unique(per_adm0$NAME_1)[j]]] <-
      c(adm1_adm2_list[[unique(per_adm0$NAME_1)[j]]], adm2_list)
  }

  regions_adm2[[sort(unique(merged_final_noNA$NAME_0))[i]]] <-
    c(regions_adm2[[sort(unique(merged_final_noNA$NAME_0))[i]]], adm1_adm2_list)
}



regions <- list(adm0 = regions_adm0, adm1 = regions_adm1, adm2 = regions_adm2)

# Sample usage:
# regions$adm2$`United Kingdom`$England$Leeds

format(object.size(regions), unit = "Mb")
format(object.size(regions_df), unit = "Mb")



# Export data -------------------------------------------------------------

usethis::use_data(regions, overwrite = TRUE)
usethis::use_data(regions_df, overwrite = TRUE)
