## code to prepare `regions` dataset goes here

# rgeoboundaries ----------------------------------------------------------

library(tidyverse)
library(rgeoboundaries)
library(sf)


# I preferred geographic data (polygons) from geoBoundaries, instead of GADM,
# as it is getting frequent updates and their shapes are more simplified
# (lighter to run).
# geoBoundaries v5.0.0; github commit: 9de7206

adm0_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM0.zip"
adm1_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM1.zip"
adm2_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/CGAZ/geoBoundariesCGAZ_ADM2.zip"
meta_url <- "https://github.com/wmgeolab/geoBoundaries/raw/c3551f2ad79d8d3a1e5c5498592b4d80deb08062/releaseData/geoBoundariesOpen-meta.csv"

meta <- read_csv(meta_url) %>%
  select(boundaryID, boundaryName, boundaryISO, boundaryYearRepresented,
         boundaryType, boundaryCanonical, Continent,
         UNSDG_region = `UNSDG-region`, UNSDG_subregion = `UNSDG-subregion`,
         worldBankIncomeGroup, admUnitCount) %>%
  # There are two mistakes in metadata for boundaryType:
  mutate(boundaryType = case_when(boundaryID == "LBN-ADM0-11630461" ~ "ADM0",
                                  boundaryID == "AUS-ADM1-45165765" ~ "ADM1",
                                  TRUE ~ boundaryType))

# The rgeoboundaries package only is updated until version 4 of the maps, while
# their github has been updated to version 5, with more accurate regions. It
# also adds two separate lines in the adm0 sf object for disputed areas. On the
# hand version 4 has hierarchy info for each regions. Thus, I will join versions
# 4 and 5 to import as much possible as those hierarchy info into version 5 maps.
adm0_gb_v4 <- gb_adm0(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)
adm1_gb_v4 <- gb_adm1(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)
adm2_gb_v4 <- gb_adm2(type = "CGAZ", quiet = FALSE) %>% tibble() %>% select(-geometry)

download_shp <- function(url) {
  temp <- tempfile()
  temp2 <- tempfile()
  download.file(url, temp)
  unzip(zipfile = temp, exdir = temp2)
  data <- read_sf(temp2) %>% tibble() %>% select(-geometry)
  unlink(c(temp, temp2))
  return(data)
}

# SeroTracker's map does not show any data for disputed territories, although
# there is data available for some of them in Airtable.

# Contested areas are based on US Department of State definition:
# 2023-06-03
# Geoboundaries v5.0
adm0_gb_v5 <- download_shp(adm0_url) %>%
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
  left_join(adm0_gb_v4 %>% select(shapeGroup, shapeID_v4 = shapeID))

adm1_gb_v5 <- download_shp(adm1_url) %>%
  mutate(shapeName = case_when(row_number() == nrow(.) ~
                                 "Disputed territory of Western Sahara",
                               row_number() == nrow(.) - 1 ~
                                 "Other disputed territories",
                               TRUE ~ shapeName),
         shapeGroup = ifelse(row_number() == nrow(.), "ESH", shapeGroup),
         shapeType = ifelse(is.na(shapeType), "ADM1", shapeType)) %>%
  rename(shapeID_v5 = shapeID) %>%
  left_join(adm1_gb_v4 %>% rename(shapeID_v4 = shapeID, shapeISO_v4 = shapeISO),
            by = c("shapeName", "shapeGroup", "shapeType"))


# adm2_gb_v5 does not have info about adm1 while some adm2s have the same name
# in different adm1s. Thus, to join adm2_gb_v5 with adm2_gb_v4, there is no way
# other than removing all 'duplicate' adm2s from adm2_gb_v4 (which has adm1 info).
adm2_gb_v4_duplicates <- adm2_gb_v4 %>%
  group_by(shapeGroup, shapeName) %>%
  filter(n() > 1) %>%
  pull(shapeName)

adm2_gb_v5 <- download_shp(adm2_url) %>%
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
    # all with ADM2, but DOUBLE CHECK later:
    shapeType = ifelse(is.na(shapeType), "ADM2", "ADM2")
  ) %>%
  rename(shapeID_v5 = shapeID) %>%
  left_join(
    by = c("shapeName", "shapeGroup", "shapeType"),
    adm2_gb_v4 %>%
      rename(shapeID_v4 = shapeID, shapeISO_v4 = shapeISO) %>%
      filter(!(shapeName %in% adm2_gb_v4_duplicates))
  )

format(object.size(adm2_gb_v5), units = "Mb")



# Testing -----------------------------------------------------------------

# Making named lists out of region names for easy selection by users:

adm1_gb_v5_noNA <- adm1_gb_v5 %>% drop_na(shapeGroup)
adm1 <- list()
for (i in 1:length(unique(adm1_gb_v5_noNA$shapeGroup))) {
  temp <- adm1_gb_v5_noNA %>%
    filter(shapeGroup == unique(adm1_gb_v5_noNA$shapeGroup)[i]) %>%
    select(shapeName) %>%
    distinct(shapeName) %>%
    pivot_wider(names_from = shapeName, values_from = shapeName) %>%
    as.list()
  adm1[[unique(adm1_gb_v5_noNA$shapeGroup)[i]]] <- c(adm1[[unique(adm1_gb_v5_noNA$shapeGroup)[i]]],
                                                     temp)
}

adm1$CAN$`British Columbia`


# usethis::use_data(regions, overwrite = TRUE)
