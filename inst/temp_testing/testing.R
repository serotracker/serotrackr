
# Testing ...

# Region data -------------------------------------------------------------

library(rgeoboundaries)
ls("package:rgeoboundaries")

adm0_gb <- gb_adm0(type = "CGAZ", quiet = FALSE) %>%
  tibble() %>%
  select(region = shapeName, GID_0 = shapeGroup, shapeID) %>%
  mutate(adm_level = 0, .after = region) %>%
  mutate(hierarchy = shapeID, .after = shapeID)

adm1_gb <- gb_adm1(type = "CGAZ", quiet = FALSE) %>%
  tibble() %>%
  select(region = shapeName, GID_0 = shapeGroup, shapeID, hierarchy = ADMHIERARC) %>%
  mutate(adm_level = 1, .after = region)


adm2_gb <- gb_adm2(type = "CGAZ", quiet = FALSE) %>%
  tibble() %>%
  select(region = shapeName, GID_0 = shapeGroup, shapeID, hierarchy = ADMHIERARC) %>%
  mutate(adm_level = 2, .after = region)

adm2_gb %>% filter(GID_0 %in% c("BRA")) %>% view()

format(object.size(adm2_gb), units = "Mb")


adm2_gadm <- geodata::gadm(country = "Brazil", level = 2, path = tempdir()
                           # version = "4.1", resolution = 2
                           )

format(object.size(adm2_gadm), units = "Kb")
