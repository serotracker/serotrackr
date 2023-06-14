
library(tidyverse)

# regions_df <- data.frame(id = 1:4,
#                          adm0 = rep(c("CAN", "USA"), each = 2),
#                          adm1 = rep(c("Alberta", "California"), each = 2),
#                          adm2 = c("Calgary", "Edmonton", "Irvine", "Berkeley"))

regions_df <- data.frame(id = paste0("adm0-", 1:2),
                         adm0 = c("CAN", "USA"),
                         adm1 = NA, adm2 = NA) %>%
  bind_rows(
    data.frame(id = paste0("adm1-", 1:4),
               adm0 = rep(c("CAN", "USA"), each = 2),
               adm1 = c("Alberta", "British Columbia", "California", "Washington"),
               adm2 = NA),
    data.frame(id = paste0("adm2-", 1:8),
               adm0 = rep(c("CAN", "USA"), each = 4),
               adm1 = rep(c("Alberta", "British Columbia", "California", "Washington"),
                          each = 2),
               adm2 = c("Calgary", "Edmonton", "Vancouver", "Prince George",
                        "Irvine", "Berkeley", "Seattle", "Vancouver"))
  )

# a <- regions_df %>% group_by(adm0) %>% nest() %>% as.list()



# adm0 --------------------------------------------------------------------

regions_adm0 <- regions_df %>%
  filter(grepl("adm0", id)) %>%
  select(adm0, id) %>%
  pivot_wider(names_from = adm0, values_from = id) %>%
  as.list()

regions_adm0$CAN


# adm1 --------------------------------------------------------------------

regions_adm1 <- list()
for (i in 1:length(unique(regions_df$adm0))) {
  temp <- regions_df %>%
    filter(grepl("adm1", id)) %>%
    filter(adm0 == unique(regions_df$adm0)[i]) %>%
    select(adm1, id) %>%
    pivot_wider(names_from = adm1, values_from = id) %>%
    as.list()
  regions_adm1[[unique(regions_df$adm0)[i]]] <-
    c(regions_adm1[[unique(regions_df$adm0)[i]]], temp)
}

regions_adm1$CAN$Alberta
# regions_adm1$CAN.id


# adm2 --------------------------------------------------------------------

regions_adm2 <- list()
for (i in 1:length(unique(regions_df$adm0))) {
  per_adm0 <- regions_df %>%
    filter(grepl("adm2", id)) %>%
    filter(adm0 == unique(regions_df$adm0)[i])

  adm2_list <- list()
  for (j in 1:length(unique(per_adm0$adm1))) {
    temp_adm2 <- per_adm0 %>%
      filter(adm1 == unique(per_adm0$adm1)[j]) %>%
      select(adm2, id) %>%
      pivot_wider(names_from = adm2, values_from = id) %>%
      as.list()
    adm2_list[[unique(per_adm0$adm1)[j]]] <-
      c(adm2_list[[unique(per_adm0$adm1)[j]]], temp_adm2)
  }

  regions_adm2[[unique(regions_df$adm0)[i]]] <-
    c(regions_adm2[[unique(regions_df$adm0)[i]]], adm2_list)
}

regions_adm2$CAN$Alberta$Calgary


# S4 ----------------------------------------------------------------------

setClass("Locations", representation(id = "character", adm0 = "list",
                                     adm1 = "list", adm2 = "list"))
regions <- new("Locations", id = "adm0-1", adm0 = regions_adm0,
               adm1 = regions_adm1, adm2 = regions_adm2)

regions <- list(adm0 = regions_adm0, adm1 = regions_adm1, adm2 = regions_adm2)

regions$adm2$USA$California$Irvine
