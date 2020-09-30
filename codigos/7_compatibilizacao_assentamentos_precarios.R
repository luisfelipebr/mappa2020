
library(tidyverse)
library(sf)
library(geobr)
library(lwgeom)

assentamentos_precarios <- read_sf("dados/dhgabc/AP_ABC_valid.gpkg")
area_de_ponderacao <- read_sf("dados/ibge/shapefiles/35SEE250GC_SIR_area_de_ponderacao.shp")
area_de_ponderacao <- st_transform(area_de_ponderacao, st_crs(assentamentos_precarios))

ap_comp <- st_intersects(x = area_de_ponderacao, y = assentamentos_precarios)
ap_comp2 <- area_de_ponderacao[lengths(ap_comp) > 0,]

ap_comp3 <- ap_comp2 %>%
  as_tibble(.) %>%
  select(AREA_POND)

names(ap_comp3) <- c("V0011")

write.csv(ap_comp3, "dados/ap_ap.csv", row.names = FALSE)
