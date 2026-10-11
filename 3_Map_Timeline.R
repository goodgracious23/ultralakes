# install.packages("tigris")

library(tigris)
library(sf)
library(ggrepel)
library(tidyverse)

#Download County and State Boundaries
options(tigris_use_cache = TRUE)

wi_counties <- counties(state = "WI", cb = TRUE, year = 2024)
mi_counties <- counties(state = "MI", cb = TRUE, year = 2024)

counties_map <- bind_rows(wi_counties, mi_counties) %>%
  filter(NAME %in% c("Vilas", "Gogebic"))

states_map <- states(cb = TRUE, year = 2024) %>%
  filter(STUSPS %in% c("WI", "MI"))

# Read in the Hydrography Data
wi_water <- st_read("hydrography/Wisconsin_NHD/NHD_H_Wisconsin_State_GDB.gdb", layer = "NHDWaterbody")

mi_water <- st_read("hydrography/Michigan_NHD/NHD_H_Michigan_State_GDB.gdb", layer = "NHDWaterbody")

water <- bind_rows(st_transform(wi_water, 4326), st_transform(mi_water, 4326))

wi_water_crop <- wi_water %>%
  st_crop(xmin = -90.0, xmax = -89.2, ymin = 45.7, ymax = 46.4)

study_lakes <- tribble(
  ~lake,          ~lon,       ~lat,          ~source,        
  "Trout",        46.03482048770976, -89.66819870747723, "NTL LTER",
  "Sparkling",    46.00672765547786, -89.70038665743763, "NTL LTER",
  "Trout Bog",    46.04112988384675, -89.68624383844217, "NTL LTER",
  "Crystal Bog",  46.007556318796404, -89.60632679600607, "NTL LTER",
  "Little Rock",  45.99583376567839, -89.701261803456, "NEON",
  "Crampton",     46.21012609293135, -89.47454586148362, "NEON",
  "Peter",        46.2528434164505, -89.5037528468892,"Cascade",
  "Paul",         46.251364129960976, -89.5034713620719,"Cascade",
  "Tuesday",      46.251286271672065, -89.49739129001864, "Cascade",
  "Crystal",      46.00180678995412, -89.61318410588659,  "NTL LTER"
)
