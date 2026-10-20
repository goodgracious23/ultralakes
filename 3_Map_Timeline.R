# install.packages("scales")

library(tigris)
library(sf)
library(ggrepel)
library(tidyverse)
library(terra)
library(ggspatial)
library(scales)


# Read in the Hydrography Data
# download gdb from https://www.sciencebase.gov/catalog/item/69743a66d4be0260181a1222 
# save to project in folder called 'hydrography'
gdb <- "hydrography/3dhp_all_CONUS_20260112_GDB.gdb"

# Get the waterbody layer's coordinate reference system
layers <- st_layers(gdb)
water_crs <- layers$crs[[match("hydro_3dhp_all_waterbody", layers$name)]]

# Broad rectangle around the northern Wisconsin / Michigan study area
# Convert the rectangle to the geodatabase's coordinates
study_area <- st_as_sfc(st_bbox(c(xmin = -89.8, ymin = 45.9, 
                                  xmax = -89.4, ymax = 46.3),
                                crs = st_crs(4326)))
study_area <- st_transform(study_area, water_crs)

# Read only waterbodies overlapping this region
wi_water <- st_read(gdb,
  layer = "hydro_3dhp_all_waterbody",
  wkt_filter = st_as_text(study_area)) %>%
  st_zm(wi_water)

# The study lakes
study_lakes <- tribble(
  ~lake,          ~lat,       ~lon,          ~source,        
  "Trout",        46.03482048770976, -89.66819870747723, "NTL LTER",
  "Sparkling",    46.00672765547786, -89.70038665743763, "NTL LTER",
  "Trout Bog",    46.04112988384675, -89.68624383844217, "NTL LTER",
  "Crystal Bog",  46.007556318796404, -89.60632679600607, "NTL LTER",
  "Little Rock",  45.99583376567839, -89.701261803456, "NEON",
  "Crampton",     46.21012609293135, -89.47454586148362, "NEON",
  "Peter",        46.2528434164505, -89.5037528468892,"Cascade",
  "Paul",         46.251364129960976, -89.5034713620719,"Cascade",
  "Tuesday",      46.251286271672065, -89.49739129001864, "Cascade",
  "Crystal",      46.00180678995412, -89.61318410588659,  "NTL LTER")

study_pts <- study_lakes %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(st_crs(wi_water))

study_pts_joined = st_join(study_pts, wi_water, join = st_intersects)

study_pts_joined %>%
  st_drop_geometry() %>%
  select(lake, source, everything())

study_polygons = st_filter(wi_water, study_pts, .predicate = st_intersects)

bb <- st_bbox(study_pts)
pad_x <- (bb$xmax - bb$xmin) * 0.7
pad_y <- (bb$ymax - bb$ymin) * 0.2

# Landcover Data Set
# downloaded from: https://www.mrlc.gov/viewer/
landcover <- rast("hydrography/landcover/Annual_NLCD_LndCov_2025_CU_C1V2_c0c58d6d-bf36-4fce-9563-efdf9effb4bd.tiff")

# create an sf bounding box around your desired map
map_extent <- st_as_sfc(st_bbox(c(xmin = -89.8, ymin = 45.9, 
                                  xmax = -89.3, ymax = 46.4), crs = 4326))

# transform the extent to the raster CRS
map_extent_nlcd <- st_transform(map_extent, crs(landcover))

# crop NLCD
landcover_crop <- crop(landcover, vect(map_extent_nlcd))

landcover_ll <- project(landcover_crop, "EPSG:4326",method = "near")

# Make a landcover data frame
landcover_df <- as.data.frame(landcover_ll, xy = TRUE, na.rm = TRUE) %>%
  rename(class = `Annual_NLCD_LndCov_2025_CU_C1V2_c0c58d6d-bf36-4fce-9563-efdf9effb4bd`) %>%
  mutate(landuse = case_when(class == 11 ~ "Water",
                             class %in% c(21, 22, 23, 24) ~ "Developed",
                             class %in% c(41, 42, 43) ~ "Forest",
                             class %in% c(31, 52, 71, 81, 82) ~ "Shrub / grassland",
                             class %in% c(90, 95) ~ "Wetland",
                             TRUE ~ "Other"),
         landuse = factor(landuse, levels = c("Forest", "Wetland", "Shrub / grassland", "Developed", "Water", "Other")))

label_pts <- study_pts %>%
  st_transform(4326) %>%
  mutate(x = st_coordinates(.)[,1], y = st_coordinates(.)[,2])

# ggplot() +
#   geom_sf(data = wi_water,
#           fill = "#005f73", alpha = 0.5, color = "#005f73") +
#   geom_sf(data = study_pts, size = 2, shape = 21,
#           fill = "#ee9b00", color = "black") +
#   coord_sf(xlim = c(bb$xmin - pad_x, bb$xmax + pad_x),
#            ylim = c(bb$ymin - pad_y, bb$ymax + pad_y),
#            expand = FALSE) +
#   theme_minimal()


ggplot() +
  # Land use data
  geom_raster(data = landcover_df,
    aes(x = x, y = y, fill = landuse, alpha = 0.5)) +
  scale_fill_manual(values = c(
    "Forest" = "#3d5941",
    "Wetland" = "#778868",
    "Shrub / grassland" = "#d6bd8d",
    "Developed" = "#de8a5a",
    "Water" = "#c0d6df",
    "Other" = "#eeeeee")) +
  
  # Add the lakes back on top
  geom_sf(data = wi_water, fill = "#005f73", color = NA, 
          inherit.aes = FALSE) +
  
  # Study lakes and their labels
  geom_sf(data = study_pts, fill = "#F2B701", color = "black", 
          size = 2, shape = 21, inherit.aes = FALSE) +
  geom_label_repel(data = label_pts, aes(x = x, y = y, label = lake),
    size = 2.5, fill = scales::alpha("white", 0.8),
    label.size = 0, min.segment.length = 0,
    box.padding = 0.3, point.padding = 0.2, seed = 1) +
  
  # Scale bar
  annotation_scale(location = "br", width_hint = 0.5, 
                   unit_category = "metric",
                   bar_cols = c("gray50", "white")) +
  
  #Make it pretty
  coord_sf(xlim = c(-89.80, -89.4), ylim = c(45.97, 46.27),
           expand = FALSE, crs = 4326) +
  theme_minimal() + 
  theme(legend.position = 'none', 
        axis.text.y = element_text(angle = 90, vjust = 0.5)) + 
  labs(x = "", y = "") + 
  scale_x_continuous(breaks = seq(-89.75, -89.40, by = 0.1),
                     labels = function(x) paste0(abs(x), "°W")) +
  scale_y_continuous(breaks = seq(46, 46.3, by = 0.1),
                     labels = function(x) paste0(x, "°N"))


# Timeline Figure =======================
lakes = read_csv("data/ultralakes.csv") %>%
  mutate(lake = factor(lake, levels = c("Peter", "Paul", "Tuesday", 
                                        "LittleRock", "Crampton",
                                        "CrystalBog", "TroutBog",
                                        "Crystal", "Trout", "Sparkling")),
         event = case_when(event == 'a' ~ 'ambient',
                           event == 'b' ~ 'fish addition',
                           event == 'c' ~ 'crayfish removal',
                           event == 'd' ~ 'zoop invasion',
                           event == 'e' ~ 'nutrient addition',
                           event == 'f' ~ 'dye + nutrient',
                           event == 'g' ~ 'dye addition',
                           event == 'h' ~ 'mixing'),
         event = factor(event, levels = c('fish addition',
                                          'crayfish removal',
                                          'zoop invasion',
                                          'nutrient addition',
                                          'dye + nutrient',
                                          'dye addition',
                                          'mixing', 
                                          'ambient'))) %>%
  filter(!is.na(event)) %>% rename(manipulation = event)

# Horizontal Orientation
ggplot(lakes, aes(x = year, y = lake, color = manipulation)) +
  # geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(aes(size = manipulation), shape = 19, alpha = 0.7) +
  scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 3)) + 
  scale_color_manual(values = c("#ee9b00", "#e36414","#9a031e",
                               "#6a994e", "#0a9396", "#005f73", 
                               "#5f0f40", "gray70")) +
  guides(color = guide_legend(ncol = 3, byrow = TRUE),
         size  = guide_legend(ncol = 3, byrow = TRUE)) +
  theme_minimal() + xlab("") + ylab("") + 
  theme(legend.key.height = unit(0.5, "cm"),
        legend.title = element_blank(),
        legend.key.width = unit(0.4, "cm"),
        legend.key.spacing.y = unit(0.02, "cm"),
        legend.position = "top")

# Vertical Orientation
ggplot(lakes, aes(y = year, x = lake, color = manipulation)) +
  scale_x_discrete(position = "top") +
  # geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(aes(size = manipulation), shape = 19, alpha = 0.7) +
  scale_size_manual(values = c(4, 4, 4, 4, 4, 4, 4, 3)) + 
  scale_color_manual(values = c("#ee9b00", "#e36414","#9a031e",
                                "#6a994e", "#0a9396", "#005f73", 
                                "#5f0f40", "gray70")) +
  theme_minimal() + xlab("") + ylab("") + 
  ylim(2025, 2003) +
  theme(legend.key.height = unit(0.5, "cm"),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0))
