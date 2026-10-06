library(mapview)
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyterra)

camera_expansion <- read.csv("grid-expansion-2025/files/new_grid_coords_2025.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +ellps=WGS84 +no_defs", remove = FALSE)  %>% 
  dplyr::filter(class == "Existing" | prioritize == "yes") %>% 
  dplyr::filter(class %in% c("Existing", "Expansion")) %>% 
  dplyr::select(Site_ID, class, longitude, latitude, floodplain_maybe) 

# Bring in roads from Miguel (2024 roads)
roads <- st_read("gis/GNP_Roads_2024/tracks-line.shp") %>% 
  st_transform(crs = st_crs(camera_expansion))
# Bring in road 2, which is missing
road2 <- st_read("gis/Data from Marc/roads_current_october_2014.shp") %>% 
  st_transform(crs = st_crs(camera_expansion)) %>% 
  filter(IDENT == "1314") %>% 
  dplyr::select(geometry)
roads <- bind_rows(roads, road2)

# Clip roads
bbox <- st_bbox(camera_expansion)
bbox_sf <- st_as_sfc(bbox) %>%  
  st_buffer(dist = 5000) 
roads_clipped <- st_intersection(roads, bbox_sf)


# Bring in wild dogs
dogs <- rast("gis/combined_group_heatmap_kde.tif")
dogs_bbox <- st_bbox(dogs)

# Map
ggplot() +
  geom_spatraster(data = dogs) +
  geom_sf(data = roads_clipped, color = "lightgray", size = 0.5) +
  geom_sf(data = camera_expansion, size = 3, alpha = 0.8,
          shape = 21, fill = "black", color = "white") +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent", name = "Wild dog activity") +
  coord_sf(xlim = c(dogs_bbox["xmin"], dogs_bbox["xmax"]),
           ylim = c(dogs_bbox["ymin"], dogs_bbox["ymax"]),
           expand = FALSE,
           crs = st_crs(dogs)) +
  theme_void() +
  theme(legend.position = "top")

