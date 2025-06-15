library(mapview)
library(sf)
library(dplyr)
library(ggplot2)

camera_expansion <- read.csv("grid-expansion-2025/files/new_grid_coords_2025.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +ellps=WGS84 +no_defs", remove = FALSE) 

camera_expansion_priority <- camera_expansion %>% 
  dplyr::filter(class == "Existing" | prioritize == "yes") %>% 
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
bbox <- st_bbox(camera_expansion_priority)
bbox_sf <- st_as_sfc(bbox) %>%  
  st_buffer(dist = 5000) 
roads_clipped <- st_intersection(roads, bbox_sf)



# Map of options, with miguel's cameras
(map_cameras <- 
  mapview(roads_clipped, 
          color = "#333333") + 
  mapview(camera_expansion_priority, 
          zcol = "class",
          col.regions = c("#55478A", "red", "#E78B4B"),
          cex = 5,
          alpha.regions = 0.75))

# Export map
mapshot(map_cameras, url = "grid-expansion-2025/files/gorongosa_camera_expansion_2025.html")


# Static map of expansion
camera_expansion_priority_expansiononly <- camera_expansion_priority %>% 
  dplyr::filter(class %in% c("Existing", "Expansion"))
ggplot() +
  geom_sf(data = roads_clipped, color = "lightgray", size = 0.5) +
  geom_sf(data = camera_expansion_priority_expansiononly, aes(color = class), size = 3, alpha = 0.8) +
  geom_text(
    data = camera_expansion_priority_expansiononly,
    aes(x = st_coordinates(camera_expansion_priority_expansiononly)[,1],
        y = st_coordinates(camera_expansion_priority_expansiononly)[,2],
        label = Site_ID),
    hjust = -0.1, vjust = -0.5, size = 3
  ) +
  scale_color_manual(values = c("red", "#E78B4B")) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(title = "Camera Expansion Priority 2025")
ggsave("grid-expansion-2025/files/camera-expansion-map.pdf")

# Static map of densification


# Clip just to densification area for plotting
camera_expansion_priority_densificationonly <- camera_expansion_priority %>% 
  dplyr::filter(class %in% c("Existing", "Densification")) %>% 
  dplyr::filter(longitude > 34.343 & longitude < 34.496 & latitude > -18.985)
bbox_dense <- st_bbox(camera_expansion_priority_densificationonly)
bbox_dense_sf <- st_as_sfc(bbox_dense) %>%  
  st_buffer(dist = 1000) 
roads_clipped_dense <- st_intersection(roads, bbox_dense_sf)

ggplot() +
  geom_sf(data = roads_clipped_dense, color = "lightgray", size = 0.5) +
  geom_sf(data = camera_expansion_priority_densificationonly, aes(color = class), size = 3, alpha = 0.8) +
  geom_text(
    data = camera_expansion_priority_densificationonly,
    aes(x = st_coordinates(camera_expansion_priority_densificationonly)[,1],
        y = st_coordinates(camera_expansion_priority_densificationonly)[,2],
        label = Site_ID),
    hjust = -0.1, vjust = -0.5, size = 3
  ) +
  scale_color_manual(values = c("#55478A", "red")) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  labs(title = "Camera Densification Priority 2025")
ggsave("grid-expansion-2025/files/camera-densification-map.pdf")

