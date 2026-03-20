# Make map for DSI poster

library(dplyr)
library(sf)
library(ggplot2)

# Make list of holdout cameras
holdout <- c("D05", "D03", "G02", "E02", "E06", "F05", "I10", "I04", "I08", "D07", "B05", "G08")

# Bring in locations of Wildcam cameras
wildcam <- read.csv("data/Gaynor_camera_locations.csv", header = T) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = "+proj=longlat +ellps=WGS84") %>%
  mutate(dataset_split = if_else(Site_ID %in% holdout,
                                 "Holdout",
                                 "Training/Evaluation"))

# Bring in park boundary shapefile
gnp <- st_read("gis/Data from Marc/gnp_boundary_west_straight_latlong.shp") %>% 
  st_transform(crs = st_crs(wildcam))

# Bring in lake Urema shapefile
urema <- st_read("gis/Data from Marc/lake_urema_latlong.shp") %>% 
  st_transform(crs = st_crs(wildcam))

# Bring in roads from Miguel (2024 roads)
roads <- st_read("gis/GNP_Roads_2024/tracks-line.shp") %>% 
  st_transform(crs = st_crs(wildcam))

# Create bounding box limits
bbox <- st_bbox(wildcam)
xpad <- (bbox$xmax - bbox$xmin) * 0.05
ypad <- (bbox$ymax - bbox$ymin) * 0.05

# Make a map
map <- ggplot() +
  geom_sf(data = gnp, fill = "#F8F8F8") +
  geom_sf(data = urema, fill = "blue") +
  geom_sf(data = roads, color = "#909090", size = 0.25) +
  geom_sf(data = wildcam, aes(fill = dataset_split, shape = dataset_split), size = 3) +
  coord_sf(xlim = c(bbox$xmin - xpad, bbox$xmax + xpad),
           ylim = c(bbox$ymin - ypad, bbox$ymax + ypad)) +  
  scale_fill_manual(values = c(
    "Holdout" = "#9a5fab",
    "Training/Evaluation" = "#ff8300"
  )) +
  scale_shape_manual(values = c(
    "Holdout" = 24,             
    "Training/Evaluation" = 21  
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(fill = "Camera Locations",
       shape = "Camera Locations") 

ggsave(map, filename = "figures/camera_map_dsi_poster.png", width = 8, height = 4, dpi = 300)
