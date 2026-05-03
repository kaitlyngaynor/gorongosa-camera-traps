# Make map for DSI poster

library(dplyr)
library(sf)
library(ggplot2)

# Bring in locations of Wildcam cameras
wildcam <- read.csv("data/camera_habitats_for_maria.csv", header = T) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = "+proj=longlat +ellps=WGS84") 

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
(map <- ggplot() +
  geom_sf(data = gnp, fill = "#F8F8F8") +
  geom_sf(data = urema, fill = "blue") +
  geom_sf(data = roads, color = "#909090", size = 0.25) +
  geom_sf(data = wildcam, aes(col = Habitat_class, shape = floodsavanna), size = 3) +
  coord_sf(xlim = c(bbox$xmin - xpad, bbox$xmax + xpad),
           ylim = c(bbox$ymin - ypad, bbox$ymax + ypad)) +  
  scale_shape_manual(values = c(
    "floodplain" = 17,             
    "savanna" = 16  
  )) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(col = "Rough Habitat Classification",
       shape = "MS Floodplain Designation"))

ggsave(map, filename = "figures/camera_map_habitat_maria.png", width = 8, height = 4, dpi = 300)
