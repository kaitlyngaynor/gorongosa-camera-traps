library(sf)
library(mapview)

hexes <- st_read("gis/Camera layers/CameraGridHexes.shp")
roads <- st_read("gis/GNP_Roads_2024/tracks-line.shp")

miguel <- st_read("gis/Miguel/Deployed_camera_stations.shp")

wildcam <- read.csv("data/Gaynor_camera_locations.csv", header = T) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = "+proj=longlat +ellps=WGS84")



miguel2 <- miguel %>%
  dplyr::mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +ellps=WGS84")
  

mapview(wildcam, zcol = "Currently_deployed") + 
  mapview(hexes, col.regions = "black", alpha.regions = 0) + 
  mapview(roads, col.regions = "black") +
  mapview(miguel2)
