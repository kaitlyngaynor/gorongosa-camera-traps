library(sf)
library(mapview)
library(units)
library(dplyr)

# Bring in the hexagons used to create original camera grid (created in ArcGIS in 2016)
hexes_orig <- st_read("gis/Camera layers/CameraGridHexes.shp")
hexes <- st_transform(hexes_orig, 4326)

# Bring in roads from Miguel (2024 roads)
roads <- st_read("gis/GNP_Roads_2024/tracks-line.shp") %>% 
  st_transform(crs = st_crs(hexes_orig))

# Bring in road 2, which is missing
road2 <- st_read("gis/Data from Marc/roads_current_october_2014.shp") %>% 
  st_transform(crs = st_crs(hexes_orig)) %>% 
  filter(IDENT == "1314") %>% 
  dplyr::select(geometry)
roads <- bind_rows(roads, road2)

# Bring in Miguel's cameras
miguel <- st_read("gis/Miguel/Deployed_camera_stations.shp") %>%
  dplyr::mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +ellps=WGS84") %>% 
  st_transform(crs = st_crs(hexes_orig))

# Bring in actual locations of Wildcam cameras, past and present
wildcam <- read.csv("data/Gaynor_camera_locations.csv", header = T) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),
           crs = "+proj=longlat +ellps=WGS84") %>% 
  st_transform(crs = st_crs(hexes_orig))

# Bring in park boundary shapefile
gnp <- st_read("gis/Data from Marc/gnp_boundary_west_straight_latlong.shp") %>% 
  st_transform(crs = st_crs(hexes_orig)) %>% 
  select("geometry") # get rid of unwanted columns

st_bbox(gnp)

mapview(wildcam, zcol = "Currently_deployed") + 
  mapview(hexes, col.regions = "black", alpha.regions = 0) + 
  mapview(roads, col.regions = "black") +
  mapview(miguel) +
  mapview(gnp, alpha.regions = 0)


# Distance between points of 5km2 grid
# side = 1.38726 = 1387 m
# dist = 2.77452 km = 2775 m


# Explore possible grid expansion -----------------------------------------

# Get the true centers of the grid
centroids <- hexes %>% 
  st_centroid() %>% 
  dplyr::mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2]
  ) %>% 
  as.data.frame() %>% 
  dplyr::select(x, y, StudySite) %>% 
  tidyr::separate_wider_position(StudySite, 
                                 widths = c("col" = 1, "row" = 2),
                                 cols_remove = FALSE)

#write.csv(centroids, "centroids.csv", row.names = F)

# Bring in my manually-created sheet
coord_builder <- read.csv("coord_builder.csv")

# Group into odd and even columns and rows
col_odd <- dplyr::filter(coord_builder, 
                         Row_Col == "Column",
                         Category == "odds") %>% 
  dplyr::select("Grid", "Coordinate") %>% 
  dplyr::rename("Longitude" = "Coordinate",
                "Column" = "Grid")
col_even <- dplyr::filter(coord_builder, 
                         Row_Col == "Column",
                         Category == "evens") %>% 
  dplyr::select("Grid", "Coordinate") %>% 
  dplyr::rename("Longitude" = "Coordinate",
                "Column" = "Grid")
row_odd <- dplyr::filter(coord_builder, 
                         Row_Col == "Row",
                         Category == "odds") %>% 
  dplyr::select("Grid", "Coordinate") %>% 
  dplyr::rename("Latitude" = "Coordinate",
                "Row" = "Grid")
row_even <- dplyr::filter(coord_builder, 
                         Row_Col == "Row",
                         Category == "evens") %>% 
  dplyr::select("Grid", "Coordinate") %>% 
  dplyr::rename("Latitude" = "Coordinate",
                "Row" = "Grid")

all_col <- dplyr::bind_rows(col_odd, col_even)
all_row <- dplyr::bind_rows(row_odd, row_even)

# Create combinations
odd_matrix <- expand.grid(col_odd$Column, row_odd$Row) %>% 
  as.data.frame()
names(odd_matrix) <- c("Column", "Row")
even_matrix <- expand.grid(col_even$Column, row_even$Row) %>% 
  as.data.frame()
names(even_matrix) <- c("Column", "Row")

# Join the coordinates
odd_matrix <- dplyr::left_join(odd_matrix, all_col)
odd_matrix <- dplyr::left_join(odd_matrix, all_row)
even_matrix <- dplyr::left_join(even_matrix, all_col)
even_matrix <- dplyr::left_join(even_matrix, all_row)

# all points
all_coords <- dplyr::bind_rows(odd_matrix, even_matrix) %>% 
  dplyr::mutate(Row = stringr::str_pad(Row, width = 2, pad = "0"),
    Site_ID = paste0(Column, Row))

# convert to sf
all_coords_sf <- st_as_sf(all_coords, 
                          coords = c("Longitude", "Latitude"),
                          crs = 4326) %>% 
  st_transform(crs = st_crs(hexes_orig))

# Compute the distance between all points and the roads
distances <- st_distance(all_coords_sf, roads)

# Convert distances to km (assuming the CRS is in meters; divide by 1000 for km)
distances_km <- distances / 1000

# Filter points that are within 1 km of the roads
points_within_1km <- all_coords_sf[apply(distances_km, 1, min) <= 1, ]

# clip to park
points_within_1km <- st_intersection(points_within_1km, gnp)

# get deployed status
wildcam_deployed <- dplyr::select(wildcam, Site_ID, Currently_deployed) %>% 
  st_drop_geometry()

# make subset of only deployed cameras
wildcam_2024_sf <- wildcam %>% 
  dplyr::filter(Currently_deployed == "yes") %>% 
  dplyr::select(Site_ID, geometry)

# Remove current cameras
new_points <- anti_join(points_within_1km, wildcam_deployed)

  mapview(roads) + 
  mapview(wildcam, zcol = "Currently_deployed") +
    mapview(new_points) 



# Densify the grid --------------------------------------------------------

# Generate Voronoi polygons constrained to the bounding box
voronoi_polygons <- st_voronoi(st_union(all_coords_sf), 
                               envelope = st_as_sfc(st_bbox(all_coords_sf))) %>%
  st_collection_extract("POLYGON")

# Extract vertices from the Voronoi polygons
dense_grid_vertices <- voronoi_polygons %>%
  st_cast("POINT") %>%  # Convert polygon boundaries to points
  st_as_sf() %>%             # Convert back to an sf object
  unique() %>%  # take only unique points
  select(x) %>% # just take geometry column
  rename("geometry" = "x") %>%  # and rename for consistency
  mutate(class = "Densification of grid")
  
# Join with centroids
all_coords_sf$class <- "Expansion of current grid"
dense_grid <- bind_rows(dense_grid_vertices, all_coords_sf) %>% 
  st_intersection(gnp) # Clip to Gorongosa

mapview(dense_grid)


# Filter to 1km of road ---------------------------------------------------

# Compute the distance between all points and the roads
distances_dense <- st_distance(dense_grid, roads)

# Filter points that are within 1 km of the roads
points_within_1km_dense <- dense_grid[apply(distances_dense, 1, min) <= 1000, ]

# Remove new points that duplicate existing cameras
distances_dense_wildcam <- st_distance(points_within_1km_dense, wildcam)
points_within_1km_dense <- points_within_1km_dense[apply(distances_dense_wildcam, 1, min) >= 500, ]

# Join existing camera locations back in
wildcam_2024_sf$class <- "Existing cameras"
camera_traps <- bind_rows(points_within_1km_dense, wildcam_2024_sf)


# Map of options, with miguel's cameras
(map_cameras <- 
  mapview(roads, 
            color = "#333333") + 
  mapview(camera_traps, 
          zcol = "class",
          col.regions = c("#55478A", "red", "#E78B4B"),
          cex = 5,
          alpha.regions = 0.75) +
  mapview(miguel, 
          col.regions = "yellow",
          cex = 5,
          alpha.regions = 0.75) +
  mapview(gnp, 
          col.regions = "black",
          alpha.regions = 0))

mapshot(map_cameras, url = "gorongosa_cameras.html")



# EXPORT COORDS FOR 2025 --------------------------------------------------

camera_traps_latlong <- camera_traps %>% 
  st_transform(4326) %>% 
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  )



# filter to lat and long limits
camera_traps_subset <- camera_traps_latlong %>% 
  dplyr::filter(longitude < 34.55 & longitude > 34.25 &
                  latitude < -18.837 & latitude > -19.015) 

mapview(camera_traps_subset, 
        zcol = "class",
        col.regions = c("#55478A", "red", "#E78B4B"),
        cex = 5,
        alpha.regions = 0.75)

write.csv(camera_traps_subset, "grid-expansion-2025/grid_prioritization.csv")

## Manually added site names to this sheet

camera_expansion <- read.csv("grid-expansion-2025/new_grid_coords_2025.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = "+proj=longlat +ellps=WGS84", remove = FALSE)

mapview(camera_expansion, 
        zcol = "class",
        col.regions = c("#55478A", "red", "#E78B4B"),
        cex = 5,
        alpha.regions = 0.75)
