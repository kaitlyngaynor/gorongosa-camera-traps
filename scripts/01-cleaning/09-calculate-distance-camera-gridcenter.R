library(here)
library(geosphere)

# I generated this file manually from the imported waypoints from the handheld GPS unit
points <- read.csv(here::here("gis", "gridcenter_vs_camera_locations.csv"))

points$distance <- NA

for (i in 1:nrow(points)) {
  points$distance[i] <- distm(c(points[i,4], points[i,3]), c(points[i,6], points[i,5]), fun = distHaversine)
}

summary(points$distance)
hist(points$distance)
sort(points$distance)

# median of 30.3 meters from center point, mean of 50.5
# max = 275.37