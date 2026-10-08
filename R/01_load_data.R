
library(sf)

lifeguards <- st_read("data/lifeguards.geojson")
body_surfing <- st_read("data/body_surfing.geojson")
fire_stations <- st_read("data/fire_stations.geojson")
shoreline <- st_read("data/shoreline_access.geojson")
roads <- st_read("data/oahu_roads.geojson")

# Standardize coordinate systems
lifeguards <- st_transform(lifeguards, 4326)
body_surfing <- st_transform(body_surfing, 4326)
fire_stations <- st_transform(fire_stations, 4326)
shoreline <- st_transform(shoreline, 4326)
roads <- st_transform(roads, 4326)

# Display dataset summary
summary_table <- data.frame(
  Dataset = c(
    "Lifeguards",
    "Body Surfing",
    "Fire Stations",
    "Shoreline Access",
    "Roads"
  ),
  Features = c(
    nrow(lifeguards),
    nrow(body_surfing),
    nrow(fire_stations),
    nrow(shoreline),
    nrow(roads)
  )
)

print(summary_table)

