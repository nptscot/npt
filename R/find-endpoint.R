library(sf)
library(ggplot2)
library(dplyr)
library(igraph)
# group isolated parts of the road network
road_network = st_read("outputdata/orcp_city_boundary.geojson")

dist_matrix <- st_distance(road_network)
threshold <- set_units(50, "m")

connectivity_matrix <- as(dist_matrix < threshold, "lgCMatrix")
# Identify connected components using the igraph package
graph <- graph_from_adjacency_matrix(connectivity_matrix, mode = "undirected", diag = FALSE)
components <- components(graph)
road_network$component <- components$membership
unique(road_network$component)
map_view <- mapview::mapview(road_network, zcol = "component", legend = TRUE)

# Print the map
print(map_view)


component_8 <- road_network[road_network$component == 8, ] 
sf::st_write(component_8, "outputdata/component_8.geojson", delete_dsn = TRUE)

# Read the GeoJSON file
gdf <- st_read("outputdata/orcp_city_boundary.geojson") 

# filter gdf based on compoent = 9
gdf <- gdf[gdf$component == "9", ] |> 
  st_transform(crs = 27700)

plot(gdf)

merged_lines = sf::st_union(gdf)
single_line <- st_line_merge(merged_lines)
single_line <- st_set_crs(single_line, 27700)  

extract_junctions <- function(geometry) {
  # Create an empty list to store points
  junctions <- list()
  
  # Extract all start and end points
  all_points <- do.call(rbind, lapply(st_geometry(geometry), function(line) {
    coords <- st_coordinates(line)
    rbind(coords[1, c("X", "Y")], coords[nrow(coords), c("X", "Y")])
  }))
  
  # Ensure all_points is a matrix
  if (!is.matrix(all_points)) {
    all_points <- matrix(all_points, ncol = 2, byrow = TRUE)
  }
  
  # Count occurrences of each point
  point_count <- table(apply(all_points, 1, function(p) paste(p, collapse = ",")))
  
  # Identify junctions as points occurring only once
  for (point in names(point_count)) {
    if (point_count[point] == 1) {
      coords <- as.numeric(strsplit(point, ",")[[1]])
      junctions <- c(junctions, list(st_point(coords)))
    }
  }
  
  # Create an sfc object with the same CRS as the input geometry
  return(st_sfc(junctions, crs = st_crs(geometry)))
}

# Check if the result is a MULTILINESTRING to handle accordingly
if (st_geometry_type(single_line) %in% c("MULTILINESTRING", "LINESTRING")) {
  true_endpoints <- extract_junctions(single_line)
} else {
  stop("Input must be a MULTILINESTRING or LINESTRING")
}

junctions_sf <- st_sf(geometry = true_endpoints)

mapview::mapview(junctions_sf) + mapview::mapview(gdf)
##################################################

# Ensure road_network is an sf object
road_network <- st_as_sf(road_network)
# Convert all line segments on road_network into a single MultiLineString.


single_object <- st_union(road_network)
dim(single_object_sf)
# Create a new sf object for the single object
single_object_sf <- st_sf(geometry = st_sfc(single_object))
points_sf <- st_cast(road_network, "POINT")
mapview::mapview(points_sf)
# save the points to a file
st_write(road_network, "outputdata/single_object_sf.geojson")

mapview::mapview(road_network)

mapview::mapview(road_network)
endpoints <- st_cast(road_network, "POINT")
plot(endpoints)
mapview::mapview(endpoints) + mapview::mapview(road_network)
# Union geometries by component
unioned_geometries <- road_network %>%
  group_by(component) %>%
  summarize(geometry = st_union(geometry), .groups = "drop")

# Function to extract all unique endpoints from a MULTILINESTRING or LINESTRING
extract_all_endpoints <- function(geometry) {
  coords <- st_coordinates(geometry)
  if (is.null(coords) || nrow(coords) == 0) {
    return(matrix(NA, ncol = 2))
  }
  
  lines <- split(coords, coords[, "L1"])
  endpoints <- do.call(rbind, lapply(lines, function(line) {
    if (nrow(line) > 1) {
      rbind(line[1, , drop = FALSE], line[nrow(line), , drop = FALSE])
    } else {
      line[1, , drop = FALSE]
    }
  }))
  unique(endpoints)
}

# Apply the function to each unioned geometry and combine results
endpoints_list <- lapply(st_geometry(unioned_geometries), extract_all_endpoints)
endpoints_coords <- do.call(rbind, endpoints_list)

# Create an sf object for the endpoints
endpoints_sf <- st_as_sf(
  data.frame(
    x = endpoints_coords[, "X"],
    y = endpoints_coords[, "Y"],
    component = rep(unioned_geometries$component, sapply(endpoints_list, function(x) nrow(x)/2))
  ),
  coords = c("x", "y"),
  crs = st_crs(road_network)
)

# Plot the road network with endpoints
ggplot() +
  geom_sf(data = road_network, color = "gray", size = 0.5, alpha = 0.7) +  # Plot roads
  geom_sf(data = endpoints_sf, aes(color = factor(component)), size = 3, alpha = 0.8) +  # Plot endpoints
  ggtitle("Road Network with Endpoints") +
  theme_minimal() +
  labs(color = "Component") +
  theme(legend.position = "right")