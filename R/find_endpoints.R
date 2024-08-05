find_component= function(rnet, threshold = 50) {

  sf::st_crs(rnet) = 27700

  # Calculate the distance matrix between features
  dist_matrix = sf::st_distance(rnet)

  # Convert the threshold to units of meters
  threshold = units::set_units(threshold, "m")

  # Create a connectivity matrix where connections are based on the threshold distance
  connectivity_matrix = Matrix::Matrix(dist_matrix < threshold, sparse = TRUE)

  # Create an undirected graph from the adjacency matrix
  graph = igraph::graph_from_adjacency_matrix(connectivity_matrix, mode = "undirected", diag = FALSE)

  # Find the connected components in the graph
  components = igraph::components(graph)

  # Assign component membership to the road network
  rnet$component = components$membership

  # Return the updated road network with component membership
  return(rnet)
}

find_endpoints = function(rnet) {
  # Remove duplicate lines in rnet
  merged_lines = sf::st_union(rnet)
  
  # Cast to LINESTRING
  merged_lines = sf::st_cast(merged_lines, "LINESTRING")

  # Create an empty list to store points
  junctions = list()
  
  # Extract the first and last coordinates of each line
  all_points = do.call(rbind,
    lapply(sf::st_geometry(merged_lines), function(line) {
      coords = sf::st_coordinates(line)
      rbind(coords[1, , drop = FALSE], coords[nrow(coords), , drop = FALSE])
    })
  )
  
  if (is.matrix(all_points)) {
    all_points = as.data.frame(all_points)
  }
  
  all_points = all_points[, 1:2]
  names(all_points) = c("X", "Y")

  # Count occurrences of each point
  points_counted = all_points %>%
    dplyr::group_by(X, Y) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = 'drop')
  
  # Filter for unique points (occurring only once)
  unique_points = points_counted %>%
    dplyr::filter(Count == 1)
  
  # Convert to sf object
  points_sf = sf::st_as_sf(unique_points, coords = c("X", "Y"), crs = sf::st_crs(rnet))
  
  return(points_sf)
}

# Example usage:
points_sf = find_endpoints(gdf)

library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
# Define the URL
url = "https://github.com/nptscot/npt/releases/download/join_orcp/orcp_city_boundary.geojson"

# Download the file from the URL
gdf_all = read_sf(url)

url = "https://github.com/nptscot/npt/releases/download/join_orcp/OS_Edin.geojson"
OS_inter = read_sf(url)

url = "https://github.com/nptscot/npt/releases/download/join_orcp/OSM_Edin.geojson"
OSM_inter = sf::read_sf(url)

find_nearest_points = function(points_sf, rnet, segment_length = 10, dist = 100) {
    rnet = stplanr::line_cast(rnet)
    rnet = stplanr::line_segment(rnet, segment_length = segment_length)   
    rnet_points_df = as.data.frame(sf::st_coordinates(rnet))
    names(rnet_points_df) = c("X", "Y", "L1")
    rnet_points_sf = st_as_sf(rnet_points_df, coords = c("X", "Y"), crs = st_crs(rnet))
    points_sf_buffer = st_buffer(points_sf, dist = dist)
    rnet_points_sf_within_buffer = sf::st_intersection(rnet_points_sf, points_sf_buffer)
    # Find the nearest point in os_points_sf_within_buffer for each point in points_sf
    nearest_points = nngeo::st_nn(points_sf, rnet_points_sf_within_buffer, k = 1, returnDist = TRUE)

    # Extract the nearest points and distances
    nearest_indices = unlist(nearest_points$nn)
    nearest_distances = unlist(nearest_points$dist)

    # Ensure nearest_indices are within the range of points_sf
    if (all(nearest_indices <= nrow(rnet_points_sf_within_buffer))) {
    # Add the index of points_sf to the nearest_os_points
    nearest_os_points = rnet_points_sf_within_buffer[nearest_indices, ]
    nearest_os_points$index_points_sf = 1:length(nearest_indices)
    } else {
    stop("The nearest_indices are out of bounds. Please check your data.")
    }
    return(nearest_os_points)
}


os_points = find_nearest_points(points_sf, OS_inter)
osm_points = find_nearest_points(os_points, OSM_inter)

mapview::mapview(os_points , col.regions = "red") +
  mapview::mapview(points_sf, col.regions = "blue") +
  mapview::mapview(osm_points, col.regions = "green") +
  mapview::mapview(OSM, col.regions = "black")

target_zone = zonebuilder::zb_zone("Edinburgh", n_circles = 4) |>
               sf::st_transform(crs = "EPSG:27700")
OSM = OSM_inter[sf::st_union(target_zone), , op = sf::st_intersects]              

calculate_paths_from_point_dist <- function(network, point, target_point, path_type = "shortest") {
    point_geom <- sf::st_as_sfc(point)
    target_geom <- sf::st_as_sfc(target_point)
    start_node <- st_nearest_feature(point_geom, network)
    end_node <- st_nearest_feature(target_geom, network)
    # Calculate the shortest path using sfnetworks
    network = network |>
        sf::st_cast("LINESTRING") |>
        sfnetworks::as_sfnetwork(directed = FALSE) |>
        sfnetworks::activate("edges") |>
        mutate(weight = st_length(geometry))

    paths <- sfnetworks::st_network_paths(
        network, 
        from = start_node, 
        to = end_node, 
        weights = NULL,
        type = "shortest"
    )
    
    # Extract the edge paths
    edges_in_paths <- paths %>%
        dplyr::pull(edge_paths) %>%
        unlist() %>%
        unique()
    
    result <- network %>%
        dplyr::slice(edges_in_paths) %>%
        sf::st_as_sf()
    
    return(result)
}

all_paths <- purrr::map_dfr(
    seq_len(nrow(points_sf)),
    function(i) {
        start_point <- points_sf[i, ]
        end_point <- osm_points |> filter(index_points_sf == i )
        calculate_paths_from_point_dist(OSM_split, start_point, end_point, path_type = "shortest")
    }
)


OS_inter = stplanr::line_cast(OS_inter)
OS_split = stplanr::line_segment(OS_inter, segment_length = 10)

OSM_inter = stplanr::line_cast(OSM_inter)
OSM_split = stplanr::line_segment(OSM_inter, segment_length = 10)


os_points_df = as.data.frame(sf::st_coordinates(OS_split))
os_points_df = as.data.frame(os_points_df)
names(os_points_df) = c("X", "Y", "L1")
os_points_sf = st_as_sf(os_points_df, coords = c("X", "Y"), crs = st_crs(gdf))

points_sf_buffer = st_buffer(points_sf, dist = 100)

os_points_sf_within_buffer = sf::st_intersection(os_points_sf, points_sf_buffer)

# Find the nearest point in os_points_sf_within_buffer for each point in points_sf
nearest_points = nngeo::st_nn(points_sf, os_points_sf_within_buffer, k = 1, returnDist = TRUE)

# Extract the nearest points and distances
nearest_indices = unlist(nearest_points$nn)
nearest_distances = unlist(nearest_points$dist)

# Ensure nearest_indices are within the range of points_sf
if (all(nearest_indices <= nrow(os_points_sf_within_buffer))) {
  # Add the index of points_sf to the nearest_os_points
  nearest_os_points = os_points_sf_within_buffer[nearest_indices, ]
  nearest_os_points$index_points_sf = 1:length(nearest_indices)
} else {
  stop("The nearest_indices are out of bounds. Please check your data.")
}

# Combine the results into a data frame
results = data.frame(
  id_points_sf = 1:nrow(points_sf),  # Assuming IDs are row numbers in points_sf
  nearest_id_os_points = os_points_sf_within_buffer$L1[nearest_indices],
  distance = nearest_distances
)

# Create interactive map using mapview
i = 5
mapview::mapview(nearest_os_points |> filter(index_points_sf == i ), col.regions = "red") +
  mapview::mapview(points_sf[i, ], col.regions = "blue") +
  mapview::mapview(gdf, col.regions = "black")

