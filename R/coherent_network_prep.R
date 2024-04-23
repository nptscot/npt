#' @param parameters parameters

cohesive_network_prep = function(
    combined_network_tile,
    sg_intermediatezone_bdry_2011,
    crs = "EPSG:27700",
    parameters
  ) {
  
  # Initialize an empty list to store results for each area
  cohesive_network_list = list()

  # Initialize an empty list to store zones for each area
  cohesive_zone_list = list()

  # Check if 'coherent_area' parameter is not null and has at least one area specified
  if (!is.null(parameters$coherent_area) && length(parameters$coherent_area) > 0) {
    
    # Loop through each area in 'coherent_area'
    for (area in parameters$coherent_area) {
      area_filename = gsub(" ", "_", area)
      print(paste("Preparing the network data for the", area))

      combined_network_tile = sf::st_transform(combined_network_tile, crs)

      las_scotland_2023 = sf::st_read("inputdata/las_scotland_2023.geojson") |> 
                          sf::st_transform(crs = crs) |> 
                          dplyr::filter(LAD23NM == area) |> 
                          sf::st_buffer(dist = 4000)

      zones = sg_intermediatezone_bdry_2011[sf::st_union(las_scotland_2023), , op = sf::st_within]
    
      zones$density = zones$TotPop2011 / zones$StdAreaHa

      # Read Scotland MasterMap GeoJSON parts
      MasterMap_file_name = paste0("inputdata/MasterMap_", area_filename, ".geojson")

      if(file.exists(MasterMap_file_name)) {
        MasterMap_zones = sf::st_read(MasterMap_file_name) |> 
                          sf::st_transform(crs = crs)
      } else {
        MasterMap = sf::st_read("inputdata/MasterMap_Scotland.geojson") |> sf::st_transform(crs = crs)
        # Read and process the original MasterMap file if the zones-specific file does not exist
        MasterMap = MasterMap |>
                      dplyr::mutate(averageWidth = dplyr::case_when(
                          !is.na(averageWidth) ~ averageWidth,                       # If averageWidth is not NA, keep it
                          is.na(averageWidth) & !is.na(minimumWidth) ~ minimumWidth,     # If averageWidth is NA but minimumWidth is not, use minimumWidth
                          is.na(averageWidth) & is.na(minimumWidth) ~ 0.1,             # If both are NA, assign 0.1
                          TRUE ~ averageWidth                                          # Fallback to keep existing averageWidth values if any other unexpected condition occurs
                          ))
        
        # Correcting the spatial operation for intersection
        MasterMap_zones = MasterMap[sf::st_union(zones), , op = sf::st_intersects]
        
        # Save the processed MasterMap_zones to a new file
        sf::st_write(MasterMap_zones, MasterMap_file_name)
      }

      # Prepare NPT data
      NPT_zones = combined_network_tile[sf::st_union(zones), , op = sf::st_intersects]

      # Define extra_tags and assume 'zones' is already defined
      extra_tags = c("ref", "maxspeed", "highway", "bicycle")

      # Check if the OSM Zones file already exists
      OSM_file_path = paste0("inputdata/OSM_", area, ".geojson")

      if (!file.exists(OSM_file_path)) {
      # If the file does not exist, proceed to download
      print(paste("The OSM at ", area, " does not exist, proceeding to download."))
      # Calculate the study area from zones
      study_area = sf::st_convex_hull(sf::st_union(zones))
      
      # Download OSM zones
      OSM_zones = osmextract::oe_get("Scotland", extra_tags = extra_tags, boundary = study_area, boundary_type = "clipsrc")
      
      OSM_zones = sf::st_transform(OSM_zones, crs = crs)
      # Save the OSM data as geojson
      sf::st_write(OSM_zones, OSM_file_path, delete_dsn = TRUE)

      print(paste("The OSM at ", area, " has been downloaded and saved at ", OSM_file_path))

      } else {
      # If the file exists, read the OSM zones from the geojson file
      OSM_zones = sf::st_read(OSM_file_path) |> sf::st_transform(crs = crs)
      }
    
      # filter highway = "footway" AND bicycle = "designated" in OSM
      OSM_zones = OSM_zones |> 
      dplyr::filter(!(highway == 'footway' & bicycle == "designated")) 
      
      # OSM_zones_no_bike = OSM_zones |> 
      #   dplyr::filter(bicycle == 'no')

      # OSM_zones = OSM_zones[!OSM_zones$geometry %in% OSM_zones_no_bike$geometry, ]

      # Filter the OSM data to include only primary and secondary highways, 
      filtered_OSM_zones = OSM_zones |>
      dplyr::filter(highway == 'primary' | highway == 'primary_link' | highway == 'secondary' | highway == 'secondary_link'   | highway == 'tertiary' | highway == 'tertiary_link' | highway == 'trunk' | highway == 'trunk_link')

      # Create a buffer of 10 units around the filtered OSM data
      buffered_filtered_OSM_zones = sf::st_buffer(filtered_OSM_zones, dist = 2)

      # Select features from NPT_zones that are within the buffered area of the OSM data
      NPT_filtered_OSM_zones = NPT_zones[sf::st_union(buffered_filtered_OSM_zones), , op = sf::st_within]
    

      NPT_filtered_OSM_zones = sf::st_join(NPT_filtered_OSM_zones, OSM_zones)
      NPT_filtered_OSM_zones$arterial = 'Yes'

      # Exclude the features in NPT_filtered_OSM_zones from the original NPT_zones dataset
      rest_of_NPT_zones = NPT_zones[!NPT_zones$geometry %in% NPT_filtered_OSM_zones$geometry, ]

      # Exclude the features in filtered_OSM_zones from the original OSM_zones dataset
      rest_of_OSM_zones = OSM_zones[!OSM_zones$geometry %in% filtered_OSM_zones$geometry, ]

      # Join the remaining parts of NPT and OSM datasets
      rest_of_NPT_OSM_zones = sf::st_join(rest_of_NPT_zones, rest_of_OSM_zones)

      rest_of_NPT_OSM_zones$arterial = 'No'

      # Combine the joined data with the NPT data within the OSM buffer
      NPT_OSM_zones = rbind(rest_of_NPT_OSM_zones, NPT_filtered_OSM_zones)


      NPT_OSM_MM_zones_for_area  = sf::st_join(
                                                NPT_OSM_zones,
                                                MasterMap_zones
                                                )

      NPT_OSM_MM_zones_for_area$averageWidth[is.na(NPT_OSM_MM_zones_for_area$averageWidth)] = 0.1

      # Store the result for the current area
      cohesive_network_list[[area]] = NPT_OSM_MM_zones_for_area

      # Store the zones data for the current area
      cohesive_zone_list[[area]] = zones      
      
      print(paste("Finished preparing the network data for the", area))
    }
    
    return(list(cohesive_network = cohesive_network_list, cohesive_zone = cohesive_zone_list))
    
  } else {
    print("No coherent area specified, proceeding with default settings.")
  }
}




