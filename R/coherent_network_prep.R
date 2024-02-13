cohesive_network_prep = function(combined_network_tile, MasterMAP, zones, crs = "EPSG:27700") {
    # combined_network_tile = sf::st_read("data-raw/combined_network_tile.geojson")
    combined_network_tile  = sf::st_transform(combined_network_tile, crs)

    # Read and transform the zones to the desired coordinate reference system (CRS)
    # zones = sf::st_read("data-raw/zones.geojson") |> sf::st_transform(crs = crs)

    zones = zones |> sf::st_transform(crs = crs)
    # # Read MasterMap GeoJSON parts
    # MasterMAP_part1 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part1.geojson")
    # MasterMAP_part2 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part2.geojson")
    # MasterMAP_part3 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part3.geojson")
    # MasterMAP_part4 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part4.geojson")
    # MasterMAP_part5 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part5.geojson")
    # MasterMAP_part6 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part6.geojson")
    # MasterMAP_part7 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part7.geojson")    
    # MasterMAP_part8 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part8.geojson")
    # MasterMAP_part9 = sf::st_read("MasterMap_Output/combined_data_RoadLink_FULL_part9.geojson")
    
    # sf::st_crs(MasterMAP_part1) = crs
    # sf::st_crs(MasterMAP_part2) = crs
    # sf::st_crs(MasterMAP_part3) = crs
    # sf::st_crs(MasterMAP_part4) = crs
    # sf::st_crs(MasterMAP_part5) = crs
    # sf::st_crs(MasterMAP_part6) = crs
    # sf::st_crs(MasterMAP_part7) = crs
    # sf::st_crs(MasterMAP_part8) = crs
    # sf::st_crs(MasterMAP_part9) = crs

    # scotland_buffer = sf::st_read("data-raw/Scotland.geojson")
    # sf::st_crs(scotland_buffer) = "EPSG:27700"

    # MasterMAP_part1 = MasterMAP_part1[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part2 = MasterMAP_part2[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part3 = MasterMAP_part3[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part4 = MasterMAP_part4[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part5 = MasterMAP_part5[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part6 = MasterMAP_part6[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part7 = MasterMAP_part7[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part8 = MasterMAP_part8[sf::st_union(scotland_buffer), , op = sf::st_intersects]
    # MasterMAP_part9 = MasterMAP_part9[sf::st_union(scotland_buffer), , op = sf::st_intersects]

    # MasterMAP = rbind(MasterMAP_part1, MasterMAP_part2, MasterMAP_part3, MasterMAP_part4, MasterMAP_part5, MasterMAP_part6, MasterMAP_part7, MasterMAP_part8, MasterMAP_part9)
    # sf::st_write(MasterMAP, ("data-raw/OS_MasterMap_Scotland.geojson"))


    # Read Scotland MasterMap GeoJSON parts

    # MasterMap = sf::st_read("data-raw/OS_MasterMap_Edinburgh.geojson") |> sf::st_transform(crs = crs)
    MasterMap = MasterMap |> sf::st_transform(crs = crs)
    # MasterMap = sf::st_read("data-raw/OS_MasterMap_Edinburgh.geojson")

    # Using mutate and case_when to update averageWidth
    MasterMap = MasterMap |>
    dplyr::mutate(averageWidth = dplyr::case_when(
        !is.na(averageWidth) ~ averageWidth,                           # If averageWidth is not NA, keep it
        is.na(averageWidth) & !is.na(minimumWidth) ~ minimumWidth,     # If averageWidth is NA but minimumWidth is not, use minimumWidth
        is.na(averageWidth) & is.na(minimumWidth) ~ 0.1,               # If both are NA, assign 0.1
        TRUE ~ averageWidth                                            # Fallback to keep existing averageWidth values if any other unexpected condition occurs
    ))

    # MasterMap_zones = sf::st_intersection(MasterMap, sf::st_union(zones))
    MasterMap_zones = MasterMap[sf::st_union(zones), , op = sf::st_intersects]
    # Prepare NPT data
    NPT_zones = combined_network_tile[sf::st_union(zones), , op = sf::st_intersects]

    # Define extra_tags and assume 'zones' is already defined
    extra_tags = c("ref", "maxspeed", "highway")

    # Check if the OSM Zones file already exists
    OSM_file_path = "data-raw/OSM_zones.geojson"
    if (!file.exists(OSM_file_path)) {
    # If the file does not exist, proceed to download
    
    # Calculate the study area from zones
    study_area = sf::st_convex_hull(sf::st_union(zones))
    
    # Download OSM zones
    OSM_zones = osmextract::oe_get_network("Scotland", extra_tags = extra_tags, boundary = study_area, boundary_type = "clipsrc")
    
    # Save the OSM data as geojson
    sf::st_write(OSM_zones, OSM_file_path)
    } else {
    # If the file exists, read the OSM zones from the geojson file
    OSM_zones = sf::st_read(OSM_file_path) |> sf::st_transform(crs = crs)
    }


    # Filter the OSM data to include only primary and secondary highways, 
    # excluding those with references '4', '5', and '6'
    filtered_OSM_zones = OSM_zones |>
    dplyr::filter(highway == 'primary' | highway == 'secondary' | ref != '4' | ref != '5' | ref != '6')

    # Create a buffer of 10 units around the filtered OSM data
    buffered_filtered_OSM_zones = sf::st_buffer(filtered_OSM_zones, dist =15)

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


    NPT_OSM_MM_zones  = sf::st_join(
    NPT_OSM_zones,
    MasterMap_zones
    )

    MasterMap_NPT_OSM_MM_zonesNPT_OSM_zones$averageWidth[is.na(NPT_OSM_MM_zones$averageWidth)] = 0.1

    return(NPT_OSM_MM_zones)
}




