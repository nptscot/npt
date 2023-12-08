
The first stage is to download the combined_network_tile data, from the
https://github.com/nptscot/outputdata/releases/tag/v2023-12-06 release.

We can do this with the `gh` CLI tool, by downloading everything
matching the `combined_network_tile` pattern in the nptscot/outputdata
repository:

``` bash
gh release download v2023-12-06 --pattern combined_network_tile.zip --repo nptscot/outputdata
```

Unzip the file with R as follows:

``` r
if (! file.exists("outputdata/combined_network_tile.geojson")) {
  unzip("combined_network_tile.zip")
  list.files("outputdata", pattern = "geojson")
}
```

``` r
parameters = jsonlite::read_json("parameters.json", simplifyVector = T)
rnet_y = geojsonsf::geojson_sf("outputdata/combined_network_tile.geojson")
names(rnet_y)
```

     [1] "utility_fastest_bicycle_go_dutch"    "utility_fastest_bicycle"            
     [3] "geometry"                            "secondary_quietest_bicycle_ebike"   
     [5] "secondary_fastest_bicycle_ebike"     "primary_quietest_bicycle_go_dutch"  
     [7] "primary_quietest_bicycle_ebike"      "commute_quietest_bicycle_go_dutch"  
     [9] "primary_quietest_bicycle"            "utility_quietest_bicycle_ebike"     
    [11] "primary_fastest_bicycle_go_dutch"    "secondary_fastest_bicycle_go_dutch" 
    [13] "secondary_quietest_bicycle"          "utility_fastest_bicycle_ebike"      
    [15] "commute_fastest_bicycle_go_dutch"    "primary_fastest_bicycle_ebike"      
    [17] "all_fastest_bicycle"                 "all_fastest_bicycle_ebike"          
    [19] "all_fastest_bicycle_go_dutch"        "commute_fastest_bicycle"            
    [21] "all_quietest_bicycle"                "commute_quietest_bicycle"           
    [23] "utility_quietest_bicycle"            "secondary_quietest_bicycle_go_dutch"
    [25] "all_quietest_bicycle_ebike"          "all_quietest_bicycle_go_dutch"      
    [27] "commute_fastest_bicycle_ebike"       "primary_fastest_bicycle"            
    [29] "secondary_fastest_bicycle"           "commute_quietest_bicycle_ebike"     
    [31] "utility_quietest_bicycle_go_dutch"   "Gradient"                           
    [33] "Quietness"                          

``` r
rnet_y = rnet_y |>
  slice_max(order_by = commute_fastest_bicycle_go_dutch, n = 10000)
```

``` r
if (parameters$open_data_build) {
    rnet_x = sf::read_sf("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_x_ed.geojson")
    rnet_x_buffers = sf::st_buffer(rnet_x, dist = 20, endCapStyle = "FLAT")
    single_rnet_x_buffer = sf::st_union(rnet_x_buffers)
    rnet_x_buffer = sf::st_sf(geometry = single_rnet_x_buffer)
    rnet_x_buffer = sf::st_make_valid(rnet_x_buffer)
  } else {
    # URL for the original route network
    url_rnet_x = "https://github.com/nptscot/networkmerge/releases/download/v0.1/OS_Scotland_Network.geojson"
    f_rnet_x = basename(url_rnet_x)
    if (!file.exists(f_rnet_x)) {
      download.file(url_rnet_x, f_rnet_x, method = "libcurl")
    }
    if (file.exists(f_rnet_x) && file.size(f_rnet_x) > 0) {
      rnet_x = geojsonsf::geojson_sf(f_rnet_x)
    } else {
      stop("File download failed or file is empty for rnet_x")
    }
    
  }
  
  # Transform the spatial data to a different coordinate reference system (EPSG:27700)
  # TODO: uncomment:
  rnet_xp = sf::st_transform(rnet_x, "EPSG:27700")
  rnet_yp = sf::st_transform(rnet_y, "EPSG:27700")
  
  # Extract column names from the rnet_yp
  name_list = names(rnet_yp)
  
  # Initialize an empty list
  funs = list()
  
  # Loop through each name and assign it a function based on specific conditions
  for (name in name_list) {
    if (name == "geometry") {
      next  # Skip the current iteration
    } else if (name %in% c("Gradient", "Quietness")) {
      funs[[name]] = mean
    } else {
      funs[[name]] = sum
    }
  }
  
  # Merge the spatial objects rnet_xp and rnet_yp based on specified parameters
  dist = 20
  angle = 10
  rnet_merged_all = stplanr::rnet_merge(rnet_xp, rnet_yp, dist = dist, segment_length = 10, funs = funs, max_angle_diff = 20)  #
```

    Warning: st_centroid assumes attributes are constant over geometries

    Joining with `by = join_by(identifier)`

``` r
  sum(rnet_merged_all[[3]], na.rm = TRUE)
```

    [1] 316474.7

``` r
  # Remove specific columns from the merged spatial object
  rnet_merged_all = rnet_merged_all[ , !(names(rnet_merged_all) %in% c('identifier','length_x'))]
  
  # Remove Z and M dimensions (if they exist) and set geometry precision
  has_Z_or_M = any(sf::st_dimension(rnet_merged_all) %in% c("XYZ", "XYM", "XYZM"))
  
  # If Z or M dimensions exist, remove them and print a message
  if (has_Z_or_M) {
    rnet_merged_all = sf::st_zm(rnet_merged_all, what = "ZM")
    cat("Z or M dimensions have been removed from rnet_merged_all.\n")
  }
  
  # Set the precision of geometries in 'rnet_merged_all' to 1e3 (0.001)
  rnet_merged_all$geometry = sf::st_set_precision(rnet_merged_all$geometry, 1e3)
  
  # Round all numeric columns in 'rnet_merged_all' to 0 decimal places
  rnet_merged_all = rnet_merged_all %>%
    mutate(across(where(is.numeric), ~ round(.x, 0)))
  
  # Prepare a list of columns to check for NA, excluding 'geometry'
  rnet_yp_list = as.list(names(rnet_yp))
  columns_to_check = unlist(rnet_yp_list[rnet_yp_list != "geometry"])
  
  # Filter out rows in 'rnet_merged_all' where all columns are NA
  rnet_merged_all = rnet_merged_all %>%
    dplyr::filter_at(columns_to_check, any_vars(!is.na(.))) %>%
    # Additional step to sample top 10000 rows for testing:
    slice_max(order_by = commute_fastest_bicycle_go_dutch, n = 10000)
  
  # Selecting only the geometry column from the 'rnet_merged_all' dataset.
  rnet_merged_all_only_geometry = rnet_merged_all %>% dplyr::select(geometry)
  
  # Merging all geometries into a single geometry using st_union from the sf package.
  rnet_merged_all_union = sf::st_union(rnet_merged_all_only_geometry)
  
  # Transforming the merged geometry to a specific coordinate reference system (CRS), EPSG:27700.
  rnet_merged_all_projected = sf::st_transform(rnet_merged_all_union, "EPSG:27700")
  
  # Converting the projected geometry into a GEOS geometry. GEOS is a library used for spatial operations.
  rnet_merged_all_geos = geos::as_geos_geometry(rnet_merged_all_projected)
  
  # Creating a buffer around the GEOS geometry. This expands the geometry by a specified distance (in meters).
  rnet_merged_all_geos_buffer = geos::geos_buffer(rnet_merged_all_geos, distance = 30, params = geos::geos_buffer_params(quad_segs = 4))
  
  # Converting the buffered GEOS geometry back to an sf object.
  rnet_merged_all_projected_buffer = sf::st_as_sf(rnet_merged_all_geos_buffer)
  
  # Confirming buffered geometry CRS as EPSG:27700.
  rnet_merged_all_buffer = sf::st_transform(rnet_merged_all_projected_buffer, "EPSG:4326")
  
  # Subsetting another dataset 'rnet_y' based on the spatial relation with 'rnet_merged_all_buffer'.
  # It selects features from 'rnet_y' that are within the boundaries of 'rnet_merged_all_buffer'.
  rnet_y_subset = rnet_y[rnet_merged_all_buffer, , op = sf::st_within]
  
  # Filter 'rnet_y' to exclude geometries within 'within_join'
  rnet_y_rest = rnet_y[!rnet_y$geometry %in% rnet_y_subset$geometry, ]
  
  # Transform the CRS of the 'rnet_merged_all' object to WGS 84 (EPSG:4326)
  rnet_merged_all = sf::st_transform(rnet_merged_all, "EPSG:4326")
  
  # Combine 'rnet_y_rest' and 'rnet_merged_all' into a single dataset
  simplified_network = dplyr::bind_rows(rnet_y_rest, rnet_merged_all)
  
  # Remove specified columns and replace NA values with 0 in the remaining columns
  items_to_remove = c('geometry', 'length_x_original', 'length_x_cropped')
  
  cols_to_convert = names(simplified_network)[!names(simplified_network) %in% items_to_remove]
  for (col in cols_to_convert) {
    simplified_network[[col]][is.na(simplified_network[[col]])] = 0
  }
  
  simplified_network
```

    Simple feature collection with 6521 features and 32 fields
    Geometry type: GEOMETRY
    Dimension:     XY
    Bounding box:  xmin: -4.806069 ymin: 55.06126 xmax: -2.078184 ymax: 57.65532
    Geodetic CRS:  WGS 84
    First 10 features:
       utility_fastest_bicycle_go_dutch utility_fastest_bicycle
    1                              3445                      98
    2                              6132                     169
    3                              6132                     169
    4                              6054                     167
    5                              6038                     166
    6                              3822                     111
    7                              4964                     133
    8                              4964                     133
    9                              4964                     133
    10                             4627                     125
       secondary_quietest_bicycle_ebike secondary_fastest_bicycle_ebike
    1                                62                              13
    2                                16                             112
    3                                16                             112
    4                                15                             111
    5                                15                             111
    6                                30                               1
    7                                15                             111
    8                                15                             111
    9                                15                             111
    10                               15                             106
       primary_quietest_bicycle_go_dutch primary_quietest_bicycle_ebike
    1                                 40                             26
    2                                  9                              5
    3                                  9                              5
    4                                 10                              6
    5                                 10                              6
    6                                 53                             31
    7                                 11                              6
    8                                 11                              6
    9                                 11                              6
    10                                11                              6
       commute_quietest_bicycle_go_dutch primary_quietest_bicycle
    1                               4618                        1
    2                                892                        0
    3                                890                        0
    4                                834                        0
    5                                827                        0
    6                               4766                        1
    7                                825                        0
    8                                824                        0
    9                                823                        0
    10                               780                        0
       utility_quietest_bicycle_ebike primary_fastest_bicycle_go_dutch
    1                            6511                               16
    2                            3093                               36
    3                            3093                               36
    4                            2866                               37
    5                            2846                               37
    6                            9447                               22
    7                            1841                               32
    8                            1841                               32
    9                            1841                               32
    10                           1488                               32
       secondary_fastest_bicycle_go_dutch secondary_quietest_bicycle
    1                                  54                          0
    2                                 201                          3
    3                                 201                          3
    4                                 199                          3
    5                                 199                          3
    6                                  21                          1
    7                                 190                          3
    8                                 190                          3
    9                                 190                          3
    10                                190                          3
       utility_fastest_bicycle_ebike commute_fastest_bicycle_go_dutch
    1                           3446                             3412
    2                           6383                             3152
    3                           6383                             3149
    4                           6351                             3133
    5                           6332                             3127
    6                           2205                             3090
    7                           5224                             3024
    8                           5224                             3023
    9                           5224                             3021
    10                          4974                             2712
       primary_fastest_bicycle_ebike all_fastest_bicycle all_fastest_bicycle_ebike
    1                             11                 333                      6790
    2                             30                 418                     10122
    3                             30                 418                     10118
    4                             30                 417                     10098
    5                             30                 414                     10072
    6                              2                 378                      3734
    7                             29                 376                      8897
    8                             29                 376                      8895
    9                             29                 376                      8893
    10                            16                 340                      8369
       all_fastest_bicycle_go_dutch commute_fastest_bicycle all_quietest_bicycle
    1                          6928                     234                  643
    2                          9521                     242                  157
    3                          9518                     242                  157
    4                          9423                     243                  149
    5                          9401                     241                  146
    6                          6955                     266                  651
    7                          8210                     236                  121
    8                          8209                     236                  121
    9                          8207                     236                  121
    10                         7562                     208                  106
       commute_quietest_bicycle utility_quietest_bicycle
    1                       474                      169
    2                        77                       77
    3                        77                       77
    4                        74                       71
    5                        72                       71
    6                       404                      245
    7                        72                       45
    8                        72                       45
    9                        72                       45
    10                       66                       37
       secondary_quietest_bicycle_go_dutch all_quietest_bicycle_ebike
    1                                  112                      12911
    2                                   26                       4209
    3                                   26                       4206
    4                                   24                       3916
    5                                   24                       3889
    6                                   44                      15809
    7                                   24                       2882
    8                                   24                       2881
    9                                   24                       2879
    10                                  24                       2471
       all_quietest_bicycle_go_dutch commute_fastest_bicycle_ebike
    1                          10431                          3320
    2                           3675                          3597
    3                           3673                          3593
    4                           3418                          3605
    5                           3395                          3598
    6                          12970                          1527
    7                           2526                          3532
    8                           2524                          3531
    9                           2523                          3529
    10                          2154                          3272
       primary_fastest_bicycle secondary_fastest_bicycle
    1                        1                         0
    2                        0                         7
    3                        0                         7
    4                        0                         7
    5                        0                         7
    6                        0                         1
    7                        0                         7
    8                        0                         7
    9                        0                         7
    10                       0                         7
       commute_quietest_bicycle_ebike utility_quietest_bicycle_go_dutch Gradient
    1                            6312                              5662        3
    2                            1096                              2747        2
    3                            1093                              2747        2
    4                            1030                              2549        2
    5                            1023                              2534        2
    6                            6301                              8106        3
    7                            1020                              1665        2
    8                            1018                              1665        2
    9                            1016                              1665        2
    10                            962                              1338        2
       Quietness                       geometry
    1        100 LINESTRING (-4.26391 55.855...
    2         60 LINESTRING (-2.1132 57.1411...
    3         60 LINESTRING (-2.11311 57.141...
    4         60 LINESTRING (-2.11326 57.140...
    5         60 LINESTRING (-2.1134 57.1406...
    6        100 LINESTRING (-4.26028 55.865...
    7         60 LINESTRING (-2.11357 57.140...
    8         60 LINESTRING (-2.11353 57.140...
    9         60 LINESTRING (-2.11351 57.140...
    10        20 LINESTRING (-2.11362 57.139...
