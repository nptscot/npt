tar_load(parameters)
tar_load(os_pois)
library(tmap)
tmap_mode("view")
library(tidyverse)

# Exploring OS pois - we could easily include a wider range of attractions

# unique(os_pois$groupname)
# # [1] "Attractions"                        "Public Infrastructure"              "Education and Health"              
# # [4] "Sport and Entertainment"            "Commercial Services"                "Accommodation, Eating and Drinking"
# # [7] "Transport"                          "Manufacturing and Production"       "Retail"                            
# # [10] "OA Centroid"

# os_attraction = os_pois |>
#   filter(groupname == "Attractions")
# 
# unique(os_attraction$classname)
# # [1] "Archaeological Sites"                                   "Historic and Ceremonial Structures"                    
# # [3] "Designated Scenic Features"                             "Historic Buildings Including Castles, Forts and Abbeys"
# # [5] "Playgrounds"                                            "Trigonometric Points"                                  
# # [7] "Battlefields"                                           "Railways (Heritage, Steam and Miniature)"              
# # [9] "Siteseeing, Tours, Viewing and Visitor Centres"         "Art Galleries"                                         
# # [11] "Museums"                                                "Historical Ships"                                      
# # [13] "Theme and Adventure Parks"                              "Bird Reserves, Collections and Sanctuaries"            
# # [15] "Horticultural Attractions"                              "Aquaria and Sea Life Centres"                          
# # [17] "Laseria, Observatories and Planetaria"                  "Information Centres"                                   
# # [19] "Country and National Parks"                             "Lochs and Lochans"                                     
# # [21] "Reservoirs"                                             "Zoos and Animal Collections"                           
# # [23] "Lakes and Waters"                                       "Commons"                                               
# # [25] "Picnic Areas"                                           "Farm Based Attractions"                                
# # [27] "Unspecified and Other Attractions" 
# 
# unique(os_attraction$categoryname)
# # [1] "Historical and Cultural"  "Landscape Features"       "Recreational"             "Tourism"                 
# # [5] "Botanical and Zoological" "Bodies Of Water" 
# 
# tm_shape(os_attraction) + tm_dots()


# Get Recreational POIs (includes playgrounds, country and national parks, commons, and picnic areas)

os_recreation = os_pois |> 
  filter(categoryname == "Recreational")

# unique(os_recreation$classname)
# [1] "Playgrounds"                "Country and National Parks" "Commons"                    "Picnic Areas" 
# unique(os_recreation$name)
# [1] "Playground"                             "Almondell & Calderwood Country Park"    "Transgression Skate Park"              
# [4] "Adventure Playground"                   "Skateboard Park"                        "Skatepark"                             
# [7] "Play Area"                              "Yard Playground"                        "Kirkhill Common"                       
# [10] "The Common"                             "Picnic Area"                            "Gracemount Play Area"                  
# [13] "Play Ground"                            "Almondell and Calder Wood Country Park" "Townhill Country Park"                 
# [16] "Country Park"                           "Beecraigs Country Park"                 "Bonaly Country Park"                   
# [19] "Muiravonside Country Park"              "Gravity Trampoline Park"   

tm_shape(os_recreation) + tm_dots()

# Get parks from Ordnance Survey

scotland_boundary = read_sf("./inputdata/Scotland_boundary.shp")
scotland_boundary = scotland_boundary |> 
  st_transform(4326)

st_layers("inputdata/opgrsp_gb.gpkg")
# Driver: GPKG 
# Available layers:
#   layer_name geometry_type features fields                       crs_name
# 1    access_point         Point   296639      3 OSGB36 / British National Grid
# 2 greenspace_site Multi Polygon   150415      6 OSGB36 / British National Grid

os_park_access = read_sf("inputdata/opgrsp_gb.gpkg")
os_park_access = os_park_access |> 
  st_transform(4326)
scotland_park_access = os_park_access[scotland_boundary, ]
st_write(scotland_park_access, "./inputdata/scotland_park_access.gpkg", delete_dsn = TRUE)
# Upload to inputdata release
old_wd = setwd("inputdata")
system("gh release list")
system("gh release create parks")
system("gh release upload parks scotland_park_access.gpkg --clobber")
setwd(old_wd)

scotland_park_access = st_read("inputdata/scotland_park_access.gpkg")

# park_access_study = scotland_park_access[study_area, ]
# tm_shape(park_access_study) + tm_dots()
# mapview::mapview(park_access_study)

# Calculate park areas and exclude very small ones

os_park_areas = read_sf("inputdata/opgrsp_gb.gpkg", layer = "greenspace_site")
os_park_areas = os_park_areas |> 
  st_transform(4326)
scotland_park_areas = os_park_areas[scotland_boundary, ]
st_write(scotland_park_areas, "./inputdata/scotland_park_areas.gpkg", delete_dsn = TRUE)
# Upload to inputdata release
old_wd = setwd("inputdata")
system("gh release upload parks scotland_park_areas.gpkg --clobber")
setwd(old_wd)

scotland_park_areas = st_read("inputdata/scotland_park_areas.gpkg")

scotland_park_areas = scotland_park_areas |> 
  mutate(area_m = units::drop_units(st_area(scotland_park_areas)))

large_parks = scotland_park_areas |> 
  filter(area_m > 6000)

# park_areas_study = large_parks[study_area, ]
# tm_shape(park_areas_study, alpha = 0.5) + tm_polygons() + 
#   tm_shape(park_access_study) + tm_dots()
# mapview::mapview(park_areas_study)

# Make a 20m buffer around the large parks
large_parks_buff = large_parks |> 
  st_buffer(dist = 20)
st_write(large_parks_buff, "inputdata/large_parks_buff.gpkg", delete_dsn = TRUE)
old_wd = setwd("inputdata")
system("gh release upload parks large_parks_buff.gpkg --clobber")
setwd(old_wd)

large_parks_buff = st_read("inputdata/large_parks_buff.gpkg")

# parks_buff_study = large_parks_buff[study_area, ]
# tm_shape(parks_buff_study, alpha = 0.5) + tm_polygons() + 
#   tm_shape(park_access_study) + tm_dots()

# Filter park access points to the buffered large parks
large_park_access = scotland_park_access[large_parks_buff, ]

# large_park_access_study = park_access_study[parks_buff_study, ]
# tm_shape(parks_buff_study, alpha = 0.5) + tm_polygons() +
#   tm_shape(large_park_access_study) + tm_dots()

# Get OS pois for whole of Scotland
path_teams = Sys.getenv("NPT_TEAMS_PATH")
os_pois = readRDS(file.path(path_teams, "secure_data/OS/os_poi.Rds"))
os_pois = os_pois %>% 
  mutate(groupname = as.character(groupname))
os_recreation = os_pois |> 
  filter(categoryname == "Recreational")

# Join park access points with OS pois
dim(large_park_access)
# [1] 21043     4
dim(os_recreation)
# [1] 6661   10

unique(large_park_access$access_type)
# [1] "Pedestrian"                   "Motor Vehicle And Pedestrian" "Motor Vehicle" 

park_access_standard = large_park_access |> 
  transmute(id, type = access_type)
os_rec_standard = os_recreation |> 
  transmute(id = ref_no, type = name)
os_rec_standard$id = as.character(os_rec_standard$id)

park_points = bind_rows(park_access_standard, os_rec_standard)

# park_points_study = park_points[study_area, ]
# tm_shape(parks_buff_study, alpha = 0.5) + tm_polygons() +
#   tm_shape(park_points_study) + tm_dots()

st_write(park_points, "inputdata/park_points.gpkg", delete_dsn = TRUE)
old_wd = setwd("inputdata")
system("gh release upload parks park_points.gpkg --clobber")
setwd(old_wd)


# Compare effect of changes -----------------------------------------------

tar_load(grid)
grid_df = data.frame(grid)
grid_df = tibble::rowid_to_column(grid_df, "grid_id")

# Original leisure grid
os_leisure = os_pois %>% 
  dplyr::filter(groupname == "Sport and Entertainment") # 20524 points
os_leisure = os_leisure %>% 
  sf::st_transform(27700)
leisure = os_leisure %>% 
  dplyr::mutate(grid_id = sf::st_nearest_feature(os_leisure, grid))
# calculate weighting of each grid point
leisure_grid = leisure %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(grid_id) %>% 
  dplyr::summarise(size = n())
# assign grid geometry
leisure_join = dplyr::inner_join(grid_df, leisure_grid)
leisure_grid = sf::st_as_sf(leisure_join)
leisure_grid = sf::st_transform(leisure_grid, 4326)
leisure_grid_study = leisure_grid[study_area, ]
tm_shape(leisure_grid_study) + tm_dots(size = "size")

# New park grid
park_grid = park_points %>% 
  st_transform(27700)
park_grid = park_grid |> 
  dplyr::mutate(grid_id = sf::st_nearest_feature(park_grid, grid))
park_grid = park_grid %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(grid_id) %>% 
  dplyr::summarise(size = n())
# assign grid geometry
park_join = dplyr::inner_join(grid_df, park_grid)
park_grid = sf::st_as_sf(park_join)
park_grid = sf::st_transform(park_grid, 4326)
park_grid_study = park_grid[study_area, ]
tm_shape(park_grid_study) + tm_dots(size = "size")

# New combined grid
combined_grid = rbind(leisure_grid_study, park_grid_study)
combined_grid = combined_grid |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(grid_id) |> 
  dplyr::summarise(size = sum(size))
# assign grid geometry
combined_join = dplyr::inner_join(grid_df, combined_grid)
combined_grid = sf::st_as_sf(combined_join)
combined_grid = sf::st_transform(combined_grid, 4326)
combined_grid_study = combined_grid[study_area, ]
tm_shape(combined_grid) + tm_dots(size = "size")
  
