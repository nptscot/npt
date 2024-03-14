tar_load(parameters)
tar_load(os_pois)
library(tmap)
tmap_mode("view")

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

park_access = scotland_park_access[study_area, ]
tm_shape(park_access) + tm_dots()
mapview::mapview(park_access)

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

scotland_park_areas = st_read("inputdata/scotland_park_access.gpkg")

# Get parks from OSM, and calculate their centroids
osmextract

# Make 500m grid of points within parks, so larger parks have higher desirability


