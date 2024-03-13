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
st_layers("inputdata/opgrsp_gb.gpkg")
os_parks = read_sf("inputdata/opgrsp_gb.gpkg")
scotland_boundary = scotland_boundary |> 
  st_transform(4326)
os_parks = os_parks |> 
  st_transform(4326)
scotland_parks = os_parks[scotland_boundary, ]
st_write(scotland_parks, "./inputdata/scotland_parks.gpkg")
# Upload to inputdata release
old_wd = setwd("inputdata")
system("gh release list")
system("gh release create parks")
system("gh release upload parks scotland_parks.gpkg --clobber")
setwd(old_wd)

parks = scotland_parks[study_area, ]
tm_shape(parks) + tm_polygons()
mapview::mapview(parks)

# Calculate park areas and weight by area and number of entrances

# Get parks from OSM, and calculate their centroids
osmextract

# Make 500m grid of points within parks, so larger parks have higher desirability


