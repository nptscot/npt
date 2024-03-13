tar_load(parameters)
tar_load(os_pois)
library(tmap)
tmap_mode("view")

# Get Recreational POIs (includes playgrounds, country and national parks, commons, and picnic areas)

# unique(os_pois$groupname)
# # [1] "Attractions"                        "Public Infrastructure"              "Education and Health"              
# # [4] "Sport and Entertainment"            "Commercial Services"                "Accommodation, Eating and Drinking"
# # [7] "Transport"                          "Manufacturing and Production"       "Retail"                            
# # [10] "OA Centroid"

# os_attraction = os_pois |> 
#   filter(groupname == "Attractions")
# unique(os_attraction$classname)
# unique(os_attraction$categoryname)
# tm_shape(os_attraction) + tm_dots()

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

# Get parks from OSM, and calculate their centroids
osmextract

# Make 500m grid of points within parks, so larger parks have higher desirability


