# Cycling by design compliance
library(tidyverse)
sys_date = Sys.Date()

# Download osm data
et = c(
  "maxspeed",
  "oneway",
  "bicycle",
  "cycleway",
  "cycleway:left",
  "cycleway:right",
  "cycleway:both",
  "lanes",
  "lanes:both_ways",
  "lanes:forward",
  "lanes:backward",
  "lanes:bus",
  "lanes:bus:conditional",
  "oneway",
  "width",      # useful to ensure width of cycleways is at least 1.5m
  "segregated"  # classifies whether cycles and pedestrians are segregated on shared paths
#  "sidewalk",
#  "ref",
#  "surface",
#  "route",           # for proposed routes
#  "state",
#  "lcn",
#  "rcn",
#  "ncn",
#  "network"
)

# Read-in road network data
osm_lines = osmextract::oe_get_network(
  place = "Scotland",
  mode = "cycling",
  extra_tags = et
  # , force_download = TRUE # keep it up-to date
)

# Simple exploratory analysis
# identify services, these are service stations, not realistic cycle trip start/end points
# osm_lines %>%
#   filter(highway == "services") %>%
#   qtm()
# osm_lines %>%
#   filter(highway == "rest_area") %>%
#   qtm()
# osm_lines %>%
#   filter(highway == "track") %>%
#   sample_n(200) %>% 
#   qtm()
# osm_lines %>%
#   filter(highway == "trunk") %>%
#   sample_n(500) %>% 
#   qtm()

to_exclude = "motorway|services|bridleway|disused|emergency|escap|far|foot|path|pedestrian|rest|road|track"

osm_highways = osm_lines %>%
  filter(!str_detect(string = highway, pattern = to_exclude))


dim(osm_highways) 
# [1] 436853     30

save_name = paste0("inputdata/osm_highways_",sys_date,".Rds")
saveRDS(osm_highways, save_name)
# osm_highways = readRDS("./inputdata/osm_highways_2024-03-20.Rds")

names(osm_highways)
# [1] "osm_id"                "name"                  "highway"              
# [4] "waterway"              "aerialway"             "barrier"              
# [7] "man_made"              "access"                "bicycle"              
# [10] "service"               "maxspeed"              "oneway"               
# [13] "cycleway"              "cycleway_left"         "cycleway_right"       
# [16] "cycleway_both"         "lanes"                 "lanes_both_ways"      
# [19] "lanes_forward"         "lanes_backward"        "lanes_bus"            
# [22] "lanes_bus_conditional" "width"                 "segregated"           
# [25] "z_order"               "other_tags"            "geometry" 
summary(osm_highways)

osm_cbd = osm_highways %>%
    select(osm_id, name, highway, maxspeed, bicycle, 
    cycleway, cycleway_left, cycleway_right, cycleway_both, 
    lanes, lanes_both_ways, lanes_forward, lanes_backward, 
    lanes_bus, lanes_bus_conditional, oneway, width, segregated)

names(osm$cbd)
# [1] "osm_id"                "name"                  "highway"              
# [4] "maxspeed"              "bicycle"               "cycleway"             
# [7] "cycleway_left"         "cycleway_right"        "cycleway_both"        
# [10] "lanes"                 "lanes_both_ways"       "lanes_forward"        
# [13] "lanes_backward"        "lanes_bus"             "lanes_bus_conditional"
# [16] "oneway"                "width"                 "segregated"           
# [19] "geometry" 

# Investigate which values are in use

unique(osm_cbd$highway)
#  [1] "residential"    "trunk"          "primary"        "secondary"     
#  [5] "tertiary"       "primary_link"   "trunk_link"     "unclassified"  
#  [9] "service"        "tertiary_link"  "cycleway"       "secondary_link"
# [13] "living_street"  "busway"         "crossing"
unique(osm_cbd$cycleway)
#  [1] NA                      "lane"                  "advisory"             
#  [4] "no"                    "share_busway"          "track"                
#  [7] "opposite"              "opposite_lane"         "yes"                  
# [10] "opposite_share_busway" "shared"                "shared_lane"          
# [13] "separate"              "opposite_track"        "segregated"           
# [16] "mtb"                   "crossing"              "sidepath"             
# [19] "none"                  "sidewalk"              "left"                 
# [22] "unmarked_lane"         "traffic_island" 
unique(osm_cbd$cycleway_left)
# [1] NA              "no"            "track"         "lane"         
# [5] "share_busway"  "shared_lane"   "separate"      "segregated"   
# [9] "advisory"      "buffered_lane" "yes"
unique(osm_cbd$cycleway_right)
# [1] NA              "shared_lane"   "track"         "lane"         
# [5] "no"            "share_busway"  "separate"      "opposite_lane"
# [9] "opposite"      "shared" 
unique(osm_cbd$cycleway_both)
# [1] "no"           NA             "lane"         "share_busway" "separate"    
# [6] "track"        "shared_lane"  "shoulder" 
unique(osm_cbd$maxspeed)
# [1] "20 mph"  "30 mph"  "70 mph"  "50 mph"  NA        "40 mph"  "60 mph" 
# [8] "10 mph"  "5 mph"   "signals" "15 mph"  "15"      "25 mph"  "11 mph" 
# [15] "50"      "9 mph"   "8 mph"   "4 mph"   "5"       "8"   



# Categorise level of segregation

osm_segregation = osm_cbd |>
  mutate(cycle_segregation = case_when(
    cycleway == "separate" ~ "separate",
    cycleway_right == "separate" ~ "separate",
    cycleway_left == "separate" ~ "separate",
    cycleway == "segregated" ~ "segregated",
    cycleway_right == "segregated" ~ "segregated",
    cycleway_left == "segregated" ~ "segregated",
    cycleway == "lane" ~ "lane",
    cycleway_right == "lane" ~ "lane",
    cycleway_left == "lane" ~ "lane",
    cycleway == "track" ~ "track",
    cycleway_left == "track" ~ "track",
    cycleway_right == "track" ~ "track"
    )
  )
