# Cycling by design compliance
library(tidyverse)
library(targets)
library(tmap)
tmap_mode("view")
sys_datetmapsys_date = Sys.Date()


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
  filter(!str_detect(string = highway, pattern = to_exclude),
         !str_detect(string = bicycle, pattern = "mtb|discouraged|unknown")
         )


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

names(osm_cbd)
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

# There are assumptions in OSM about the number of lanes for all roads smaller than trunk/motorway
# So when there are NA values we can assume there is 1 lane 
unique(osm_cbd$lanes)
# [1] NA    "2"   "4"   "1"   "3"   "6"   "5"   "1.5" "0" 
unique(osm_cbd$lanes_both_ways)
# [1] NA  "1"
unique(osm_cbd$lanes_forward)
# [1] NA  "2" "1" "3"
unique(osm_cbd$lanes_backward)
# [1] NA  "1" "2" "4" "3"
unique(osm_cbd$lanes_bus)
# [1] NA                   "1"                  "yes|yes|designated"
unique(osm_cbd$lanes_bus_conditional) # This tag seems to be unusued
# [1] NA

unique(osm_cbd$bicycle)
# [1] NA            "designated"  "yes"         "dismount"    "permissive" 
# [6] "customers"   "destination" "mtb"         "discouraged" "unknown" 

unique(osm_cbd$oneway)
# [1] NA            "yes"         "no"          "-1"          "alternating"
# [6] "reversible" 
unique(osm_cbd$width)
# [1] NA       "5.3"    "5.5"    "5.2"    "4.2"    "2.5 m"  "2.0 m"  "5.6"   
# [9] "8.6"    "10"     "6.5"    "6"      "4.8"    "2.5"    "6.0"    "2.2"   
# [17] "2"      "3"      "4"      "10.4"   "7.2"    "6.1"    "9"      "5.8"   
# [25] "7.5"    "9.1"    "7.3"    "2.75"   "8"      "4.9"    "6.7"    "2.3"   
# [33] "7.8"    "3.2"    "4.5"    "8.5"    "3.1"    "7"      "2.25"   "1.5"   
# [41] "1"      "0.75"   "1.75"   "9.3"    "5.9"    "7.9"    "8.1"    "3.6"   
# [49] "5"      "6.2"    "4.3"    "11"     "1.8"    "3.5"    "11.1"   "9.8"   
# [57] "4.4"    "11.5"   "8.3"    "8.8"    "12"     "7.1"    "4.7"    "4.1"   
# [65] "6.3"    "6.6"    "7.7"    "9.5"    "13"     "10.5"   "2.7"    "13.5"  
# [73] "5.4"    "4.6"    "5.1"    "5.0"    "5.7"    "8.4"    "8.2"    "2.4"   
# [81] "2.6"    "12.8"   "3.3"    "9 m"    "0.5"    "12.5"   "6.9"    "6.8"   
# [89] "narrow" "6.4"    "9.6"    "2.8"    "3.9"    "2m"     "15"     "3.4"   
# [97] "9.0"    "5.5."   "4.0"    "3.0"    "3.8"    "7.4"    "10m"    "3m"    
# [105] "3.7"    "1.6"    "10.0"   "1.1"    "1.5 m"  "1.9"    "8.0"    "11.0"  
# [113] "14"     "4.85"   "2.9"    "8.9"    "0.7"    "1m"     "20"     "5.5 m" 
# [121] "7.0"    "3.75"   "1.75m"  "1.2"    "0.75m"  "1.5m"   "2.3 m"  "16.5"  
# [129] "2.0"    "2.0m"   "4.0m"   "2.1"    "1.4"    "4m"     "1.6 m"  "8.7"   
# [137] "0.5 m"  "4.75"   ".8"     "5'6\""  "11.8"   "14.9"   "10.1"   "2.65"  
# [145] "14.8"   "18"     "17"     "5.9 m" 
unique(osm_cbd$segregated)
# [1] NA    "yes" "no"
unique(osm_cbd$highway)
#  [1] "residential"    "trunk"          "primary"        "secondary"     
# [5] "tertiary"       "primary_link"   "trunk_link"     "unclassified"  
# [9] "service"        "tertiary_link"  "cycleway"       "secondary_link"
# [13] "living_street"  "busway"         "crossing"

# Busways are ok to cycle on (there's only a couple in Scotland anyway) - FINE
busway = osm_cbd |> 
  filter(highway == "busway")
tm_shape(busway) + tm_lines()
# This is just a single lane track in Orkney - FINE
zero_lane = osm_cbd |> 
  filter(lanes == "0")
tm_shape(zero_lane) + tm_lines()
# This is a few small roads that can be treated as single lane - FINE
half_lane = osm_cbd |> 
  filter(lanes == "1.5")
tm_shape(half_lane) + tm_lines()
# A couple of dead-end tracks - REMOVE
mtb = osm_cbd |> 
  filter(bicycle == "mtb")
tm_shape(mtb) + tm_lines()
# dead end track - REMOVE
unknown = osm_cbd |> 
  filter(bicycle == "unknown")
tm_shape(unknown) + tm_lines()
# Towpath section, onroad tram route, pedestrian crossing, etc - KEEP
dismount = osm_cbd |> 
  filter(bicycle == "dismount")
tm_shape(dismount) + tm_lines()

# Check segregation categories in Scotland or Edinburgh

# there are very few of these and most seem to be stepped_or_footway
separate = osm_cbd |> 
  filter(cycleway == "separate")
tm_shape(separate) + tm_lines()
# one single road segment with unclear (post-google streetview) cycle infrastructure
buffered = osm_cbd |> 
  filter(cycleway_left == "buffered_lane")
tm_shape(buffered) + tm_lines()
# Very few, unclear but seem to be pedestrian level
segway = osm_cbd |> 
  filter(cycleway == "segregated")
tm_shape(segway) + tm_lines()

tar_load(study_area)
osm_study = osm_cbd[study_area, ]

# These are off-road paths shared with pedestrians
segno = osm_study |> 
  filter(segregated == "no")
tm_shape(segno) + tm_lines()
# These are more mixed - some fully kerbed, others footway level/shared with pedestrians
segyes = osm_study |> 
  filter(segregated == "yes")
tm_shape(segyes) + tm_lines()

# Classic painted cycle lane
lane = osm_study |> 
  filter(cycleway == "lane")
tm_shape(lane) + tm_lines()
# Light segregation with orca wands
track = osm_study |> 
  filter(cycleway == "track")
tm_shape(track) + tm_lines()

# Mostly detached/remote, but also some adjacent to highways as 'level_track'
# We may need to filter using distance from highway
det = osm_study |> 
  filter(highway == "cycleway")
tm_shape(det) + tm_lines()

# segregated == no |> stepped_or_footway


# Categorise segregation --------------------------------------------------

# The categories are:
# detached_track
# level_track
# stepped_or_footway
# light_segregation
# cycle_lane
# mixed_traffic


# Function for level_track v detached_track

# Make a 10m buffer around highway==cycleway
# If other highways appear within this buffer, it's a level_track
# Of no other highways appear, it's a detached track


osm_segregation = osm_study |>
  mutate(cycle_segregation = case_when(
    
    # Cycleways detached from the road (edit to add level_track)
    highway == "cycleway" ~ "detached_track",
    
    # Cycleways on road
    cycleway == "lane" ~ "cycle_lane",
    cycleway_right == "lane" ~ "cycle_lane",
    cycleway_left == "lane" ~ "cycle_lane",
    cycleway_both == "lane" ~ "cycle_lane",
    
    cycleway == "track" ~ "light_segregation",
    cycleway_left == "track" ~ "light_segregation",
    cycleway_right == "track" ~ "light_segregation",
    cycleway_both == "track" ~ "light_segregation",
    
    
    # Shared with pedestrians (but not highway == cycleway)
    segregated == "no" ~ "stepped_or_footway",
    
    # Rare cases
    cycleway == "separate" ~ "stepped_or_footway",
    cycleway_left == "separate" ~ "stepped_or_footway",
    cycleway_right == "separate" ~ "stepped_or_footway",
    cycleway_both == "separate" ~ "stepped_or_footway",
    cycleway == "buffered_lane" ~ "cycle_lane",
    cycleway_left == "buffered_lane" ~ "cycle_lane",
    cycleway_right == "buffered_lane" ~ "cycle_lane",
    cycleway_both == "buffered_lane" ~ "cycle_lane",
    cycleway == "segregated" ~ "stepped_or_footway",
    cycleway_left == "segregated" ~ "stepped_or_footway",
    cycleway_right == "segregated" ~ "stepped_or_footway",
    cycleway_both == "segregated" ~ "stepped_or_footway",
    
    # Default mixed traffic
    .default = "mixed_traffic"
    )
  )

