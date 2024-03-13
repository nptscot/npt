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
  "sidewalk",
  "ref",
  "surface",
  "segregated",
  "route",           # for proposed routes
  "state",
  "lcn",
  "rcn",
  "ncn",
  "network"
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
# osm_highways = readRDS("./inputdata/osm_highways_2023-08-09.Rds")

names(osm_highways)
summary(osm_highways)

osm_cbd = osm_highways %>%
    select(osm_id, name, highway, maxspeed, bicycle, cycleway, cycleway_left, cycleway_right, cycleway_both, lanes, segregated)
