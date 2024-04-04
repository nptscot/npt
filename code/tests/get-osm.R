# The aim of this script is to get and save OSM data

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
osm_lines %>%
  filter(highway == "services") %>%
  qtm()
osm_lines %>%
  filter(highway == "rest_area") %>%
  qtm()
osm_lines %>%
  filter(highway == "track") %>%
  sample_n(200) %>% 
  qtm()
osm_lines %>%
  filter(highway == "trunk") %>%
  sample_n(500) %>% 
  qtm()

to_exclude = "motorway|services|bridleway|disused|emergency|escap|far|foot|path|rest|road|track"

osm_highways = osm_lines %>%
  filter(!str_detect(string = highway, pattern = to_exclude),
         is.na(bicycle)|!str_detect(string = bicycle, pattern = "mtb|discouraged|unknown"),
         !(str_detect(string = highway, pattern = "pedestrian")& !str_detect(string = bicycle, pattern = "designated")))

dim(osm_highways) 
# [1] 423190     30
saveRDS(osm_highways, "inputdata/osm_highways_2023-08-09.Rds") # 46 MB file
