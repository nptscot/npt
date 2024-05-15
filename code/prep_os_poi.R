# Prep OS POI data
# Secure Data not for publication
library(sf)
library(dplyr)
library(targets)
library(tmap)
sf_use_s2(FALSE)

secure_path = Sys.getenv("NPT_TEAMS_PATH")
dir.create(file.path(tempdir(),"poi"))
unzip("D:/OneDrive - University of Leeds/Data/OS/Points of Intrest/2023/Download_2300307.zip",
      exdir = file.path(tempdir(),"poi"))
poi = st_read(file.path(tempdir(),"poi/poi_5111956/poi_5111956.gpkg"))
unlink(file.path(tempdir(),"poi"), recursive = TRUE)
poi = poi[,c("ref_no","name","groupname","categoryname","classname", "brand","qualifier_type","qualifier_data")]

scot = readRDS("inputdata/lads_scotland.Rds")
scot = st_combine(scot)
scot = st_buffer(scot, 0)
scot = st_transform(scot, 27700)

poi_scot = poi[scot,]

# types = group_by(st_drop_geometry(poi_scot), 
#                  groupname,categoryname,classname) |>
#   summarise(count = n())
# types = types[order(types$groupname, types$categoryname),]
# write.csv(types,"inputdata/poi_types.csv", row.names = FALSE)

types = read.csv("inputdata/poi_types.csv")
types$count <- NULL

poi_scot = left_join(poi_scot, types, by = c("groupname","categoryname","classname"))
poi_scot = st_transform(poi_scot, 4326)

#Remove a few problem locations
poi_scot$workplace[poi_scot$ref_no %in% c(161055074, 159995345, 161138082)] <- FALSE


# Add in OA centroids when no POI in a zone
zones = readRDS("inputdata/DataZones.Rds")
subpoints_origins = readRDS("inputdata/oas.Rds")

poi_scot_join = st_join(poi_scot[poi_scot$workplace,], zones)
poi_scot_join = unique(poi_scot_join$DataZone)

zones_missing = zones[!zones$DataZone %in% poi_scot_join,]
subpoints_origins = subpoints_origins[zones_missing,]

subpoints_origins$ref_no = 0
subpoints_origins$name = subpoints_origins$code
subpoints_origins$groupname = "OA Centroid"
subpoints_origins$categoryname = "OA Centroid"
subpoints_origins$classname = "OA Centroid"
subpoints_origins$brand = NA
subpoints_origins$qualifier_type = NA
subpoints_origins$qualifier_data = NA
subpoints_origins$workplace = TRUE
subpoints_origins$geom = subpoints_origins$geometry
subpoints_origins$geometry = NULL
st_geometry(subpoints_origins) = "geom"

subpoints_origins = subpoints_origins[,names(poi_scot)]

poi_scot = rbind(poi_scot, subpoints_origins)

foo = zones[zones$DataZone == "S01008460",]
bar = poi_scot[foo,]
qtm(foo) + qtm(bar)

saveRDS(poi_scot,file.path(secure_path,"secure_data/OS/os_poi.Rds"))

# jitterS01008460
# foo = poi_scot[poi_scot$classname == "Historic Buildings Including Castles, Forts and Abbeys",]
# qtm(foo) 
