# Prep OS POI data
# Secure Data not for publication
library(sf)
library(dplyr)
sf_use_s2(FALSE)

secure_path = Sys.getenv("NPT_TEAMS_PATH")

poi = st_read("D:/OneDrive - University of Leeds/Data/OS/Points of Intrest/2023/Download_2300307/poi_5111956/poi_5111956.gpkg")
poi = poi[,c("ref_no","name","groupname","categoryname","classname", "brand","qualifier_type","qualifier_data")]

scot = readRDS("inputdata/lads_scotland.Rds")
scot = st_combine(scot)
scot = st_buffer(scot, 0)
scot = st_transform(scot, 27700)

poi_scot = poi[scot,]

# types = group_by(st_drop_geometry(poi_scot), 
#                  groupname,categoryname,classname) %>%
#   summarise(count = n())
# types = types[order(types$groupname, types$categoryname),]
# write.csv(types,"inputdata/poi_types.csv", row.names = FALSE)

types = read.csv("inputdata/poi_types.csv")
types$count <- NULL

poi_scot = left_join(poi_scot, types, by = c("groupname","categoryname","classname"))
poi_scot = st_transform(poi_scot, 4326)

saveRDS(poi_scot,file.path(secure_path,"secure_data/OS/os_poi.Rds"))


# foo = poi_scot[poi_scot$classname == "Historic Buildings Including Castles, Forts and Abbeys",]
# qtm(foo) 
