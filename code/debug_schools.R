flow_sf = readRDS("D:/University of Leeds/TEAM - Network Planning Tool - General/secure_data/schools/school_dl_sub30km.Rds")
school_fast <- readRDS("outputdata/school_fast_sub30k.Rds")

long = school_fast[school_fast$length > 40000, ]
qtm(long)
qtm(long) + qtm(flow_sf[flow_sf$route_number %in% long$route_number,], lines.col = "red")