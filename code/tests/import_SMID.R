# Import SIMD data
library(sf)
library(dplyr)


dir.create(file.path(tempdir(),"SIMD"))
unzip("../inputdata/SIMD/simd2020_withgeog.zip",
      exdir = file.path(tempdir(),"SIMD"))
files <- list.files(file.path(tempdir(),"SIMD/simd2020_withgeog"), full.names = TRUE)

zones <- read_sf(file.path(tempdir(),"SIMD/simd2020_withgeog/sc_dz_11.shp"))
simd <- read.csv(file.path(tempdir(),"SIMD/simd2020_withgeog/simd2020_withinds.csv"))
if(length(simd$`ï..Data_Zone`)>1) {
  simd = simd %>% 
    rename(Data_Zone = `ï..Data_Zone`)    
}
unlink(file.path(tempdir(),"SIMD"), recursive = TRUE)

zones <- zones[,c("DataZone","Name","TotPop2011","ResPop2011","HHCnt2011")]
simd$Intermediate_Zone <- NULL
simd$Council_area  <- NULL

zones <- left_join(zones, simd, by = c("DataZone" = "Data_Zone"))
zones <- st_make_valid(zones)

# Split into map
zones_map <- zones[,c("DataZone","Total_population","SIMD2020v2_Decile",
                      "drive_petrol","drive_GP",
                      "drive_post","drive_primary","drive_retail",
                      "drive_secondary","PT_GP","PT_post",
                      "PT_retail","broadband")]
zones_data <- st_drop_geometry(zones)


saveRDS(zones_map,"outputdata/SIMD_zones.Rds")
saveRDS(zones_data,"outputdata/SIMD_data.Rds")

# Prep for tileing

zones_map$drive_petrol <- round(zones_map$drive_petrol, 1)
zones_map$drive_GP <- round(zones_map$drive_GP, 1)
zones_map$drive_post <- round(zones_map$drive_post, 1)
zones_map$drive_primary <- round(zones_map$drive_primary, 1)
zones_map$drive_retail <- round(zones_map$drive_retail, 1)
zones_map$drive_secondary <- round(zones_map$drive_secondary, 1)
zones_map$PT_GP <- round(zones_map$PT_GP, 1)
zones_map$PT_post <- round(zones_map$PT_post, 1)
zones_map$PT_retail <- round(zones_map$PT_retail, 1)
zones_map$broadband <- as.integer(gsub("%","",zones_map$broadband)) 

st_write(zones_map,"outputs/data_zones.geojson")
