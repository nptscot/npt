library(sf)
library(tmap)
library(dplyr)
tmap_mode("view")
sf_use_s2(FALSE)

b_med = readRDS("outputs/buildings_med_lsoa_split.Rds")
b_high = readRDS("outputs/buildings_high_lsoa_split.Rds")
b_low = readRDS("outputs/buildings_low_reg_lsoa_split.Rds")
b_verylow = readRDS("outputs/buildings_low_nat_lsoa_split.Rds")

b_med <- st_make_valid(b_med)


b_high <- b_high[,c("geo_code")]
b_med <- b_med[,c("geo_code")]
b_low <- b_low[,c("geo_code")]
b_verylow <- b_verylow[,c("geo_code")]

dat <- read.csv("D:/OneDrive - University of Leeds/Data/Broadband Speed/InternetUserCalssification2018_Secure.csv")
dat <- dat[,c("LSOA11_CD","GRP_CD")]


b_high <- left_join(b_high, dat, by = c("geo_code" = "LSOA11_CD"))
b_med <- left_join(b_med, dat, by = c("geo_code" = "LSOA11_CD"))
b_low <- left_join(b_low, dat, by = c("geo_code" = "LSOA11_CD"))
b_verylow <- left_join(b_verylow, dat, by = c("geo_code" = "LSOA11_CD"))

st_write(b_high, "C:/tiles/daysmetric_test/high.geojson")
st_write(b_med, "C:/tiles/daysmetric_test/med.geojson")
st_write(b_low, "C:/tiles/daysmetric_test/low.geojson")
st_write(b_verylow, "C:/tiles/daysmetric_test/verylow.geojson")


