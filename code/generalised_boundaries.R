library(sf)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/Wales_lsoa_2011_gen_clipped.zip", exdir = "tmpzip")
wales_gen <- read_sf("tmpzip/wales_lsoa_2011_gen_clipped.shp")
unlink("tmpzip", recursive = TRUE)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/Wales_lsoa_2011_sgen_clipped.zip", exdir = "tmpzip")
wales_supgen <- read_sf("tmpzip/wales_lsoa_2011_sgen_clipped.shp")
unlink("tmpzip", recursive = TRUE)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/SG_DataZoneBdry_2011.zip", exdir = "tmpzip")
scot_full <- read_sf("tmpzip/SG_DataZone_Bdry_2011.shp")
unlink("tmpzip", recursive = TRUE)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_clipped.zip", exdir = "tmpzip")
GB_full <- read_sf("tmpzip/infuse_lsoa_lyr_2011_clipped.shp")
unlink("tmpzip", recursive = TRUE)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/England_lsoa_2011_gen_clipped.zip", exdir = "tmpzip")
Eng_gen <- read_sf("tmpzip/england_lsoa_2011_gen_clipped.shp")
unlink("tmpzip", recursive = TRUE)

dir.create("tmpzip")
unzip("D:/OneDrive - University of Leeds/Data/OA Bounadries/England_lsoa_2011_sgen_clipped.zip", exdir = "tmpzip")
Eng_supgen <- read_sf("tmpzip/england_lsoa_2011_sgen_clipped.shp")
unlink("tmpzip", recursive = TRUE)

scot_gen <- rmapshaper::ms_simplify(scot_full, keep = 0.5,
                                    keep_shapes = TRUE)
scot_gen <- st_make_valid(scot_gen)

scot_supgen <- rmapshaper::ms_simplify(scot_full, keep = 0.05,
                                    keep_shapes = TRUE)
scot_supgen <- st_make_valid(scot_supgen)


# Make GB Gen
Eng_gen <- Eng_gen[,c("code")]
scot_gen <- scot_gen[,c("DataZone")]
wales_gen <- wales_gen[,c("code")]

names(scot_gen) <- c("code","geometry")

GB_gen = rbind(Eng_gen, scot_gen, wales_gen)

# Make GB Supergen
Eng_supgen <- Eng_supgen[,c("code")]
scot_supgen <- scot_supgen[,c("DataZone")]
wales_supgen <- wales_supgen[,c("code")]

names(scot_supgen) <- c("code","geometry")

GB_supgen = rbind(Eng_supgen, scot_supgen, wales_supgen)

# Make GB Full
GB_full <- GB_full[,c("geo_code")]
names(GB_full) <- c("code","geometry")

# Convert to 4326
sf_use_s2(FALSE)
GB_full <- st_transform(GB_full, 4326)
GB_gen <- st_transform(GB_gen, 4326)
GB_supgen <- st_transform(GB_supgen, 4326)

GB_full <- st_make_valid(GB_full)
GB_gen <- st_make_valid(GB_gen)
GB_supgen <- st_make_valid(GB_supgen)

summary(st_is_valid(GB_full))
summary(st_is_valid(GB_gen))
summary(st_is_valid(GB_supgen))

saveRDS(GB_full, "D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_full.Rds")
saveRDS(GB_gen, "D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_generalised.Rds")
saveRDS(GB_supgen, "D:/OneDrive - University of Leeds/Data/OA Bounadries/GB_LSOA_2011_super_generalised.Rds")
