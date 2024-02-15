# Prep LA bounds
path = "D:/OneDrive - University of Leeds/Data/Admin Boundaries/"

path_la = "Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC.zip"

library(sf)


# LA

# u = "https://nptscot.blob.core.windows.net/pmtiles/la.pmtiles"
# la = sf::read_sf(u)
# f = basename(u)
# download.file(u, f)
# la = sf::read_sf(f)
# # with gdal from bash:
# system("ogr2ogr la.gpkg la.pmtiles")
# la = sf::read_sf("la.gpkg")
# plot(la)

la <- sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
la <- la[, c("LAD23CD", "LAD23NM")]
# LAs in Scotland, CD starts with "S":
la_scotland <- la[grepl("^S", la$LAD23CD),]
sf::write_sf(la_scotland, "las_scotland_2023.geojson")
sf::write_sf(la, "las_2023.geojson")
piggyback::pb_release_create(repo = "nptscot/npt", "boundaries")
# With gh cli
system("gh release create boundaries-2024")
system("gh release upload boundaries-2024 las_scotland_2023.geojson las_2023.geojson")

st_write(la,"../atumscot/outputs/la.geojson")

unlink(file.path(tempdir(),"LA"), recursive = TRUE)


# Westminster
path_westminster = "Westminster_Parliamentary_Constituencies_(Dec_2021)_UK_BFC.zip"
dir.create(file.path(tempdir(),"zones"))
unzip(file.path(path, path_westminster), exdir = file.path(tempdir(),"zones"))

westminster <- read_sf(file.path(tempdir(),"zones","Westminster_Parliamentary_Constituencies_(Dec_2021)_UK_BFC.shp"))
westminster <- westminster[,c("PCON21CD")]
westminster <- st_transform(westminster, 4326)
st_write(westminster,"../atumscot/outputs/westminster.geojson")
unlink(file.path(tempdir(),"zones"), recursive = TRUE)

# holyrood
path_holyrood = "Scottish_Parliamentary_Constituencies_(May_2021)_Boundaries_SC_BFC.zip"
dir.create(file.path(tempdir(),"zones"))
unzip(file.path(path, path_holyrood), exdir = file.path(tempdir(),"zones"))

holyrood <- read_sf(file.path(tempdir(),"zones","SPC_MAY_2021_SC_BFC.shp"))
holyrood <- holyrood[,c("SPC21CD")]
holyrood <- st_transform(holyrood, 4326)
st_write(holyrood,"../atumscot/outputs/holyrood.geojson")
unlink(file.path(tempdir(),"zones"), recursive = TRUE)

# Wards
path_wards = "Wards_(December_2022)_Boundaries_GB_BFC.zip"
dir.create(file.path(tempdir(),"zones"))
unzip(file.path(path, path_wards), exdir = file.path(tempdir(),"zones"))

wards <- read_sf(file.path(tempdir(),"zones","WD_DEC_22_GB_BFC.shp"))
wards <- wards[,c("WD22CD")]
wards <- st_transform(wards, 4326)
st_write(wards,"../atumscot/outputs/wards.geojson")
unlink(file.path(tempdir(),"zones"), recursive = TRUE)

# Regional
path_regional = "D:/OneDrive - University of Leeds/Data/OS/Boundary Line/bdline_gpkg_gb/Data/bdline_gb.gpkg"
regions <- read_sf(path_regional, "scotland_and_wales_region")
regions <- regions[regions$Area_Description == "Scottish Parliament Electoral Region",]
regions <- regions[,c("Census_Code")]
regions <- st_transform(regions, 4326)
st_write(regions,"../atumscot/outputs/scot_regions.geojson")
