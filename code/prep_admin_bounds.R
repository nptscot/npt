# Prep LA bounds
path = "D:/OneDrive - University of Leeds/Data/Admin Boundaries/"

path_la = "Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC.zip"

library(sf)


# LA
dir.create(file.path(tempdir(),"zones"))
unzip(file.path(path, path_la), exdir = file.path(tempdir(),"zones"))

la <- read_sf(file.path(tempdir(),"zones","LAD_DEC_2022_UK_BFC.shp"))
la <- la[,c("LAD22CD")]
la <- st_transform(la, 4326)
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


