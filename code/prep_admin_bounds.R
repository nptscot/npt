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

la = sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

la = la[, c("LAD23CD", "LAD23NM")]
# LAs in Scotland, CD starts with "S":
la_scotland = la[grepl("^S", la$LAD23CD),]

# Get the regions from the LAD23NM
# https://www.publiccontractsscotland.gov.uk/Maps/LocalAuthorityRegions.aspx

la_to_region_lookup = tibble::tribble(
    ~Region, ~LAD23NM,
    "Aberdeen and North East", "Aberdeenshire",
    "Aberdeen and North East", "Aberdeen City",
    "Aberdeen and North East", "Moray",
    # "Highlands and Islands",
    # Argyll and Bute, Eilean Siar (Western Isles), Highland, Orkney, Shetland
    "Highlands and Islands", "Argyll and Bute",
    "Highlands and Islands", "Na h-Eileanan Siar",
    "Highlands and Islands", "Highland",
    "Highlands and Islands", "Orkney Islands",
    "Highlands and Islands", "Shetland Islands",
    # Tayside, Central and Fife	Angus, Clackmannanshire, Dundee City, Falkirk, Fife, Perth and Kinross, Stirling    
    "Tayside, Central and Fife", "Angus",
    "Tayside, Central and Fife", "Clackmannanshire",
    "Tayside, Central and Fife", "Dundee City",
    "Tayside, Central and Fife", "Falkirk",
    "Tayside, Central and Fife", "Fife",
    "Tayside, Central and Fife", "Perth and Kinross",
    "Tayside, Central and Fife", "Stirling",
    # Edinburgh and Lothians	City of Edinburgh, East Lothian, Midlothian, West Lothian
    "Edinburgh and Lothians", "City of Edinburgh",
    "Edinburgh and Lothians", "East Lothian",
    "Edinburgh and Lothians", "Midlothian",
    "Edinburgh and Lothians", "West Lothian",
    # Glasgow and Strathclyde	East Ayrshire, East Dunbartonshire, East Renfrewshire, Glasgow City, Inverclyde, North Ayrshire, North Lanarkshire, Renfrewshire, South Ayrshire, South Lanarkshire, West Dunbartonshire
    "Glasgow and Strathclyde", "East Ayrshire",
    "Glasgow and Strathclyde", "East Dunbartonshire",
    "Glasgow and Strathclyde", "East Renfrewshire",
    "Glasgow and Strathclyde", "Glasgow City",
    "Glasgow and Strathclyde", "Inverclyde",
    "Glasgow and Strathclyde", "North Ayrshire",
    "Glasgow and Strathclyde", "North Lanarkshire",
    "Glasgow and Strathclyde", "Renfrewshire",
    "Glasgow and Strathclyde", "South Ayrshire",
    "Glasgow and Strathclyde", "South Lanarkshire",
    "Glasgow and Strathclyde", "West Dunbartonshire",
    # Scotland South	Dumfries and Galloway, Scottish Borders
    "Scotland South", "Dumfries and Galloway",
    "Scotland South", "Scottish Borders"
)
la_regions = dplyr::left_join(la_scotland, la_to_region_lookup)
la_regions |>
  select(Region) |>
  plot()
# check for NAs: 
la_regions[is.na(la_regions$Region),]

# system("gh release upload boundaries-2024 las_scotland_2023.geojson las_2023.geojson --clobber")
dir.create("inputdata/boundaries", showWarnings = FALSE)
sf::write_sf(la_regions, "inputdata/boundaries/la_regions_2023.geojson", delete_dsn = TRUE)
sf::write_sf(la, "inputdata/boundaries/las_2023.geojson", delete_dsn = TRUE)

# https://github.com/nptscot/npt/releases/download/boundaries-2024/las_2023.geojson

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
