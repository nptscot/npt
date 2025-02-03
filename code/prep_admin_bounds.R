# Prep LA bounds
path = "D:/OneDrive - University of Leeds/Data/Admin Boundaries/"

path_la = "Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC.zip"

library(sf)
library(dplyr)


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


# Boundary options: 
# https://www.ons.gov.uk/methodology/geography/geographicalproducts/digitalboundaries
    # (BFC) Full resolution - clipped to the coastline (Mean High Water mark)
    # (BFE) Full resolution - extent of the realm (usually this is the Mean Low Water mark but, in some cases, boundaries extend beyond this to include offshore islands)
    # (BGC) Generalised (20m) - clipped to the coastline (Mean High Water mark)
    # (BUC) Ultra Generalised (500m) - clipped to the coastline (Mean High Water mark)


la_bsc = sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")
la_bfe = sf::read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BFE/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

# BFE version is 50x larger than BSC but we need the BFE version for the islands
object.size(la_bfe) |> as.numeric() /
  object.size(la_bsc) |> as.numeric() 


la_bfe_simplified = rmapshaper::ms_simplify(la_bfe, keep = 0.02)

# BFE version is now 1.4x larger than BSC
object.size(la_bfe_simplified) |> as.numeric() /
  object.size(la_bsc) |> as.numeric()

mapview::mapview(la_bsc) + mapview::mapview(la_bfe_simplified)

la = la_bsc

la = la[, c("LAD23CD", "LAD23NM")]
# LAs in Scotland, CD starts with "S":
la_scotland = la[grepl("^S", la$LAD23CD),]

# Get the regions from the LAD23NM
# https://www.publiccontractsscotland.gov.uk/Maps/LocalAuthorityRegions.aspx

la_to_region_lookup = tibble::tribble(
    ~Region, ~LAD23NM,
    # HITRANS
    "HITRANS", "Highland",
    "HITRANS", "Moray",
    "HITRANS", "Orkney Islands",
    "HITRANS", "Na h-Eileanan Siar",
    "HITRANS", "Argyll and Bute", 
    
    # Nestrans
    "Nestrans", "Aberdeen City",
    "Nestrans", "Aberdeenshire",
    
    # SESTRAN
    "SESTRAN", "City of Edinburgh",
    "SESTRAN", "Clackmannanshire",
    "SESTRAN", "East Lothian",
    "SESTRAN", "Falkirk",
    "SESTRAN", "Fife",
    "SESTRAN", "Midlothian",
    "SESTRAN", "Scottish Borders",
    "SESTRAN", "West Lothian",
    
    # SPT
    # Moray and most of the Argyll and Bute area (Helensburgh and Lomond is covered by SPT)
    # Source: https://home.scotland-excel.org.uk/about-us/our-members/highlands-and-islands-transport-partnership-hitrans/
    # "SPT", "Argyll and Bute",
    "SPT", "East Ayrshire",
    "SPT", "East Dunbartonshire",
    "SPT", "East Renfrewshire",
    "SPT", "Glasgow City",
    "SPT", "Inverclyde",
    "SPT", "North Ayrshire",
    "SPT", "North Lanarkshire",
    "SPT", "Renfrewshire",
    "SPT", "South Ayrshire",
    "SPT", "South Lanarkshire",
    "SPT", "West Dunbartonshire",
    
    # SWESTRANS
    "SWESTRANS", "Dumfries and Galloway",
    
    # Tactran
    "Tactran", "Angus",
    "Tactran", "Dundee City",
    "Tactran", "Perth and Kinross",
    "Tactran", "Stirling",
    
    # ZetTrans
    "ZetTrans", "Shetland Islands"
)

la_regions = dplyr::left_join(la_scotland, la_to_region_lookup)
la_regions |>
  select(Region) |>
  mapview::mapview(zcol = "Region")

# check for NAs: 
la_regions[is.na(la_regions$Region),]

# Check for duplicates:
la_regions |>
  count(LAD23NM) |>
  filter(n > 1)
  
scottish_regions = la_regions |>
  group_by(Region) |>
  summarise(`Number of LAs` = n(), `LAs` = paste(LAD23NM, collapse = ", ")) |>
  arrange(desc(`Number of LAs`))


# Save for future reference:
sf::write_sf(la_regions, "la_regions_scotland_bfe_simplified_2023.geojson", delete_dsn = TRUE)
sf::write_sf(la, "la_uk_bfe_simplified_2023.geojson", delete_dsn = TRUE)
sf::write_sf(scottish_regions, "scottish_regions.geojson", delete_dsn = TRUE)

system("gh release upload boundaries-2024 la_regions_scotland_bfe_simplified_2023.geojson --clobber")
system("gh release upload boundaries-2024 la_uk_bfe_simplified_2023.geojson --clobber")
system("gh release upload boundaries-2024 scottish_regions.geojson --clobber")
# Resulting dataset: 
# https://github.com/nptscot/npt/releases/download/boundaries-2024/la_regions_scotland_bfe_simplified_2023.geojson

# Test reading the data back in:
la_regions = sf::read_sf("https://github.com/nptscot/npt/releases/download/boundaries-2024/la_regions_scotland_bfe_simplified_2023.geojson")
mapview::mapview(la_regions, zcol = "Region")
scottish_regions = sf::read_sf("https://github.com/nptscot/npt/releases/download/boundaries-2024/scottish_regions.geojson")
mapview::mapview(scottish_regions)

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
