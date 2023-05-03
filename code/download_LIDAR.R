id = "F_DTM_P_10754"

url = "https://environment.data.gov.uk/UserDownloads/interactive/62ec213f385f46358649627db7e0932467856/LIDARCOMP/LIDAR-DTM-2m-2022-TQ26ne.zip"

grids = sf::read_sf("https://github.com/charlesroper/OSGB_Grids/raw/master/GeoJSON/OSGB_Grid_5km.geojson")
grids = grids[,c(1,7:10)]
grids = grids[grids$ENGLAND == "t",]

base_url = "https://environment.data.gov.uk/UserDownloads/interactive/62ec213f385f46358649627db7e0932467856/LIDARCOMP/LIDAR-DTM-2m-2022-"
file_path = "D:/OneDrive - University of Leeds/Data/LIDAR/DTM-2m-2022/"

# URLS don't work

for(i in 1:nrow(grids)){
  message(grids$TILE_NAME[i])
  nm = paste0(substr(grids$TILE_NAME[i],1,4),tolower(substr(grids$TILE_NAME[i],5,6)))
  url = paste0(base_url, nm, ".zip")
  
  download.file(url, 
                destfile = paste0(file_path,nm,".zip"), 
                mode = "wb")
  sleep(1)
  
}



