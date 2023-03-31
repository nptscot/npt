# See tippacanoe for notes

# cd /tmp
# git clone https://github.com/felt/tippecanoe.git
# cd tippecanoe
# make -j
# make install
tippecanoe --version

# Route network
tippecanoe -o outputdata/rnet.pmtiles \
  --name=rnet \
  --layer=rnet \
  --attribution=UniverstyofLeeds \
  --minimum-zoom=6 \
  --maximum-zoom=13 \
  --drop-smallest-as-needed \
  --maximum-tile-bytes=5000000 \
  --simplification=10 \
  --buffer=5 \
  --force  rnet.geojson


