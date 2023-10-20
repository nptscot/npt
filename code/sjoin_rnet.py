import geopandas as gpd
from shapely.geometry import MultiPolygon
import pandas as pd

# rnet_merged_all = gpd.read_file(f"data-raw/rnet_merged_all.geojson")
# rnet_y = gpd.read_file("https://github.com/nptscot/networkmerge/releases/download/v0.1/combined_network_tile.geojson")

#read reproducibility of data
rnet_merged_all = gpd.read_file("tmp/rnet_merged_all.gpkg")
# To be replaced with combined network:
rnet_y = gpd.read_file("https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_y_ed.geojson")


rnet_yp = rnet_y

# Set buffer distance and create a buffered GeoDataFrame around rnet_merged_all geometries, using a flat end cap style

rnet_merged_all_buffer = rnet_merged_all.buffer(0.0002, cap_style=3)

# Create a unary union of the buffered geometries to create a single geometry object
single_rnet_merged_all_buffer = rnet_merged_all_buffer.unary_union
# Convert the single geometry object into a GeoDataFrame
single_rnet_merged_all_buffer_gdf = gpd.GeoDataFrame(geometry=[single_rnet_merged_all_buffer])

# Perform a spatial join between rnet_yp and the buffer, keeping only those geometries from rnet_yp that are within the buffer
within_join = gpd.sjoin(rnet_yp, single_rnet_merged_all_buffer_gdf, op='within')

# Keep only those geometries from rnet_yp that were NOT within the buffer
rnet_yp_rest = rnet_yp.loc[~rnet_yp.index.isin(within_join.index)]

# Concatenate (vertically stack) the rnet_yp_rest and rnet_merged_all GeoDataFrames and check the number of columns
combined_data = gpd.GeoDataFrame(pd.concat([rnet_yp_rest, rnet_merged_all], ignore_index=True))

combined_data = combined_data.to_crs(epsg=4326)

cols_to_convert = combined_data.columns.to_list()
items_to_remove = ['geometry', 'length_x_original', 'length_x_cropped', 'value']

# Remove items from the list
cols_to_convert = [col for col in cols_to_convert if col not in items_to_remove]

combined_data[cols_to_convert] = combined_data[cols_to_convert].fillna(0)

combined_data[cols_to_convert] = combined_data[cols_to_convert].round().astype(int)

combined_data.to_file("tmp/simplified_network.gpkg", driver='GPKG')