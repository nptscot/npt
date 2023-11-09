import geopandas as gpd
from shapely.geometry import MultiPolygon
import pandas as pd
import json

# TODO: use small dataset if open data build is TRUE
# Load parameters from JSON file
with open('parameters.json') as f:
    params = json.load(f)

# Check the 'open_data_build' parameter
open_data_build = params['open_data_build'][0]

# Define URLs
open_data_url = "https://github.com/ropensci/stplanr/releases/download/v1.0.2/rnet_y_ed.geojson"
closed_data_url = "https://github.com/nptscot/networkmerge/releases/download/v0.1/combined_network_tile.geojson"

# Choose the correct URL based on the parameter
url_to_load = open_data_url if open_data_build else closed_data_url

# Read the GeoJSON file into a GeoDataFrame
rnet_y = gpd.read_file(url_to_load)

# Read the GeoPackage file 'rnet_merged_all.gpkg' into a GeoDataFrame.
# rnet_merged_all = gpd.read_file("tmp/rnet_merged_all.gpkg")
rnet_merged_all = gpd.read_file("tmp/rnet_merged_all.geojson")

rnet_yp = rnet_y

# Apply a buffer of 0.0002 decimal degrees to all geometries in 'rnet_merged_all' with a flat end cap style (cap_style=3).
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

# Change the coordinate reference system of the combined data to EPSG:4326 (WGS 84).
combined_data = combined_data.to_crs(epsg=4326)

# Prepare a list of column names to be converted, excluding specific columns.
cols_to_convert = combined_data.columns.to_list()
items_to_remove = ['geometry', 'length_x_original', 'length_x_cropped', 'value']
cols_to_convert = [col for col in cols_to_convert if col not in items_to_remove]

# Fill NA/NaN values with 0 for the columns to be converted.
combined_data[cols_to_convert] = combined_data[cols_to_convert].fillna(0)

# Round the values and convert them to integers for the columns to be converted.
combined_data[cols_to_convert] = combined_data[cols_to_convert].round().astype(int)

# Save the simplified_network GeoDataFrame to a GeoPackage file.
combined_data.to_file("tmp/simplified_network.gpkg", driver='GPKG')