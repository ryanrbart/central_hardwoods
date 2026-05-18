# Create shapefiles with buffered bounding box surrounding each of the central hardwood watersheds

source("R/0_utilities.R")

# Note that most watersheds are from Gages 2, except for the Lower Natural Area
# watershed in the Big Barren region of the Mark Twain National Forest.


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get data boundary data for Gages 2 watersheds

# Import Gages 2 boundary data
boundary_gages_2 <- sf::st_read("../1_data/gages_2/boundaries-shapefiles-by-aggeco/bas_ref_all.shp") %>% 
  sf::st_transform(crs = 4326) 

# Filter shapefiles for gages 2 watersheds
boundary_gages_2_selected <- boundary_gages_2 %>% 
  dplyr::filter(GAGE_ID %in% !!watershed_table$watershed_id) %>% 
  dplyr::left_join(., watershed_table, by = c("GAGE_ID" = "watershed_id"))

# Create buffer (as far as I can tell, the buffer is in meters despite projection being in longlat)
# For station clim data, buffer can probably be 1000m. For gridded data, 8000m should cover 4 or 6 km products.
bbox_gages_2 <- sf::st_buffer(boundary_gages_2_selected, 8000)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Barren (Lower Natural Area (LNA)) processing 

# Coordinates for LNA gauge (Source Table 1 in Big Barren Report)
# NAD83 UTM15N (Easting, Northing)
gauge_coord_barren_utm <- c(672767.129, 4079188.630)

# Assign coordinates to UTM and then reproject to long/lat
gauge_coord_barren_longlat <- sf::st_sf(geom = sf::st_sfc(sf::st_point(gauge_coord_barren_utm)), crs = 32615) %>% 
  sf::st_transform(crs = 4326)

# Find bounding coordinates of the Barren watershed
# Plug coordinates into a viewer. Several options (haven't yet found web based tool that delineates a watershed)
# https://www.epa.gov/waterdata/waters-geoviewer
# https://apps.nationalmap.gov/downloader/#/
# Based on viewer, manually find bounding box for Barren and populate the values below. Note: buffer already included. 
bbox_barren <- sf::st_sf(sf::st_as_sfc(sf::st_bbox(c(xmin = -91.24, ymin = 36.8, xmax = -91.02, ymax = 36.92), crs = 4326)))

# Create larger buffer for accommodating climate grid
bbox_barren <- sf::st_buffer(bbox_barren, 8000)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Save bounding boxes as shapefiles

# Note that shapefile will abbreviate columns greater than a length of 10 (and even sometimes when shorter)
sf::st_write(bbox_gages_2, "out_r/gis/bbox_of_watersheds/bbox_gages_2.shp", delete_layer = TRUE)
sf::st_write(bbox_barren, "out_r/gis/bbox_of_watersheds/bbox_barren.shp", delete_layer = TRUE)

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Combine bounding boxes into a list and save

# Create list of gages 2
bbox_list <- purrr::map(seq_len(nrow(bbox_gages_2)), function(.x){
  dem <- dplyr::slice(bbox_gages_2, .x)
})
names(bbox_list) <- bbox_gages_2$watershed

# Combine gages 2 and Barren
bbox_list <- c(bbox_list,  list(barren = bbox_barren))
# Sort names alphabetically
bbox_list <- bbox_list[order(names(bbox_list))]
# Write bbox list
readr::write_rds(bbox_list, "out_r/gis/bbox_of_watersheds/bbox_list.rds")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Gauge point locations

# Need to download Gages 2 points. Barren already downloaded

# Import Gages 2 point data
gauge_coord_gages_2 <- sf::st_read("../1_data/gages_2/gagesII_9322_point_shapefile/gagesII_9322_sept30_2011.shp") %>% 
  sf::st_transform(crs = 4326) 

# Filter shapefiles for gages 2 watersheds
gauge_coord_gages_2_selected <- gauge_coord_gages_2 %>% 
  dplyr::filter(STAID %in% !!watershed_table$watershed_id) %>% 
  dplyr::left_join(., watershed_table, by = c("STAID" = "watershed_id"))

# Export
sf::st_write(gauge_coord_barren_longlat, "out_r/kml/barren_gauge.kml", append=FALSE)
sf::st_write(gauge_coord_gages_2_selected, "out_r/kml/gages_2_gauge.kml", append=FALSE)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Individual collective boundary files





# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Individual watershed boundary files

# Beaver
beaver_boundary <- boundary_gages_2_selected %>% 
  dplyr::filter(GAGE_ID %in% "03373508")

st_write(beaver_boundary, "out_r/kml/beaver_boundary.kml")
st_write(beaver_boundary, "out_r/gis/misc/beaver_boundary.shp")


# Lusk
lusk_boundary <- boundary_gages_2_selected %>% 
  dplyr::filter(GAGE_ID %in% "03384450")

st_write(lusk_boundary, "out_r/kml/lusk_boundary.kml")
st_write(lusk_boundary, "out_r/gis/misc/lusk_boundary.shp")


# Monday
monday_boundary <- boundary_gages_2_selected %>% 
  dplyr::filter(GAGE_ID %in% "03158200")

st_write(monday_boundary, "out_r/kml/monday_boundary.kml")
st_write(monday_boundary, "out_r/gis/misc/monday_boundary.shp")


# Patoka
patoka_boundary <- boundary_gages_2_selected %>% 
  dplyr::filter(GAGE_ID %in% "03374455")

st_write(patoka_boundary, "out_r/kml/patoka_boundary.kml")
st_write(patoka_boundary, "out_r/gis/misc/patoka_boundary.shp")


# Potomac
potomac_boundary <- boundary_gages_2_selected %>% 
  dplyr::filter(GAGE_ID %in% "01606000")

st_write(potomac_boundary, "out_r/kml/potomac_boundary.kml")
st_write(potomac_boundary, "out_r/gis/misc/potomac_boundary.shp")



