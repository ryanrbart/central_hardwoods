# Generate rhessys input files, identify outlet, and clip basin rasters

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create landscape input files

purrr::map(seq_len(nrow(watershed_table)), function(x){
  
  print(paste0("------- ", watershed_table$watershed[x], " -------"))
  
  RHESSysPreprocessing::make_landscape_rasters(dem = file.path("out_r", "watershed_processing", watershed_table$watershed[x], "dem_landscape.tif"),
                                               work_folder = file.path("out_r", "watershed_processing", watershed_table$watershed[x]),
                                               stream_threshold = watershed_table$stream_threshold[x],
                                               patch_method = "simple")
})

# Quick verification step
shed_num <- 1
streams <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[shed_num], "streams_landscape.tif"))
plot(streams)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Adjust pour point 

# Adjustments to pour point are made manually via the mutate function below
# based on iterations with the make_basin_raster function. In future, this
# should be shiny app or function.

pour_point <- dplyr::select(watershed_table, watershed, gauge_long, gauge_lat)

pour_point <- pour_point %>% 
  dplyr::mutate(gauge_long = dplyr::case_when(watershed == "beaver" ~ gauge_long + 0.002,
                                              .default = gauge_long),
                gauge_lat = dplyr::case_when(watershed == "beaver" ~ gauge_lat + 0.004,
                                             .default = gauge_lat))


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make basin raster

purrr::map(seq_len(nrow(watershed_table)), function(x){
  
  print(paste0("------- ", watershed_table$watershed[x], " -------"))
  
  RHESSysPreprocessing::make_basin_raster(work_folder = file.path("out_r", "watershed_processing", watershed_table$watershed[x]),
                                          pour_point_file = NULL,
                                          pour_point_longlat = c(pour_point$gauge_long[x], pour_point$gauge_lat[x]),
                                          proj_epsg = watershed_table$projection[x],
                                          streams = "streams_landscape.tif",
                                          d8_pointer = "d8_pointer_landscape.tif",
                                          snap_dist = 1000,
                                          save_map = TRUE)
  
})

# Quick verification step
shed_num <- 6
basin <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[shed_num], "basin.tif"))
plot(basin)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Generate vector files of basin raster (maybe add as argument to make_basin_raster)

purrr::map(seq_len(nrow(watershed_table)), function(x){
  
  # Import raster
  basin <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[x], "basin.tif"))
  
  # Convert to vector
  basin_v <- as.polygons(basin , values = TRUE, extent=FALSE)
  
  # Export
  terra::writeVector(basin_v, file.path("out_r", "gis", "watershed_shapefiles", paste0("basin_", watershed_table$watershed[x], ".shp")))
  terra::writeVector(basin_v, file.path("out_r", "gis", "watershed_kmls", paste0("basin_", watershed_table$watershed[x], ".kml")))
  
})



