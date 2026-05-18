# Clip basin rasters

source("R/0_utilities.R")


# Note: Manually remove `.aux.json` files, which are sometimes created by the
# terra package for the clim files.
# https://gis.stackexchange.com/questions/428278/ This can be automated at some
# point.


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make basin input files

purrr::map(seq_len(nrow(watershed_table)), function(x){
  
  print(paste0("------- ", watershed_table$watershed[x], " -------"))
  
  RHESSysPreprocessing::clip_landscape_rasters_by_basin(work_folder = file.path("out_r", "watershed_processing", watershed_table$watershed[x]),
                                                        file_identifier = "_landscape.tif")
  
})

# Quick verification step
shed_num <- 2
aspect <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[shed_num], "aspect.tif"))
plot(aspect)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
