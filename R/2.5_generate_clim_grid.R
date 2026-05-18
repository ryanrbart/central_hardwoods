# Generate clim grid

source("R/0_utilities.R")

# Note: Despite having a setGDALconfig line set in 0_utilities and embedded in
# RHESSysWorkflowinR::generate_clim_grid, this script still produces .xml files
# associated with the outputted tifs. Manually delete for now.


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get dem of landscape to use as base raster

dem_landscape_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  # We are using the DEM landscape raster as a boundary map for climate grid
  dem_landscape <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "dem_landscape.tif"))
  return(dem_landscape)
})
names(dem_landscape_list) <- watershed_table$watershed


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create clim grid

clim_grid_table_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  
  print(paste("--------", watershed_table$watershed[.x], "--------"))
  clim_grid_table <- RHESSysWorkflowinR::generate_clim_grid(dem_landscape = dem_landscape_list[[.x]],
                                                            var_name = "tmmx",
                                                            start_date = "2000-01-01",
                                                            end_date = "2000-01-01",
                                                            data_type = "gridmet",
                                                            output_clim_grid_original = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_original.tif"),
                                                            output_clim_grid_landscape = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_landscape.tif"),
                                                            output_clim_grid_dem_landscape = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_dem_landscape.tif"),
                                                            output_clim_grid_table = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_table.csv"))
  return(clim_grid_table)
})
names(clim_grid_table_list) <- watershed_table$watershed


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------


