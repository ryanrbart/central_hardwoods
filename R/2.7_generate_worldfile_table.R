# Generate a worldfile dataframe from rasters for querying and data analysis.

source("R/0_utilities.R")

# This step is superceded by running 2.17 after the 2.8.

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Assign raster names

raster_names = c("aspect", "basin",
                 "cf_overstory", "cf_understory",
                 "clim_grid", "dem",
                 "ehr", "lc_overstory",
                 "lc_understory", "patch",
                 "slope", "streams",
                 "subbasin", "whr")



worldfile_table_list <- purrr::map(.x = seq_len(nrow(watershed_table)), .f = \(.x){
  
  print(paste0("------- ", watershed_table$watershed[.x], " -------"))
  
  RHESSysWorkflowinR::generate_worldfile_table(input_path = file.path("out_r", "watershed_processing", watershed_table$watershed[.x]), 
                                               raster_names = raster_names,
                                               convert_aspect = FALSE,
                                               input_file_ext = ".tif",
                                               remove_buffer = TRUE,
                                               output_file = file.path("out_r",
                                                                       watershed_table$watershed[.x],
                                                                       paste0("worldfile_table_", watershed_table$watershed[.x], ".csv")))
})

names(worldfile_table_list) <-  watershed_table$watershed


