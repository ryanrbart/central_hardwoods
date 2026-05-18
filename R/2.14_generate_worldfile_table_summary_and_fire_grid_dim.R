# Generate a summary of worldfile table variables across watersheds

source("R/0_utilities.R")

# Note: The generate_worldfile_table_summary does not currently work for Central
# Hardwood watersheds. The fire grid dimension code does work.


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import worldfile_tables as a list

worldfile_table_list <- purrr::map(.x = seq_len(nrow(watershed_table)), .f = \(.x){
  RHESSysWorkflowinR::import_worldfile_table(worldfile_table = file.path("out_r",
                                                                         watershed_table$watershed[.x],
                                                                         paste0("worldfile_table_", watershed_table$watershed[.x], ".csv")))
  
})
names(worldfile_table_list) <-  watershed_table$watershed

# Create 'basin' summary
worldfile_table_summary_basin <- RHESSysWorkflowinR::generate_worldfile_table_summary(worldfile_table_list = worldfile_table_list,
                                                                                output_type = "basin",
                                                                                patch_size = 90,
                                                                                output_filename = "out_r/worldfile_table_summary_basin.csv")

# Create 'landcover' summary
worldfile_table_summary_landcover <- RHESSysWorkflowinR::generate_worldfile_table_summary(worldfile_table_list = worldfile_table_list,
                                                                                output_type = "landcover",
                                                                                patch_size = 90,
                                                                                output_filename = "out_r/worldfile_table_summary_landcover.csv")

# print(worldfile_table_summary_basin)
# print(worldfile_table_summary_landcover)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get the number of rows and columns in the fire grid tables

# Import each table

dimensions_table <- purrr::map_dfr(.x = seq_len(nrow(watershed_table)), .f = \(.x){

  # Import each fire grid
  fire_grid <- readr::read_table(file.path("ws",
                                           watershed_table$watershed[.x],
                                           "auxdata",
                                           paste0(watershed_table$watershed[.x], "_watershed.dem")),
                                 col_names = FALSE)
  
  # Find rows and cols
  dimensions_table <- tibble::tibble(name =  watershed_table$watershed[.x],
                                     nrow = nrow(fire_grid),
                                     ncol = ncol(fire_grid))
  
  return(dimensions_table)
})

print(dimensions_table)


