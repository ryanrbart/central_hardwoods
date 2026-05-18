# Generate clim files

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get dem_landscape, clim_grid_landscape and clim_grid_dem_landscape (Each has
# same extent and resolution).

dem_landscape_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  dem_landscape <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "dem_landscape.tif"))
  return(dem_landscape)
})
names(dem_landscape_list) <- watershed_table$watershed

clim_grid_landscape_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  clim_grid_landscape = terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_landscape.tif"))
  return(clim_grid_landscape)
})
names(clim_grid_landscape_list) <- watershed_table$watershed

clim_grid_dem_landscape_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  clim_grid_dem_landscape = terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_dem_landscape.tif"))
  return(clim_grid_dem_landscape)
})
names(clim_grid_dem_landscape_list) <- watershed_table$watershed

# ----
# Get clim_grid_table_list

clim_grid_table_list <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  clim_grid_table = readr::read_csv(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "clim_grid_table.csv"), show_col_types = FALSE)
  return(clim_grid_table)
})
names(clim_grid_table_list) <- watershed_table$watershed



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Call generate_clim_file function

# Right now just trying to call directly for gridmet. Eventually, will need to
# generate a table or something with all the different outputs that I want (gcm,
# rcp, etc.). Then call a wrapper function around generate_clim_file or just use
# purrr::map across every scenario in the table.

output_list_pr <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  
  output <- RHESSysWorkflowinR::generate_clim_file(dem_landscape = dem_landscape_list[[.x]],
                                                   clim_grid_table = clim_grid_table_list[[.x]],
                                                   output_folder = file.path("ws", watershed_table$watershed[.x], "clim"),
                                                   watershed_name = watershed_table$watershed[.x],
                                                   var_name = "pr",
                                                   start_date = "1979-01-01",
                                                   end_date = "2023-12-31",
                                                   data_type = "gridmet",
                                                   replicate = .x)
  return(output)
})


output_list_tmax <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  
  output <- RHESSysWorkflowinR::generate_clim_file(dem_landscape = dem_landscape_list[[.x]],
                                                   clim_grid_table = clim_grid_table_list[[.x]],
                                                   output_folder = file.path("ws", watershed_table$watershed[.x], "clim"),
                                                   watershed_name = watershed_table$watershed[.x],
                                                   var_name = "tmmx",
                                                   start_date = "1979-01-01",
                                                   end_date = "2023-12-31",
                                                   data_type = "gridmet",
                                                   replicate = .x)
  return(output)
})


output_list_tmin <- purrr::map(seq_len(nrow(watershed_table)), function(.x){
  
  output <- RHESSysWorkflowinR::generate_clim_file(dem_landscape = dem_landscape_list[[.x]],
                                                   clim_grid_table = clim_grid_table_list[[.x]],
                                                   output_folder = file.path("ws", watershed_table$watershed[.x], "clim"),
                                                   watershed_name = watershed_table$watershed[.x],
                                                   var_name = "tmmn",
                                                   start_date = "1979-01-01",
                                                   end_date = "2023-12-31",
                                                   data_type = "gridmet",
                                                   replicate = .x)
  return(output)
})



# --------------------------------------------------------------------------
# Call modify_clim_file function

purrr::walk(seq_len(nrow(watershed_table)), function(.x){
  
  print("---------")
  print(paste("Processing", watershed_table$watershed[.x]))
  
  RHESSysWorkflowinR::modify_clim_file(clim_filename = file.path("ws", watershed_table$watershed[.x], "clim", paste0(watershed_table$watershed[.x], "_gridmet_1979_2023")),
                                       clim_type = "grid",
                                       manipulation_type = "shuffle",
                                       period = "year",
                                       out_filename = file.path("ws", watershed_table$watershed[.x], "clim",
                                                                paste0(watershed_table$watershed[.x], "_gridmet_shuffled_", start_year,"_", start_year + shuffle_years - 1)),
                                       duplicate_before = NULL,
                                       duplicate_after = NULL,
                                       shuffle_years = 500,
                                       start_year = 2001,
                                       start_month = 1,
                                       start_day = 1)
})


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Analysis

precip_mean_list <- purrr::map(seq_along(output_list_pr), function(.x){
  precip_mean <- mean(output_list_pr[[.x]]$`1`)*365
  return(precip_mean)
})
names(precip_mean_list) <- watershed_table$watershed

# Note precipitation is in meters


tmax_mean_list <- purrr::map(seq_along(output_list_tmax), function(.x){
  tmax_mean <- mean(output_list_tmax[[.x]]$`1`)
  return(tmax_mean)
})
names(tmax_mean_list) <- watershed_table$watershed



