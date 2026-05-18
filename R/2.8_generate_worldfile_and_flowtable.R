# Generate worldfile and flowtables

source("R/0_utilities.R")


# Note that the fire_grid_out argument is set at FALSE in RHESSysPreprocess. The
# error is in RHESSysPreprocessing::write_fire_grids.R line 50. Will placed a
# stop function in code apparently because there was no easy way to replace a sp
# function with a terra or sf function. I will need to either create this new
# function, or put a sp::write.asciigrid in the code, re-adding an sp
# dependency. Remove this note once completed.

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Setup

# Import worldfile_tables as a list
worldfile_table_list <- purrr::map(.x = seq_len(nrow(watershed_table)), .f = \(.x){
  RHESSysWorkflowinR::import_worldfile_table(worldfile_table = file.path("out_r",
                                                                         watershed_table$watershed[.x],
                                                                         paste0("worldfile_table_", watershed_table$watershed[.x], ".csv")))
  
})
names(worldfile_table_list) <-  watershed_table$watershed


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make worldfiles and flowtables

purrr::map(.x = seq_along(worldfile_table_list), .f = \(.x){
  
  print(paste0("------- ", watershed_table$watershed[.x], " -------"))
  
  # Find average watershed latitude
  shed_latitude <- RHESSysWorkflowinR::compute_worldfile_basin_latitude(worldfile_table = worldfile_table_list[[.x]],
                                                                        utm_projection = terra::crs(paste0("epsg:", watershed_table$projection[[.x]])))
  
  # Create new template with correct latitude for each watershed
  RHESSysPreprocessing::update_template(template = "bin/template_gridded",
                                        out_file = file.path("ws", watershed_table$watershed[.x], "bin", "template_gridded"),
                                        vars = "latitude",
                                        values = shed_latitude,
                                        overwrite = TRUE)
  
  # Make worldfiles and flowtables
  RHESSysPreprocessing::RHESSysPreprocess(template = file.path("ws", watershed_table$watershed[.x], "bin", "template_gridded"),
                                          name = file.path("ws", watershed_table$watershed[.x], "worldfiles", paste0(watershed_table$watershed[.x], "_watershed")),
                                          map_dir = file.path("out_r", "watershed_processing", watershed_table$watershed[.x]),
                                          streams = "streams.tif",
                                          overwrite = TRUE,
                                          header = FALSE,
                                          asprules = NULL,
                                          fire_grid_out = TRUE,
                                          convert_aspect = FALSE)
  
  # Move flowtable to flowtables folder
  file.rename(from = file.path("ws", watershed_table$watershed[.x], "worldfiles", paste0(watershed_table$watershed[.x], "_watershed.flow")),
              to = file.path("ws", watershed_table$watershed[.x], "flowtables", paste0(watershed_table$watershed[.x], "_watershed.flow")))
  
  # Move fire grids to auxdata folder
  fire_grid_names <- c("dem", "hillslope", "patch", "zone")
  # Step through each fire grid
  purrr::map(seq_along(fire_grid_names), function(.y){
      file.rename(from = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0(watershed_table$watershed[.x], "_watershed.", fire_grid_names[.y])),
                  to = file.path("ws", watershed_table$watershed[.x], "auxdata", paste0(watershed_table$watershed[.x], "_watershed.", fire_grid_names[.y])))
  })
  
})


