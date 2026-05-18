# Download DEM

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import list of bbox for gages 2 watersheds and Lower Natural Area watershed

bbox_list <- readr::read_rds("out_r/gis/bbox_of_watersheds/bbox_list.rds")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get dem

dem_list <- purrr::map(seq_along(bbox_list), function(.x){
  
  # Do checks
  if (any(names(bbox_list) != watershed_table$watershed)) {stop(paste("Order of inputs for watershed_table and bbox_list are not the same."))}
  
  dem <- RHESSysPreprocessing::download_terrain_variables(bbox = bbox_list[[.x]],
                                                          proj_epsg = watershed_table$projection[.x],
                                                          data_source = "FedData",
                                                          variable = "dem",
                                                          label = names(bbox_list)[.x],
                                                          res_ned = "1",
                                                          res_final = 90,
                                                          res_final_example = NULL,
                                                          output_file = file.path("out_r",
                                                                                  "watershed_processing",
                                                                                  watershed_table$watershed[.x],
                                                                                  "dem_landscape.tif")
  )
  return(dem)
})
names(dem_list) <- names(bbox_list)


# ----
# Note: 2023-10-20 The 30m DEM underlying Barren is corrupted. The 10 m dem was
# processed instead for Barren

dem_list[[1]] <- RHESSysPreprocessing::download_terrain_variables(bbox = bbox_list[[1]],
                                                                  proj_epsg = watershed_table$projection[1],
                                                                  data_source = "FedData",
                                                                  variable = "dem",
                                                                  label = names(bbox_list)[1],
                                                                  res_ned = "13",
                                                                  res_final = 90,
                                                                  res_final_example = NULL,
                                                                  output_file = file.path("out_r",
                                                                                          "watershed_processing",
                                                                                          watershed_table$watershed[1],
                                                                                          "dem_landscape.tif")
)
names(dem_list)[[1]] <- names(bbox_list)[[1]]


