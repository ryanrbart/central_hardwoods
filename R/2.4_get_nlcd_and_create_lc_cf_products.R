# Import and process spatial terrain data for watersheds

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import list of bbox for gages 2 watersheds and Lower Natural Area watershed

bbox_list <- readr::read_rds("out_r/gis/bbox_of_watersheds/bbox_list.rds")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get nlcd

nlcd_list <- purrr::map(seq_along(bbox_list), function(.x){
  
  # Get template for landcover
  dem <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "dem_landscape.tif"))
  
  nlcd <- RHESSysPreprocessing::download_terrain_variables(bbox = bbox_list[[.x]],
                                                           proj_epsg = watershed_table$projection[.x],
                                                           data_source = "FedData",
                                                           variable = "nlcd",
                                                           label = names(bbox_list)[.x],
                                                           nlcd_year = 2019,
                                                           res_final = NULL,
                                                           res_final_example = dem,
                                                           output_file = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "nlcd_original.tif")
  )
  return(nlcd)
})
names(nlcd_list) <- names(bbox_list)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Simplify nlcd to lc_overstory and lc_understory

lc_overstory_list <- purrr::map(seq_along(nlcd_list), function(.x){
  lc_overstory <- RHESSysWorkflowinR::reclassify_nlcd(nlcd_raster = nlcd_list[[.x]],
                                                      classification = "overstory_5")
  
  # Export land cover files (Nulled because lc_overstory is further processed in next section)
  # terra::writeRaster(lc_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "lc_overstory_landscape.tif"), overwrite=TRUE)
})
names(lc_overstory_list) <- names(bbox_list)


lc_understory_list <- purrr::map(seq_along(nlcd_list), function(.x){
  lc_understory <- RHESSysWorkflowinR::reclassify_nlcd(nlcd_raster = nlcd_list[[.x]],
                                                       classification = "understory_2")
  
  # Export land cover files (Nulled because lc_understory is further processed in next section)
  # terra::writeRaster(lc_understory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "lc_understory_landscape.tif"), overwrite=TRUE)
})
names(lc_understory_list) <- names(bbox_list)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Generate non-nlcd vegetation categories

# The previous section reduced the NLCD categories to deciduous forest,
# evergreen forest, mixed forest, shrub, and grass. This section reclassifies
# those categories to the individual species. Later, this will be reclassified
# again based on Nowacki's product.

lc_overstory_list2 <- purrr::map(seq_along(lc_overstory_list), function(.x){
  
  # Replace these watersheds and species count as necessary
  if (watershed_table$watershed[.x] == "barren"){
    deciduous_sample <- c(101,121,122)
    evergreen_sample <- c(142)                      # No 142 - Shortleaf pine
    mixed_sample <- c(121,122,142)
  }
  if (watershed_table$watershed[.x] != "barren"){
    deciduous_sample <- c(101,102,121,122)
    evergreen_sample <- c(141, 142)                      # No 142 - Shortleaf pine
    mixed_sample <- c(101,102,121,122,141,142)
  }
  
  # ----
  # Change deciduous vegetation
  
  # Make new raster with random sample of new vegetation IDs. Can be weighted
  # by changing number of IDs in sample function.
  lc_overstory_new_veg <- lc_overstory_list[[.x]]
  values(lc_overstory_new_veg) <- sample(deciduous_sample, terra::ncell(lc_overstory_new_veg), replace = TRUE)
  # Swap out values in lc_overstory
  lc_overstory <- terra::ifel(lc_overstory_list[[.x]] == 41, lc_overstory_new_veg, lc_overstory_list[[.x]])

  # ----
  # Change evergreen vegetation
  
  # Make new raster with random sample of new vegetation IDs. Can be weighted
  # by changing number of IDs in sample function.
  lc_overstory_new_veg <- lc_overstory_list[[.x]]
  values(lc_overstory_new_veg) <- sample(evergreen_sample, terra::ncell(lc_overstory_new_veg), replace = TRUE)
  # Swap out values in lc_overstory
  lc_overstory <- terra::ifel(lc_overstory_list[[.x]] == 42, lc_overstory_new_veg, lc_overstory)
  
  # ----
  # Change mixed vegetation
  
  # Make new raster with random sample of new vegetation IDs. Can be weighted
  # by changing number of IDs in sample function.
  lc_overstory_new_veg <- lc_overstory_list[[.x]]
  values(lc_overstory_new_veg) <- sample(mixed_sample, terra::ncell(lc_overstory_new_veg), replace = TRUE)
  # Swap out values in lc_overstory
  lc_overstory <- terra::ifel(lc_overstory_list[[.x]] == 43, lc_overstory_new_veg, lc_overstory)
  
  # Export land cover files 
  terra::writeRaster(lc_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "lc_overstory_landscape.tif"), overwrite=TRUE)
  
  return(lc_overstory)
})
names(lc_overstory_list2) <- names(bbox_list)



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create an understory layer


# Create a single grass understory for both deciduous and evergreen vegetation

lc_understory_list <- purrr::map(seq_along(lc_understory_list), function(.x){
  
  lc_understory <- lc_understory_list[[.x]]
  lc_understory <- terra::ifel(lc_overstory_list[[.x]] %in% c(101,102,121,122), 71, lc_understory)    # Deciduous overstory
  lc_understory <- terra::ifel(lc_overstory_list[[.x]] %in% c(141,142), 71, lc_understory)            # Evergreen overstory
  
  # Export land cover files 
  terra::writeRaster(lc_understory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "lc_understory_landscape.tif"), overwrite=TRUE)
  
  return(lc_understory)
})
names(lc_understory_list) <- names(bbox_list)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create land cover for watersheds with a single vegetation type

# This code is not used. 1) it has bug in it and more importantly 2) it was
# decided to apply a land-cover change script the previously generated file.

# make_single_lc_watershed <- function(lc_overstory_list,
#                                      lc_id,
#                                      output_subname,
#                                      watershed_names){
#   
#   lc_overstory_output_list <- purrr::map(seq_along(lc_overstory_list), function(.x){
#     
#     lc_overstory <- lc_overstory_list[[.x]]
#     lc_overstory <- terra::ifel(lc_overstory_list[[.x]] %in% seq(1,200), lc_id, lc_overstory)
#     
#     # Export land cover files 
#     terra::writeRaster(lc_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0("lc_overstory_", output_subname, "_landscape.tif")), overwrite=TRUE)
#     
#     return(lc_overstory)
#   })
#   names(lc_overstory_output_list) <- names(bbox_list)
# 
#   return(lc_overstory_output_list))
# }
# 
# lc_overstory_output_list <- make_single_lc_watershed(lc_overstory_list = lc_overstory_list,
#                                                      lc_id = 101,
#                                                      output_subname = "redmaple",
#                                                      watershed_names = names(bbox_list)
# 

# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create cover fraction rasters

cf_overstory_list <- purrr::map(seq_along(lc_overstory_list), function(.x){
  
  # Overstory
  cf_overstory <- lc_overstory_list[[.x]]
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 41, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 42, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 43, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 101, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 102, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 121, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 122, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 141, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 142, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 52, 0.9, cf_overstory)
  cf_overstory <- terra::ifel(lc_overstory_list[[.x]] == 71, 0.9, cf_overstory)
  
  # Export land cover files
  terra::writeRaster(cf_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "cf_overstory_landscape.tif"), overwrite=TRUE)
})
names(cf_overstory_list) <- names(bbox_list)


cf_understory_list <- purrr::map(seq_along(lc_understory_list), function(.x){
  
  cf_understory <- lc_understory_list[[.x]]
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 31, 0.1, cf_understory)
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 50, 0.1, cf_understory)
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 53, 0.1, cf_understory)
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 71, 0.1, cf_understory)
  
  # Export land cover files
  terra::writeRaster(cf_understory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "cf_understory_landscape.tif"), overwrite=TRUE)
})
names(cf_understory_list) <- names(bbox_list)


