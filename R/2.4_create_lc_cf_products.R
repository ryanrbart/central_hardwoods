# Import and process spatial terrain data for watersheds

# This script was never used. Replaced by 2.16.

# It was decided to make modified worldfiles based on an established worldfile
# (created in other 2.4 based on NLCD layers), not directly from the rasters.
# Reasoning is that modifications of land cover should by treated as sites, not
# new watersheds. The creation of a watershed from rasters will create a unique
# watershed name under current workflows.


source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Inputs

#period <- "modern"
#period <- "savanna"
period <- "50oakgrass"


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import list of bbox for gages 2 watersheds

bbox_list <- readr::read_rds("out_r/gis/bbox_of_watersheds/bbox_list.rds")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get template for landcover

dem_list <- purrr::map(seq_along(bbox_list), function(.x){
  
  dem <- terra::rast(file.path("out_r", "watershed_processing", watershed_table$watershed[.x], "dem_landscape.tif"))
  
})
names(dem_list) <- names(bbox_list)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Generate preset vegetation categories

# redmaple, 101,
# sugarmaple, 102,
# whiteoak, 121,
# redoak, 122,
# whitepine, 141,
# shortleafpine, 142,
# shrub, 52,
# grass, 71,

# sample(mixed_sample, 100, replace = TRUE, prob = c(.05, .33, .33, .29))

lc_overstory_list <- purrr::map(seq_along(dem_list), function(.x){
  
  if (period == "modern"){
    # Establish vegetation in each watershed
    if (watershed_table$watershed[.x] == "barren"){
      new_sample <- c(101,121,122,142)              # Sugar maple, white oak, red oak, shortleaf pine
      new_prob <- c(0.05,0.33,0.33,0.29)
    }
    if (watershed_table$watershed[.x] != "barren"){
      new_sample <- c(101,121,122,142)              # 
      new_prob <- c(0.05,0.33,0.33,0.29)
    }
  }
  if (period == "50oakgrass"){
    # Establish vegetation in each watershed
    if (watershed_table$watershed[.x] == "barren"){
      new_sample <- c(121,122,71)                   # 25% white oak, 25% red oak, 50% grass
      new_prob <- c(0.25,0.25,0.5)
    }
    if (watershed_table$watershed[.x] != "barren"){
      new_sample <- c(101,121,122,142)
      new_prob <- c(0.05,0.33,0.33,0.29)
    }
  }
  
  # ----
  # Resample to new vegetation probabilities
  
  # Make new raster with random sample of new vegetation IDs. Can be weighted
  # by changing number of IDs in sample function.
  lc_overstory_new_veg <- dem_list[[.x]]
  values(lc_overstory_new_veg) <- sample(new_sample, terra::ncell(lc_overstory_new_veg), replace = TRUE, prob = new_prob)
  # Swap out values in lc_overstory
  lc_overstory <- lc_overstory_new_veg
  
  # ----
  # Export land cover files 
  terra::writeRaster(lc_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0("lc_overstory_", period, "_landscape.tif")), overwrite=TRUE)
  
  return(lc_overstory)
})
names(lc_overstory_list) <- names(bbox_list)



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create an understory layer


# Create a grass understory for both deciduous and evergreen vegetation

lc_understory_list <- purrr::map(seq_along(dem_list), function(.x){
  
  lc_understory <- dem_list[[.x]]
  lc_understory <- terra::ifel(lc_overstory_list[[.x]] %in% c(101,102,121,122), 53, lc_understory)    # Deciduous overstory
  lc_understory <- terra::ifel(lc_overstory_list[[.x]] %in% c(141,142), 53, lc_understory)            # Evergreen overstory
  
  # Export land cover files 
  terra::writeRaster(lc_understory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0("lc_understory_", period, "_landscape.tif")), overwrite=TRUE)
  
  return(lc_understory)
})
names(lc_understory_list) <- names(bbox_list)



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
  terra::writeRaster(cf_overstory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0("cf_overstory_", period, "_landscape.tif")), overwrite=TRUE)
})
names(cf_overstory_list) <- names(bbox_list)


cf_understory_list <- purrr::map(seq_along(lc_understory_list), function(.x){
  
  cf_understory <- lc_understory_list[[.x]]
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 31, 0.1, cf_understory)
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 50, 0.1, cf_understory)
  cf_understory <- terra::ifel(lc_understory_list[[.x]] == 53, 0.1, cf_understory)
  
  # Export land cover files
  terra::writeRaster(cf_understory, filename = file.path("out_r", "watershed_processing", watershed_table$watershed[.x], paste0("cf_understory_", period, "_landscape.tif")), overwrite=TRUE)
})
names(cf_understory_list) <- names(bbox_list)






