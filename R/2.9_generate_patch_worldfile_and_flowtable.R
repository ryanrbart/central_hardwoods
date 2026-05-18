# Generate patch-level worldfiles and flowtables based on land cover

source("R/0_utilities.R")

options(scipen = 999) # no scientific notation - prevents automatic conversion later


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Generate patch-level files for each watershed

#watershed_site_table <- dplyr::filter(watershed_site_table, site != "watershed")
#watershed_site_table_expanded <-  dplyr::filter(watershed_site_table_expanded, site != "watershed")

watershed_site_table_expanded <-  dplyr::filter(watershed_site_table_expanded,
                                                site %in% c("redmaple","whiteoak", "redoak",
                                                            "shortleafpine", "shrub", "grass"),
                                                watershed == "barren")


# Step through all watershed_sites
purrr::map(seq_len(nrow(watershed_site_table_expanded)), \(.x){
  
  print(paste0("Processing ", watershed_site_table_expanded$watershed[.x], "_", watershed_site_table_expanded$site[.x]))
  
  # Import worldfile_table
  worldfile_table_selected <- RHESSysWorkflowinR::import_worldfile_table(worldfile_table = file.path("out_r", watershed_site_table_expanded$watershed[.x], paste0("worldfile_table_watershedmodern_", watershed_site_table_expanded$watershed[.x], ".csv")))
  
  # Double check that the veg_num in the watershed_site_table_expanded table actually exists in the worlfile
  if (watershed_site_table_expanded$veg_num[.x] %in% unique(worldfile_table_selected$lc_overstory)){
    
    # Identify the patch to represent each vegetation type
    cell_selected <- RHESSysWorkflowinR::identify_patch_based_on_lc_and_median_elevation(worldfile_table=worldfile_table_selected, land_cover_number=watershed_site_table_expanded$veg_num[.x])
    
    # Create the patch-level worldfile
    RHESSysWorkflowinR::generate_patch_level_worldfile_and_flowtable(worldfile_in = file.path("ws", watershed_site_table_expanded$watershed[.x], "worldfiles", 
                                                                                              paste0(watershed_site_table_expanded$watershed[.x], "_watershed.world")),
                                                                     flowtable_in = file.path("ws", watershed_site_table_expanded$watershed[.x], "flowtables", 
                                                                                              paste0(watershed_site_table_expanded$watershed[.x], "_watershed.flow")),
                                                                     worldfile_out = file.path("ws", watershed_site_table_expanded$watershed[.x], "worldfiles", 
                                                                                               paste0(watershed_site_table_expanded$watershed[.x], "_", watershed_site_table_expanded$site[.x], ".world")),
                                                                     flowtable_out = file.path("ws", watershed_site_table_expanded$watershed[.x], "flowtables", 
                                                                                               paste0(watershed_site_table_expanded$watershed[.x], "_", watershed_site_table_expanded$site[.x], ".flow")),
                                                                     basin = cell_selected$basin,
                                                                     hillslope = cell_selected$subbasin,
                                                                     zone = cell_selected$patch,
                                                                     patch = cell_selected$patch)
  }
})


