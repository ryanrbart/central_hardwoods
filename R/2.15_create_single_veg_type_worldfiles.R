# Modify the watershed-level worldfile to be a single vegetation type

# This script changes the just the vegetation ID for the overstory and
# understory in an non-spun-up worldfile. The worldfile subsequently needs
# to be spun-up and then substituted as the worldfile for any additional step.

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make the function

make_single_lc_watershed <- function(watershed_table,
                                     lc_overstory_id,            # Need to be a char
                                     lc_understory_id,
                                     site_name){
  
  # Step through all watershed_sites
  worlds <- purrr::map(seq_len(nrow(watershed_table)), \(.x){
    print(paste("Iteration", .x, "out of", nrow(watershed_table)))
    
    # Bring in worldfile
    worldfile_in = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                             paste0(watershed_table$watershed[.x], "_watershed.world"))
    world <- RHESSysPreprocessing::read_world(worldfile_in)
    
    # Identify canopy
    world <- world %>%
      dplyr::mutate(canopy = 0) %>% 
      dplyr::mutate(canopy = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d$")) , canopy))
    
    # Modify worldfile
    world <- world %>%
      dplyr::mutate(values = dplyr::case_when(vars == "veg_parm_ID" & canopy == 1 ~ lc_overstory_id,
                                              vars == "veg_parm_ID" & canopy == 2 ~ lc_understory_id,
                                              .default = as.character(values)))
    
    # Export worldfile
    worldfile_out = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                             paste0(watershed_table$watershed[.x], paste0("_", site_name, ".world")))
    write.table(dplyr::select(world, values, vars), file = worldfile_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="  ")
    
  })
}


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Call the function

# As of 27 February 2025, these watersheds only generated for Barren.
watershed_table <- watershed_table %>% dplyr::filter(watershed == "barren")


make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "101",
                         lc_understory_id = "53",
                         site_name = "watershedredmaple")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "102",
                         lc_understory_id = "53",
                         site_name = "watershedsugarmaple")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "121",
                         lc_understory_id = "53",
                         site_name = "watershedwhiteoak")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "122",
                         lc_understory_id = "53",
                         site_name = "watershedredoak")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "141",
                         lc_understory_id = "53",
                         site_name = "watershedwhitepine")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "142",
                         lc_understory_id = "53",
                         site_name = "watershedshortleafpine")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "52",
                         lc_understory_id = "31",
                         site_name = "watershedshrub")

make_single_lc_watershed(watershed_table = watershed_table,
                         lc_overstory_id = "71",
                         lc_understory_id = "31",
                         site_name = "watershedgrass")




