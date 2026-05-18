# Create single vegetation worldfiles for fire runs

source("R/0_utilities.R")

# Note that modify_worldfile_landcover function is located in 2.16. Has to be
# loaded directly.

# change_value_in_worldfile function can't be used to change veg_parm_id because
# it doesn't have functionality to differentiate by canopy (yet!).

# This approach isn't perfect, as it uses vegetation storages spun up as
# different vegetation types. I should probably apply this step before spinup,
# but given that the spinup I don't do a proper "fire" spinup, would requiresome
# wrangling.


# ------------------------------------------------------------------------------

# redmaple - 101
# whiteoak - 121
# redoak - 122
# shortleafpine - 142

# ------------------------------------------------------------------------------
# Get worldfile table

# Import worldfile_tables as a list
worldfile_table_list <- purrr::map(.x = seq_len(nrow(watershed_table)), .f = \(.x){
  RHESSysWorkflowinR::import_worldfile_table(worldfile_table = file.path("out_r",
                                                                         watershed_table$watershed[.x],
                                                                         paste0("worldfile_table_", watershed_table$watershed[.x], ".csv")))
})
names(worldfile_table_list) <-  watershed_table$watershed


# ------------------------------------------------------------------------------
# Redmaple

# Add make directory script?


# Change worldfile overstory
modify_worldfile_landcover(world_in = "ws/barren/worldfiles/barren_watershedmodern_spinup_fire/barren_watershedmodern_spinup_fire.world",
                           lc_overstory_id = "101",            # Need to be a char
                           lc_understory_id = NULL,
                           target_canopy = 1,
                           lc_scenario = NULL,
                           worldfile_table = worldfile_table_list[[1]],
                           world_out = "ws/barren/worldfiles/barren_watershedredmaple_spinup_fire/barren_watershedredmaple_spinup_fire.world"
)

# Understory should already be 71.

# Change worldfile Fire ID
change_value_in_worldfile(world_in = "ws/barren/worldfiles/barren_watershedredmaple_spinup_fire/barren_watershedredmaple_spinup_fire.world",
                          world_out = "ws/barren/worldfiles/barren_watershedredmaple_spinup_fire/barren_watershedredmaple_spinup_fire.world",
                          change_value = 101,
                          change_loc = "fire_parm_ID",
                          return_data = FALSE,
                          overwrite = TRUE)


# ------------------------------------------------------------------------------
# Whiteoak

# Change worldfile overstory
modify_worldfile_landcover(world_in = "ws/barren/worldfiles/barren_watershedmodern_spinup_fire/barren_watershedmodern_spinup_fire.world",
                           lc_overstory_id = "121",            # Need to be a char
                           lc_understory_id = NULL,
                           target_canopy = 1,
                           lc_scenario = NULL,
                           worldfile_table = worldfile_table_list[[1]],
                           world_out = "ws/barren/worldfiles/barren_watershedwhiteoak_spinup_fire/barren_watershedwhiteoak_spinup_fire.world"
)

# Understory should already be 71.

# Change worldfile Fire ID
change_value_in_worldfile(world_in = "ws/barren/worldfiles/barren_watershedwhiteoak_spinup_fire/barren_watershedwhiteoak_spinup_fire.world",
                          world_out = "ws/barren/worldfiles/barren_watershedwhiteoak_spinup_fire/barren_watershedwhiteoak_spinup_fire.world",
                          change_value = 121,
                          change_loc = "fire_parm_ID",
                          return_data = FALSE,
                          overwrite = TRUE)




# Should we use modify_worldfile_variables from 2.16 here?
