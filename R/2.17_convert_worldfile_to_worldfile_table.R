# Create script to generate a worldfile_table directly from a worldfile.

# The original make_worldfile_table script generates a worldfile_table from the
# input rasters, which often works fine. However, if the worldfile has been
# modified in subsequent steps, the worldfile_table also needs to be updated.

# Although this function creates a worldfile_table from a worldfile, it cannot
# (at this time) produce data for the cell_id, x, y,	col_num, and row_num
# columns. These columns are generated but filled with NAs. Similarly, the
# streams column does not contain data, simply NA's.

# Improvements: 1) Create streams column based on flowtable input. 2) Possibly
# add optional fire_id column. This would probably need to be included in all
# watershed_tables as a variable, not just fire tables.

# This function should be moved to RHESSysWorkflowinR.

source("R/0_utilities.R")

# --------------------------------------------------------------------------
# Function

convert_worldfile_to_worldfile_table <- function(worldfile_path,
                                                 worldfile_table_path){
  
  # Read in worldfile
  world <- RHESSysPreprocessing::read_world(worldfile_path)
  
  # Identify canopy
  world <- world %>%
    dplyr::mutate(canopy = 0) %>% 
    dplyr::mutate(canopy = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d$")) , canopy))
  
  # Create starter worldfile table
  worldfile_table_pixel <- c(
    cell_id = NA,
    x = NA,
    y = NA,
    col_num = NA,
    row_num = NA,
    basin = NA,
    subbasin = NA,
    patch = NA,
    aspect = NA,
    cf_overstory = NA,
    cf_understory = NA,
    clim_grid = NA,
    dem = NA,
    ehr = NA,
    lc_overstory = NA,
    lc_understory = NA,
    slope = NA,
    streams = NA,
    whr = NA
  )
  
  # Create start of worldfile table
  worldfile_table <- tibble::as_tibble_row(worldfile_table_pixel)
  
  # Initialize worldfile table counter
  worldfile_table_row = 1
  
  # Build the worldfile table
  #for (aa in seq(1:2500)){   # testing purposes
  for (aa in seq_len(nrow(world))){
    if (world$vars[aa] == "basin_ID") {worldfile_table_pixel$basin = world$values[aa]}
    if (world$vars[aa] == "hillslope_ID") {worldfile_table_pixel$subbasin = world$values[aa]}
    if (world$vars[aa] == "patch_ID") {worldfile_table_pixel$patch = world$values[aa]}
    if (world$vars[aa] == "aspect") {worldfile_table_pixel$aspect = world$values[aa]}
    if (world$vars[aa] == "cover_fraction" & world$canopy[aa] == 1) {worldfile_table_pixel$cf_overstory = world$values[aa]}
    if (world$vars[aa] == "cover_fraction" & world$canopy[aa] == 2) {worldfile_table_pixel$cf_understory = world$values[aa]}
    if (world$vars[aa] == "z" & world$level[aa] == "patch") {worldfile_table_pixel$dem = world$values[aa]}
    if (world$vars[aa] == "e_horizon") {worldfile_table_pixel$ehr = as.numeric(world$values[aa])*100}
    if (world$vars[aa] == "veg_parm_ID" & world$canopy[aa] == 1) {worldfile_table_pixel$lc_overstory = world$values[aa]}
    if (world$vars[aa] == "veg_parm_ID" & world$canopy[aa] == 2) {worldfile_table_pixel$lc_understory = world$values[aa]}
    if (world$vars[aa] == "slope" & world$level[aa] == "patch") {worldfile_table_pixel$slope = world$values[aa]}    # Assumes that zone and patch slope are equal.
    if (world$vars[aa] == "w_horizon") {worldfile_table_pixel$whr = as.numeric(world$values[aa])*100}
    
    # Assign worldfile_table_pixel to worldfile_table row when a pixel is complete.
    if (world$vars[aa] == "canopy_strata_n_basestations" & world$canopy[aa] == 2) {
      worldfile_table[worldfile_table_row,] <- worldfile_table_pixel
      worldfile_table_row = worldfile_table_row + 1
    }
  }
  
  # Sort worldfile_table by patch
  worldfile_table <- worldfile_table %>% 
    dplyr::arrange(patch)
  
  # Write output
  readr::write_csv(worldfile_table, worldfile_table_path)
  
}


# --------------------------------------------------------------------------
# Get worldfile and example of worldfile_table

# Get worldfile
.x <- 1
#site_name <- "watershedmodern"
site_name <- "watershedredmaple"

world_in <- file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                      paste0(watershed_table$watershed[.x], paste0("_", site_name, ".world")))
worldfile_table_out = file.path("out_r", watershed_table$watershed[.x],
                      paste0("worldfile_table_", site_name, "_", watershed_table$watershed[.x], ".csv"))


convert_worldfile_to_worldfile_table(worldfile_path = world_in,
                                     worldfile_table_path = worldfile_table_out)

