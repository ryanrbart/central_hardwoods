# Utilities
# Includes variables, libraries, and files/directories


# ---------------------------------------------------------------------

print(paste0("--- Processing utilities script ---"))


# ---------------------------------------------------------------------
# Libraries

suppressPackageStartupMessages(library(caladaptr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(remotes))
suppressPackageStartupMessages(library(RHESSysIOinR))
suppressPackageStartupMessages(library(RHESSysPreprocessing))
suppressPackageStartupMessages(library(RHESSysWorkflowinR))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(zoo))


print(paste0("--- Finished with R libraries ---"))

# ---------------------------------------------------------------------
# Settings

# Prevents xml files from being generated from function terra::writeRaster
setGDALconfig("GDAL_PAM_ENABLED", "FALSE")     # Prevent xml files from being generated


# ---------------------------------------------------------------------
# Variables

# Note: Calibration usually includes passing the watershed or watershed_site
# tables to the functions, since we are just calibrating each watershed or
# watershed_site individually. Scenarios usually have the watershed and
# watershed_site tables combined with additional tables via full join by a
# variable or character().

watershed_table <- tibble::tribble(
  ~watershed, ~watershed_id, ~projection, ~national_forest, ~worldfile_table, ~n_rows, ~n_cols, ~area_km2, ~label, ~stream_threshold, ~gauge_long, ~gauge_lat,
  "barren", "00000000", 32615, "mark_twain", "worldfile_table_barren", 124, 195, 124.2, "Barren", 20, -91.06236, 36.84282,
  #"beaver", "03373508", 32616, "hoosier", "worldfile_table_beaver", NA, NA, 159.4, "Beaver", 20, -86.74500, 38.67311,
  "lusk", "03384450", 32616, "shawnee", "worldfile_table_lusk", 165, 137, 111.1, "Lusk", 20, -88.547269, 37.472273,
  "monday", "03158200", 32617, "wayne", "worldfile_table_monday", 308, 219, 295, "Monday", 20, -82.191537, 39.435348,
  "patoka", "03374455", 32616, "hoosier", "worldfile_table_patoka", 73, 84, 32.5, "Patoka", 20, -86.387206, 38.444781,
  "potomac", "01606000", 32617, "monongahela", "worldfile_table_potomac", 796, 442, 805, "North Fork South Branch Potomac", 20, -79.233651, 38.984555,
)

# Tree species
#	Red oak (Quercus rubra)
#	White oak (Quercus alba)
#	Sugar maple (Acer saccharum)
#	Red maple (Acer rubrum)
#	White pine (Pinus strobus)
#	Shortleaf pine (Pinus echinata)


# Is site really just another way of saying world setup, or maybe just name of worldfile?

watershed_site_table <- tibble::tribble(
  ~watershed, ~site,
  "barren", "watershed",
  "barren", "watershedredmaple",
  #"barren", "watershedsugarmaple",
  "barren", "watershedwhiteoak",
  "barren", "watershedredoak",
  #"barren", "watershedwhitepine",
  "barren", "watershedshortleafpine",
  #"barren", "watershedshrub",
  "barren", "watershedgrass",
  "barren", "watershedmodern",
  "barren", "redmaple",
  #"barren", "sugarmaple",
  "barren", "whiteoak",
  "barren", "redoak",
  #"barren", "whitepine",
  "barren", "shortleafpine",
  #"barren", "shrub",
  "barren", "grass",
  "barren", "watershedredmaplewoodland",
  "barren", "watershedredmapleclosedcanopy",
  "barren", "watershedredoakwoodland",
  "barren", "watershedredoakclosedcanopy",
  "barren", "watershedshortleafpinewoodland",
  "barren", "watershedshortleafpineclosedcanopy",
  "lusk", "watershed",
  "lusk", "watershedredmaple",
  "lusk", "watershedsugarmaple",
  "lusk", "watershedwhiteoak",
  "lusk", "watershedredoak",
  "lusk", "watershedwhitepine",
  #"lusk", "watershedshortleafpine",
  "lusk", "watershedshrub",
  "lusk", "watershedgrass",
  "lusk", "redmaple",
  "lusk", "sugarmaple",
  "lusk", "whiteoak",
  "lusk", "redoak",
  "lusk", "whitepine",
  #"lusk", "shortleafpine",
  "lusk", "shrub",
  "lusk", "grass",
  "monday", "watershed",
  "monday", "watershedredmaple",
  "monday", "watershedsugarmaple",
  "monday", "watershedwhiteoak",
  "monday", "watershedredoak",
  "monday", "watershedwhitepine",
  #"monday", "watershedshortleafpine",
  "monday", "watershedshrub",
  "monday", "watershedgrass",
  "monday", "redmaple",
  "monday", "sugarmaple",
  "monday", "whiteoak",
  "monday", "redoak",
  "monday", "whitepine",
  #"monday", "shortleafpine",
  "monday", "shrub",
  "monday", "grass",
  "patoka", "watershed",
  "patoka", "watershedredmaple",
  "patoka", "watershedsugarmaple",
  "patoka", "watershedwhiteoak",
  "patoka", "watershedredoak",
  "patoka", "watershedwhitepine",
  #"patoka", "watershedshortleafpine",
  "patoka", "watershedshrub",
  "patoka", "watershedgrass",
  "patoka", "redmaple",
  "patoka", "sugarmaple",
  "patoka", "whiteoak",
  "patoka", "redoak",
  "patoka", "whitepine",
  #"patoka", "shortleafpine",
  "patoka", "shrub",
  "patoka", "grass",
  "potomac", "watershed",
  "potomac", "watershedredmaple",
  "potomac", "watershedsugarmaple",
  "potomac", "watershedwhiteoak",
  "potomac", "watershedredoak",
  "potomac", "watershedwhitepine",
  #"potomac", "watershedshortleafpine",
  "potomac", "watershedshrub",
  "potomac", "watershedgrass",
  "potomac", "redmaple",
  "potomac", "sugarmaple",
  "potomac", "whiteoak",
  "potomac", "redoak",
  "potomac", "whitepine",
  #"potomac", "shortleafpine",
  "potomac", "shrub",
  "potomac", "grass",
)


site_veg_table <- tibble::tribble(
  ~site, ~veg_num,
  "watershed", 0,
  "redmaple", 101,
  "sugarmaple", 102,
  "whiteoak", 121,
  "redoak", 122,
  "whitepine", 141,
  "shortleafpine", 142,
  "shrub", 52,
  "grass", 71,
)


veg_veg_num_table <- tibble::tribble(
  ~veg, ~veg_num,
  "redmaple", 101,
  "sugarmaple", 102,
  "whiteoak", 121,
  "redoak", 122,
  "whitepine", 141,
  "shortleafpine", 142,
  "shrub", 52,
  "grass", 71,
)

# The site_veg_table was removed above. May need to be fixed.
watershed_site_table_expanded <- watershed_site_table %>% 
  dplyr::full_join(., watershed_table, by = "watershed") %>%
  dplyr::full_join(., site_veg_table, by = "site")


# ----

spatial_level_table <- tibble::tribble(
  ~spatial_level,
  "basin",
  "patch",
  "stratum"
)

spatial_level_canopy_table <- tibble::tribble(
  ~spatial_level, ~canopy,
  "stratum", 1,
  "stratum", 2,
  "patch", 0,
  "basin", 0,
) 

site_spatial_level_table <- tibble::tribble(
  ~site, ~spatial_level,
  "watershed", "basin",
  "redmaple", "patch",
  "sugarmaple", "patch",
  "whiteoak", "patch",
  "redoak", "patch",
  "whitepine", "patch",
  "shortleafpine", "patch",
  "shrub", "patch",
  "grass", "patch",
)

spatial_level_period_table <- tibble::tribble(
  ~spatial_level, ~period,
  "patch", "spinup_veg_patch",
  "patch", "calibration_veg_patch",
  "basin", "spinup_veg_basin",
  "basin", "calibration_veg_basin",
  "basin", "calibration_streamflow_basin",
)


# ---------------------------------------------------------------------
# Make simulations

ignitions_table <- tibble::tribble(
  ~ignitions,
  "low",
  "high",
)

ps_load_table <- tibble::tribble(
  ~psload,
  "low",
  "medium",
  "high",
)

ps_moisture_table <- tibble::tribble(
  ~psmoisture,
  "low",
  "medium",
  "high",
)

#forest_type_table <- tibble::tribble(
#  ~pft,
#   "grass",
#   "maple_woodland",
#   "maple_closedcanopy",
#   "oak_woodland",
#   "oak_closedcanopy",
#   "pine_woodland",
#   "pine_closedcanopy",
# )
  
all_scenarios_table <- watershed_site_table %>% 
  dplyr::filter(watershed == "barren",
                site %in% c("watershedgrass","watershedredmaplewoodland","watershedredmapleclosedcanopy",
                            "watershedredoakwoodland","watershedredoakclosedcanopy",
                            "watershedshortleafpinewoodland","watershedshortleafpineclosedcanopy")) %>% 
  tidyr::crossing(., ignitions_table) %>% 
  tidyr::crossing(., ps_load_table) %>% 
  tidyr::crossing(., ps_moisture_table)


# Scenarios for barren
# all_scenarios_table %>% dplyr::filter(watershed == "barren")


# ---------------------------------------------------------------------
# Make folders

RHESSysWorkflowinR::create_rhessys_folders(watersheds = watershed_table$watershed, out_slurm = TRUE)


# ---------------------------------------------------------------------
# Projections

# Projection information - Change to using EPSG
# 4326
proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 32610
proj_utm10 <- "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"
# 32611
proj_utm11 <- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs"

# Projection information
# long/lat = 4326
# UTM 10N: 32610 (West Ca)
# UTM 11N: 32611 (East Ca)
# UTM 15N: 32615 (MO)
# UTM 16N: 32616 (IL, IN)
# UTM 17N: 32617 (OH, WV)


# ---------------------------------------------------------------------

print(paste0("--- Finished with utilities script ---"))


