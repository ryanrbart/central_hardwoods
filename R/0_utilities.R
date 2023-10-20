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
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(remotes))
suppressPackageStartupMessages(library(RHESSysIOinR))
suppressPackageStartupMessages(library(RHESSysPreprocessing))
suppressPackageStartupMessages(library(RHESSysWorkflowinR))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(terra))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(zoo))


print(paste0("--- Finished with R libraries ---"))

# ---------------------------------------------------------------------
# Variables

# Note: Calibration usually includes passing the watershed or watershed_site
# tables to the functions, since we are just calibrating each watershed or
# watershed_site individually. Scenarios usually have the watershed and
# watershed_site tables combined with additional tables via full join by a
# variable or character().

watershed_table <- tibble::tribble(
  ~watershed, ~watershed_id, ~projection, ~national_forest, ~worldfile_table, ~n_rows, ~n_cols, ~area_km2, ~label, ~stream_threshold, ~gauge_long, ~gauge_lat,
  "beaver", "03373508", 32616, "hoosier", "worldfile_table_beaver", NA, NA, 159.4, "Beaver", 20, -86.74500, 38.67311,
  "lna", "00000000", 32615, "mark_twain", "worldfile_table_lna", NA, NA, 124.2, "Lower Natural Area", 20, -91.06236, 36.84282,
  "lusk", "03384450", 32616, "shawnee", "worldfile_table_lusk", NA, NA, 111.1, "Lusk", 20, -88.547269, 37.472273,
  "monday", "03158200", 32617, "wayne", "worldfile_table_monday", NA, NA, 295, "Monday", 20, -82.191537, 39.435348,
  "patoka", "03374455", 32616, "hoosier", "worldfile_table_patoka", NA, NA, 32.5, "Patoka", 20, -86.387206, 38.444781,  
  "potomac", "01606000", 32617, "monongahela", "worldfile_table_potomac", NA, NA, 805, "North Fork South Branch Potomac", 20, -79.233651, 38.984555,
)

watershed_site_table <- tibble::tribble(
  ~watershed, ~site,
  "potomac", "watershed",
  "potomac", "tree",
  "potomac", "shrub",
  "potomac", "grass",
  "monday", "watershed",
  "monday", "tree",
  "monday", "shrub",
  "monday", "grass",
  "patoka", "watershed",
  "patoka", "tree",
  "patoka", "shrub",
  "patoka", "grass",
  "lusk", "watershed",
  "lusk", "tree",
  "lusk", "shrub",
  "lusk", "grass",
  "lna", "watershed",
  "lna", "tree",
  "lna", "shrub",
  "lna", "grass",
)

site_table <- tibble::tribble(
  ~site, ~veg_num,
  "watershed", 0,
  "tree", 42,
  "shrub", 52,
  "grass", 71,
)

watershed_site_table_expanded <- watershed_site_table %>% 
  dplyr::full_join(., watershed_table, by = "watershed") %>%
  dplyr::full_join(., site_table, by = "site")


# ----

spatial_level <- tibble::tribble(
  ~spatial_level,
  "basin",
  "patch",
  "stratum"
)

fire_replicate <- tibble::tribble(
  ~fire_replicate,
  0,
  1,
  2,
)

period <- tibble::tribble(
  ~period,
  "precolonial",
  "colonial",
)

# # For simulations, generate all possible combinations
# all_scenarios <- watershed_ownership %>% 
#   dplyr::full_join(., ownership_management, by = "ownership") %>% 
#   dplyr::full_join(., management_period, by = "management") %>% 
#   dplyr::full_join(., management_management_replicate, by = "management") %>% 
#   dplyr::full_join(., fire_replicate, by = character()) %>% 
#   dplyr::full_join(., period_gcm_rcp_basestation, by = "period") %>% 
#   mutate(period = factor(period, levels = !!period$period),
#          watershed = factor(watershed, levels  = !!watershed_table$watershed),
#          management = factor(management, levels  = !!management$management))


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


