# Patch level veg spinup: Call rhessys functions

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Inputs

sites <- tribble(
  ~watershed, ~site_core, ~national_forest, ~state,
  "shed_kansas", FALSE, "none", "kansas",
  "shed_mark_twain", TRUE, "mark_twain", "missouri",
  "shed_shawnee", TRUE, "shawnee", "illinois",
  "shed_hoosier", TRUE, "hoosier", "indiana",
  "shed_wayne", TRUE, "wayne", "ohio",
  "shed_fernow", FALSE, "none", "west_virginia",
)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import data


states <- sf::st_read("data/States/State_boundaries.shp")

streamflow <- sf::st_read("data/Stream_gages/USGS_gages.shp")

national_forests <- sf::st_read("data/National_Forests/National_Forests.shp")

# huc_12 <- sf::st_read("data/HUC_12/HUC_12_watersheds.shp")

fire_history <- sf::st_read("data/Fire_history_sites/Fire_History.shp")

r9_subsections <- sf::st_read("data/subsections/R9_Subsections/R9_Subsections.shp")

subsections_100 <- sf::st_read("data/subsections/subsections_100/subsections_100.shp")

fuel_treatments <- sf::st_read("data/Fuel_treatments/Fuel_treatments.shp")

# Prescribed_fire appears to be a duplicate of Fuel_treatments
# prescribed_fire <- sf::st_read("data/Fuel_treatments/Prescribed_fire.shp")


potomac_project <- sf::st_read("data/Mon_Projects/PotomacProject.shp")

sitlington_greenbriar_project <- sf::st_read("data/Mon_Projects/SitlingtonGreenbriarProject.shp")

# This is a very large file. Unable to plot as of yet.
R9_percent_pyrophilic_simplified <- sf::st_read("data/R9_percent_pyrophilic_simplified/R9_percent_pyrophilic_simplified.shp")

# Mean Fire Interval
mfiarr10f <- terra::rast("data/for_RBart/mfiarr10f.tif")

# Vegetation layers for Monongahela
wv_habitat <- sf::st_read("data/vegetation/WV_terrestrial_habitat_clipped/WV_terrestrial_habitat_clipped.shp")

# Wayne Paeloecology Study Sample Locations
wayne_sample_locations <- sf::st_read("data/paleoecology_sites/Wayne_Paeloecology_Study_Sample_Locations/2023_Paleoecology_Modeling_Study_Sampling_Locations.shp")



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Process data

# Use national forests layer as boundary box?


mfiarr10f_100 <- mfiarr10f %>% 
  tidyterra::filter(mfiarr10f < 100)



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make plots

# This takes a while!!
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = national_forests)

plot(streamflow)

plot(huc_12)



ggplot(huc_12) +
  geom_sf()


ggplot() +
  geom_sf(data=wv_habitat)

unique(wv_habitat$community)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make Kml


st_write(national_forests, "out_r/kml/national_forests.kml")

st_write(streamflow, "out_r/kml/streamflow.kml")

st_write(fire_history, "out_r/kml/fire_history.kml")

st_write(r9_subsections, "out_r/kml/r9_subsections.kml")

st_write(subsections_100, "out_r/kml/subsections_100.kml")

st_write(fuel_treatments, "out_r/kml/fuel_treatments.kml")

st_write(potomac_project, "out_r/kml/potomac_project.kml")

st_write(sitlington_greenbriar_project, "out_r/kml/sitlington_greenbriar_project.kml")

st_write(wayne_sample_locations, "out_r/kml/wayne_paleoecology_locations.kml")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Identify candidate watersheds

watershed_candidates_short <- tribble(
  ~usgs_id, ~watershed, ~national_forest, ~rank, ~q_record,
  01606000, "potomac", "monongahela", 1, "1940-2023", 
  03180500, "greenbrier", "monongahela",  2, "1943-2023",
  03068800, "shavers", "monongahela",  3, "1973-2022",
  01596500, "savage", "monongahela",  NA, "",
  03186500, "williams", "monongahela",  NA, "",
  03065400, "blackwater", "monongahela",  NA, "",
  03205470, "symmes", "wayne", 2, "2000-2022",
  03115400, "little_muskingum", "wayne",  3, "1995-2022",
  03158200, "monday", "wayne",  1, "1997-2015",
  03237280, "upper_twin_creek", "none",  0, "1990-2022",
  03373508, "beaver", "hoosier",  2, "2007-2022",
  03374455, "patoka", "hoosier",  1, "1968-2003",  
  03384450, "lusk", "shawnee",  1, "1967-2022",
  03385000, "hayes", "shawnee",  2, "1949-1975",
  00000000, "lower_natural_area", "twain", 1, "2016-2022",
  07061270, "east_fork_black", "twain",  2, "2003-2022",
  07014000, "huzzah", "twain",  3, "2007-2022", 
  06928300, "roubidoux", "twain",  5, "1999-2022",
  07050152, "roaring", "twain",  4, "2008-2022",
  07309435, "jimmy", "none", 0, "No daily data",
)

# Reference watersheds
watershed_candidates_full <- readr::read_csv("../1_data/gages_2/processed_metadata/gages_2_metadata_allcontiguous_ref.csv") %>% 
  dplyr::left_join(watershed_candidates_short, ., by = c("usgs_id" = "STAID")) %>% 
  dplyr::filter(rank == 1)


# Reference and Non-reference watesheds
# watershed_candidates_full <- readr::read_csv("../1_data/gages_2/processed_metadata/gages_2_metadata_allcontiguous.csv") %>% 
#   mutate(STAID = as.double(STAID)) %>% 
#   dplyr::left_join(watershed_candidates_short, ., by = c("usgs_id" = "STAID"))



# View(dplyr::filter(watershed_candidates_full, watershed == "symmes"))



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# How many patches in the watershed?

# For reference, Tujunga had approximately 20,700 patches.
EcoHydroConversions::calculate_patches_in_watershed(watershed_area_km2 = 168,
                                                    watershed_area_mi2 = NULL,
                                                    patch_side_m = 90)
# ----

patch_side_m = 90
# Patoka
EcoHydroConversions::calculate_patches_in_watershed(watershed_area_km2 = 33,
                                                    watershed_area_mi2 = NULL,
                                                    patch_side_m = patch_side_m)
# Potomac
EcoHydroConversions::calculate_patches_in_watershed(watershed_area_km2 = 805,
                                                    watershed_area_mi2 = NULL,
                                                    patch_side_m = patch_side_m)









