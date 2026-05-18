# Process Simard Pinto canopy height data

source("R/0_utilities.R")

# Simard Pinto is global canopy height dataset at a resolution of 1 km
# (https://doi.org/10.1029/2011JG001708).


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Get data

# Get canopy height data (projection is WGS84 long/lat)
canopy_height_raster <- terra::rast("../1_data/tree_height/simard_pinto/Simard_Pinto_3DGlobalVeg_JGR.tif")

# ----
# Get shapefile boundaries of watersheds

# Change projection to WGS84 to match canopy height data
watershed_boundaries_barren <- st_read("out_r/gis/watershed_shapefiles/basin_barren.shp")
watershed_boundaries_barren <- st_transform(watershed_boundaries_barren, proj_longlat)

watershed_boundaries_lusk <- st_read("out_r/gis/watershed_shapefiles/basin_barren.shp")
watershed_boundaries_lusk <- st_transform(watershed_boundaries_lusk, proj_longlat)

watershed_boundaries_monday <- st_read("out_r/gis/watershed_shapefiles/basin_barren.shp")
watershed_boundaries_monday <- st_transform(watershed_boundaries_monday, proj_longlat)

watershed_boundaries_patoka <- st_read("out_r/gis/watershed_shapefiles/basin_barren.shp")
watershed_boundaries_patoka <- st_transform(watershed_boundaries_patoka, proj_longlat)

watershed_boundaries_potomac <- st_read("out_r/gis/watershed_shapefiles/basin_barren.shp")
watershed_boundaries_potomac <- st_transform(watershed_boundaries_potomac, proj_longlat)

boundary_list <- list(watershed_boundaries_barren,
                      watershed_boundaries_lusk,
                      watershed_boundaries_monday,
                      watershed_boundaries_patoka,
                      watershed_boundaries_potomac)
names(boundary_list) <- watershed_table$watershed_id


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------

sp_height_by_watershed <- RHESSysWorkflowinR::subset_raster_to_dataframe_multi(raster = canopy_height_raster,
                                                                               boundary_list = boundary_list,
                                                                               watershed_id = NULL,
                                                                               buffer = 0.03)


sp_height_summary <- RHESSysWorkflowinR::generate_dataframe_summary(dataframe = sp_height_by_watershed,
                                                                    watershed_table = watershed_table,
                                                                    output_file = "out_r/watersheds_all/simard_pinto_canopy_height_summary.csv",
                                                                    table_type = "quantile",
                                                                    keep_buffer_for_table = FALSE,
                                                                    add_landfire_fri_variables = FALSE)

# The following functions don't work. The first because I no longer pass watershed_boundaries (a
# single shapefile with multiple polygons) to the function. Need to fix.

sp_height_map <- RHESSysWorkflowinR::generate_dataframe_map(dataframe = sp_height_by_watershed,
                                                            watershed_boundaries = watershed_boundaries,
                                                            watershed_table = watershed_table,
                                                            figure_title = "Tree Heights from Simard Pinto",
                                                            legend_name = "Tree\nHeight (m)",
                                                            output_file = "out_r/watersheds_all/simard_pinto_canopy_height_map.pdf",
                                                            keep_buffer_for_map = TRUE,
                                                            add_landfire_fri_variables = FALSE)


sp_height_histogram <- RHESSysWorkflowinR::generate_dataframe_histogram(dataframe = sp_height_by_watershed,
                                                                        watershed_table = watershed_table,
                                                                        figure_title = "Tree Heights from Simard Pinto",
                                                                        output_file = "out_r/watersheds_all/simard_pinto_canopy_height_histogram.pdf",
                                                                        binwidth = 1,
                                                                        keep_buffer_for_hist = FALSE,
                                                                        add_landfire_fri_variables = FALSE)




