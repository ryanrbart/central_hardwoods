# Inputs: Watershed-level vegetation spinup

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Assign filter type

filter_table <- tibble::tibble(
  filter_type = "watershed_site",
  filter_inside_type = "",
  deselect_columns = NA_integer_
) 


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Filter watershed_site_table

watershed_site_table <- watershed_site_table %>% 
  dplyr::filter(site == "watershedmodern")

watershed_site_table_expanded <- watershed_site_table_expanded %>% 
  dplyr::filter(site == "watershedmodern")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the watershed level

misc_per_watershed_table <- watershed_table %>% 
  dplyr::mutate(
    # Parameter-set parameters
    parameter_set_replicates = case_when(watershed %in% c("barren") ~ 500,
                                         watershed %in% c("lusk") ~ 2,
                                         watershed %in% c("monday") ~ 2,
                                         watershed %in% c("patoka") ~ 2,
                                         watershed %in% c("potomac") ~ 2),
    def_file_parameters_pathname = file.path("out_r", watershed, "def_file_parameters"),
    def_file_method = "lhc",
    parameter_set_behavioral_record_pathname = file.path("out_r", watershed, "parameter_set_behavioral_record"),
    behavioral_repeat_ps = TRUE,
    select_num_behavioral_parameter_sets = 10,
    #select_num_behavioral_parameter_sets = NA_integer_,
    step = 2,
    tec_type = "regular"
  )

# View(misc_per_watershed_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the site level

misc_per_site_table <- watershed_site_table %>% 
  dplyr::mutate(
    # Parameter-set parameters
    new_parameter_set_behavioral_record = TRUE,
    write_parameters = TRUE,
    include_prior_parameters = TRUE,
    include_new_parameters = FALSE,
    rename_inputted_worldfile_with_ps_ids = FALSE,
    rename_outputted_worldfile_filename = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup"), paste0(watershed, "_", site, "_spinup.world")),
    rename_outputted_worldfile_with_ps_ids = TRUE,
    # Worldfile export parameters
    worldfile_export_pathname = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup.world"))
  )

# View(misc_per_site_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the site level

parameters_per_run_table <- watershed_site_table %>% 
  dplyr::mutate(
    nothing_for_now = TRUE
  )


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for rhessys input file

rhessys_table <- watershed_site_table %>% 
  dplyr::mutate(
    version = "bin/rhessys7.4",
    tec_file = file.path("ws", watershed, "tecfiles", "spinup_veg", paste0("spinup_veg_", site, ".tec")),
    world_file = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, ".world")),
    world_hdr_prefix = paste0("spinup_veg_", site),
    flowtable = file.path("ws", watershed, "flowtables", paste0(watershed, "_", site, ".flow")),
    start = "1980 10 1 1",
    end = "2000 10 1 1",
    output_folder = file.path("ws", watershed, "out", paste0("spinup_veg_", site)),
    output_prefix = paste0("spinup_veg_", site),
    outputfilter_name = file.path("ws", watershed, "tecfiles", "spinup_veg", paste0("spinup_veg_", site, ".yml")),
    commandline_options = "-g -asciigrid"
  )

# View(rhessys_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for tec file

tec_table <- watershed_site_table %>% 
  dplyr::mutate(
    print_daily_on = "1980 10 1 1",
    print_monthly_on = "1980 10 1 2",
    print_yearly_on = "1980 10 1 3",
    output_current_state = "2000 9 30 1"
  )

# View(tec_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for hdr file

hdr_table <- watershed_site_table %>% 
  dplyr::mutate(
    def_path = file.path("ws", "defs"),
    clim_path = file.path("ws", watershed, "clim"),
    basin = "basin.def",
    hillslope = "hill.def",
    zone = "zone.def",
    soil = "patch.def",
    landuse = "lu.def",
    stratum = "veg_redmaple_1.def,veg_sugarmaple_1.def,veg_whiteoak_1.def,veg_redoak_1.def,veg_whitepine_1.def,veg_shortleafpine_1.def,veg_understory_evergreen_1.def,veg_understory_deciduous_1.def,veg_shrub_1.def,veg_grass_1.def,veg_nonveg.def",
    basestations = paste0(watershed, "_gridmet_1979_2023.base")
  )

# View(hdr_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create list for def file changes

def_file_list <- NULL


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create output filter table

# outputfilter_table <- watershed_site_table %>% 
#   dplyr::cross_join(., tibble(spatial_level = c("basin", "patch", "stratum"))) %>% 
#   dplyr::mutate(
#     timestep = case_when(spatial_level == "basin" ~ "daily",
#                          spatial_level %in% c("patch", "stratum") ~ "yearly"),
#     output_format = "csv",
#     output_path = file.path("ws", watershed, "out", paste0("spinup_veg_", site)),
#     output_filename = paste0("spinup_veg_", site, "_", spatial_level),
#     spatial_ID = "1",
#     variables = case_when(spatial_level == "basin" ~ "patch.streamflow",
#                           spatial_level == "patch" ~ "totalc, soilc, nonsoilc=totalc-soilc, litterc, litterc_bg",
#                           spatial_level == "stratum" ~ "height, totalc, stemc, leafc, rootc, cpool, cwdc")
#   )

outputfilter_table <- watershed_site_table %>% 
  dplyr::cross_join(., tibble(spatial_level = c("basin"))) %>% 
  dplyr::mutate(
    timestep = "daily",
    output_format = "csv",
    output_path = file.path("ws", watershed, "out", paste0("spinup_veg_", site)),
    output_filename = paste0("spinup_veg_", site, "_", spatial_level),
    spatial_ID = "1",
    variables = "stratum.cs.totalc, patch.soil_cs.totalc"
  )

# View(outputfilter_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create slurm table

computation_table <- watershed_site_table %>% 
  dplyr::mutate(
    combine_by_linking = list(c("rhessys", "tec", "def")),
    combine_by_multiplying = list(c("hdr")),
    return_cmd = FALSE,
    # General computation
    parallel = TRUE,
    parallel_method = "simple",
    n_cores = 2,
    # Direct Slurm inputs 
    nodes = 2,
    cpus_per_node = 30,
    rscript_path = "/home/rbart/.conda/envs/rhessys/bin/Rscript",
    job_name = paste0("6.1_", watershed, "_", site),
    output = file.path("home", "rbart", "carb_rhessys", "out_slurm", paste0("6.1_", watershed, "_", site)),
    # Slurm 'Options'
    partition = "ARB",
    export = "ALL",
    `mail-user` = "rbart3@ucmerced.edu",
    `mail-type_end` = "end",
    `mail-type_fail` = "fail"
  )

# View(computation_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create evaluation table

# This table contains the types of analyses to be performed for all watersheds
# or watershed_sites.
evaluation_table_structure <- tribble(
  ~variable, ~timestep, ~spatial_level, ~evaluation_type,
  "cs.totalc", "daily", "basin", "threshold",
  "nonsoilc", "daily", "basin", "threshold",
  "soil_cs.totalc", "daily", "basin", "threshold",
)


evaluation_table <- watershed_site_table %>% 
  dplyr::cross_join(., evaluation_table_structure) %>% 
  dplyr::left_join(., spatial_level_canopy_table, by = c("spatial_level"), multiple = "all", relationship = "many-to-many") %>% 
  dplyr::relocate(evaluation_type, .after = canopy) %>% 
  dplyr::mutate(
    evaluation_yearly_index = "year",
    evaluation_period_start = case_when(watershed %in% c("barren") ~ 1980,
                                        watershed %in% c("lusk") ~ 1980,
                                        watershed %in% c("monday") ~ 1980,
                                        watershed %in% c("patoka") ~ 1980,
                                        watershed %in% c("potomac") ~ 1980),
    evaluation_period_end = case_when(watershed %in% c("barren") ~ 1981,
                                      watershed %in% c("lusk") ~ 1981,
                                      watershed %in% c("monday") ~ 1981,
                                      watershed %in% c("patoka") ~ 1981,
                                      watershed %in% c("potomac") ~ 1981),
    evaluation_timestep = "yearly",
    evaluation_spatial_unit = "basin",
    evaluation_spatial_metric = "mean",
    evaluation_output_folder = file.path("out_r", watershed, paste0("spinup_veg_", site)),
    # Objective functions
    objective_function = "rmse",
    objective_function_behavioral_threshold_type = "rank_percent",
    objective_function_behavioral_threshold = 0.8,
    # Behavioral
    behavioral_process = TRUE,   # This parameter may be obsolete (2023-11-23)
    # Thresholds
    threshold_min = dplyr::case_when(evaluation_type == "threshold" & variable == "cs.totalc" ~ -9999,
                                     evaluation_type == "threshold" & variable == "nonsoilc" ~ -9999,
                                     evaluation_type == "threshold" & variable == "soil_cs.totalc" ~ -9999),
    threshold_max = dplyr::case_when(evaluation_type == "threshold" & variable == "cs.totalc" ~ 9999,
                                     evaluation_type == "threshold" & variable == "nonsoilc" ~ 9999,
                                     evaluation_type == "threshold" & variable == "soil_cs.totalc" ~ 9999),
    threshold_use_all_criteria_for_behavioral = FALSE,
    threshold_override = FALSE,
    threshold_override_pathname = "out_r/watersheds_all/spinup_tree_carbon_thresholds.csv",
    threshold_override_primary_quantile = 0.95,
    threshold_override_secondary_quantile = 0.75,
    threshold_override_secondary_percentile = NA_real_,
    # Figures
    figure_process = FALSE,
    figure_type = list(c("time_series_yearly")),
    figure_title = pmap(list(watershed = watershed, site = site, variable = variable, spatial_level = spatial_level, canopy = canopy),
                        \(watershed, site, variable, spatial_level, canopy){c(paste0("Veg spinup (patch level): ", watershed, "_", site, "_", variable, "_", spatial_level, "_", canopy))}),
    figure_label_y = case_when(variable == "cs.totalc" ~ list(c("Total C (m)")),
                               variable == "nonsoilc" ~ list(c("Non-Soilc (Kg/m2)")),
                               variable == "soil_cs.totalc" ~ list(c("Soilc (Kg/m2)"))),
    figure_highlight_type = list(c("behavioral_only")),
    figure_output_filename = pmap(list(watershed = watershed, site = site, variable = variable, spatial_level = spatial_level, canopy = canopy),
                                  \(watershed, site, variable, spatial_level, canopy){c(paste0("figure_spinup_", watershed, "_", site, "_", variable, "_", spatial_level, "_", canopy))}),
    figure_consolidation_folder = file.path("out_r", "watersheds_all", "spinup_veg")
  )



