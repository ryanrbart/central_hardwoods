# Inputs: Watershed-level streamflow calibration

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
    behavioral_repeat_ps = FALSE,
    select_num_behavioral_parameter_sets = 10,
    #select_num_behavioral_parameter_sets = NA_integer_,
    step = 3,
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
    rename_inputted_worldfile_with_ps_ids = TRUE,
    rename_outputted_worldfile_filename = NA_character_,
    rename_outputted_worldfile_with_ps_ids = FALSE,
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
    tec_file = file.path("ws", watershed, "tecfiles", "calibration_streamflow", paste0("calibration_streamflow_", site, ".tec")),
    world_file = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup"), paste0(watershed, "_", site, "_spinup", ".world")),
    world_hdr_prefix = paste0("calibration_streamflow_", site),
    flowtable = file.path("ws", watershed, "flowtables", paste0(watershed, "_", site, ".flow")),
    start = case_when(watershed %in% c("barren") ~ "2015 10 1 1",
                      watershed %in% c("lusk") ~ "2016 10 1 1",
                      watershed %in% c("monday") ~ "2008 10 1 1",
                      watershed %in% c("patoka") ~ "1996 10 1 1",
                      watershed %in% c("potomac") ~ "2016 10 1 1"),
    end = case_when(watershed %in% c("barren") ~ "2022 10 1 1",
                    watershed %in% c("lusk") ~ "2022 10 1 1",
                    watershed %in% c("monday") ~ "2014 10 1 1",
                    watershed %in% c("patoka") ~ "2002 10 1 1",
                    watershed %in% c("potomac") ~ "2022 10 1 1"),
    output_folder = file.path("ws", watershed, "out", paste0("calibration_streamflow_", site)),
    output_prefix = paste0("calibration_streamflow_", site),
    outputfilter_name = file.path("ws", watershed, "tecfiles", "calibration_streamflow", paste0("calibration_streamflow_", site, ".yml")),
    commandline_options = "-g -asciigrid"
  )

# View(rhessys_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for tec file

tec_table <- watershed_site_table %>% 
  dplyr::mutate(
    print_daily_on = case_when(watershed %in% c("barren") ~ "2015 10 1 1",
                               watershed %in% c("lusk") ~ "2016 10 1 1",
                               watershed %in% c("monday") ~ "2008 10 1 1",
                               watershed %in% c("patoka") ~ "1996 10 1 1",
                               watershed %in% c("potomac") ~ "2016 10 1 1"),
    print_monthly_on = case_when(watershed %in% c("barren") ~ "2015 10 1 2",
                               watershed %in% c("lusk") ~ "2016 10 1 2",
                               watershed %in% c("monday") ~ "2008 10 1 2",
                               watershed %in% c("patoka") ~ "1996 10 1 2",
                               watershed %in% c("potomac") ~ "2016 10 1 2"),
    print_yearly_on = case_when(watershed %in% c("barren") ~ "2015 10 1 3",
                               watershed %in% c("lusk") ~ "2016 10 1 3",
                               watershed %in% c("monday") ~ "2008 10 1 3",
                               watershed %in% c("patoka") ~ "1996 10 1 3",
                               watershed %in% c("potomac") ~ "2016 10 1 3"),
    output_current_state = "2050 12 30 1"
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

outputfilter_table <- watershed_site_table %>% 
  dplyr::cross_join(., tibble(spatial_level = c("basin"))) %>% 
  dplyr::mutate(
    timestep = "daily",
    output_format = "csv",
    output_path = file.path("ws", watershed, "out", paste0("calibration_streamflow_", site)),
    output_filename = paste0("calibration_streamflow_", site, "_", spatial_level),
    spatial_ID = "1",
    variables = "patch.streamflow, hill.gw.Qout, stratum.cs.totalc, patch.soil_cs.totalc"
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
    job_name = paste0("7.1_", watershed, "_", site),
    output = file.path("home", "rbart", "carb_rhessys", "out_slurm", paste0("7.1_", watershed, "_", site)),
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

#' @param The evaluation table is complicated 
#' @param threshold_min If using a threshold evaluation, the minimum threshold
#value. If importing values from observed data, these values can be dummy values
#as they will be overwritten.

# Timestep in the evaluation table should match the outputfilter. To change a
# timestep for evaluation, use objective_function_evaluation.

# To avoid using rowwise, which was extremely slow for evaluation_table (but not
# the def_file tables!), pmap was used for mutates that required a list output
# and variable inputs from other columns. See
# https://community.rstudio.com/t/dplyr-alternatives-to-rowwise/8071

# This table contains the types of analyses to be performed for all watersheds
# or watershed_sites.
evaluation_table_structure <- tribble(
  ~variable, ~timestep, ~spatial_level, ~evaluation_type,
  "streamflow", "daily", "basin", "objective_function"
)

evaluation_table <- watershed_site_table %>% 
  dplyr::cross_join(., evaluation_table_structure) %>% 
  dplyr::left_join(., spatial_level_canopy_table, by = c("spatial_level"), multiple = "all", relationship = "many-to-many") %>% 
  dplyr::relocate(evaluation_type, .after = canopy) %>% 
  dplyr::mutate(
    evaluation_yearly_index = "wy",
    evaluation_period_start = case_when(watershed %in% c("barren") ~ 2017,
                                        watershed %in% c("lusk") ~ 2018,
                                        watershed %in% c("monday") ~ 2010,
                                        watershed %in% c("patoka") ~ 1998,
                                        watershed %in% c("potomac") ~ 2018),
    evaluation_period_end = case_when(watershed %in% c("barren") ~ 2022,
                                      watershed %in% c("lusk") ~ 2022,
                                      watershed %in% c("monday") ~ 2014,
                                      watershed %in% c("patoka") ~ 2002,
                                      watershed %in% c("potomac") ~ 2022),
    evaluation_timestep = "daily",
    evaluation_spatial_unit = "patch",
    evaluation_spatial_metric = "mean",
    evaluation_output_folder = file.path("out_r", watershed, paste0("calibration_streamflow_", site)),
    # Objective functions
    objective_function_metric = "kge",
    objective_function_variable = "q_mm",
    objective_function_behavioral_threshold_type = "rank_value",
    objective_function_behavioral_threshold = 1,
    objective_function_observed_data_pathname = file.path("data", "streamflow", "q_central_hardwood.csv"),
    # Behavioral
    behavioral_process = TRUE,
    # Thresholds
    # Figures
    figure_process = TRUE,
    figure_type = list(c("time_series_daily")),
    figure_title = pmap(list(watershed = watershed, site = site, variable = variable, canopy = canopy),
                        \(watershed, site, variable, canopy){c(paste0("Calibration streamflow (watershed level): ", watershed, "_", site, "_", variable, "_", canopy))}),
    figure_label_y = case_when(variable == "streamflow" ~ list(c("Streamflow (mm)"))),
    figure_highlight_type = list(c("behavioral_only")),
    figure_output_filename = pmap(list(watershed = watershed, site = site, variable = variable, canopy = canopy),
                                  \(watershed, site, variable, canopy){c(paste0("figure_calibration_streamflow_", watershed, "_", site, "_", variable, "_", canopy))}),
    figure_consolidation_folder = file.path("out_r", "watersheds_all", "calibration_streamflow")
  )


