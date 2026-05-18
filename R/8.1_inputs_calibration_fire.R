# Inputs: Watershed-level fire spread and effects calibration

source("R/0_utilities.R")

# Note: When transferring to fire simulations, 1) make sure fire_parm_ID is
# present in worldfile (if gets removed when exporting worldfile from RHESSys
# when wmfire is not turned on), 2) fire and fire_grid_prefix variables are
# referenced in hdr, 3) new parameters (if they need changing) in def file, 4)
# add '-firespread #' to commandline_options, 5) if on cluster, make path to
# libwmfire.so explicit by adding it to LD_LIBRARY_PATH in .bash_profile.

library(lobstr)

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
  #dplyr::filter(site == "watershedmodern")
  dplyr::filter(site == "watershedredmaple")
  #dplyr::filter(site == "watershedwhiteoak")

watershed_site_table_expanded <- watershed_site_table_expanded %>% 
  #dplyr::filter(site == "watershedmodern")
  dplyr::filter(site == "watershedredmaple")
  #dplyr::filter(site == "watershedwhiteoak")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the watershed level

misc_per_watershed_table <- watershed_table %>% 
  dplyr::mutate(
    # Parameter-set parameters
    parameter_set_replicates = 10,
    def_file_parameters_pathname = file.path("out_r", watershed, "def_file_parameters_fire"),
    def_file_method = "lhc",
    parameter_set_behavioral_record_pathname = file.path("out_r", watershed, "parameter_set_behavioral_record_fire"),
    behavioral_repeat_ps = FALSE,
    select_num_behavioral_parameter_sets = NA_integer_,
    step = 1,
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
    include_new_parameters = TRUE,
    rename_inputted_worldfile_with_ps_ids = FALSE,
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
    version = "bin/rhessys7.4_fire",
    tec_file = file.path("ws", watershed, "tecfiles", "calibration_fire", paste0("calibration_fire_", site, ".tec")),
    world_file = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup_fire"), paste0(watershed, "_", site, "_spinup_fire", ".world")),
    world_hdr_prefix = paste0("calibration_fire_", site),
    flowtable = file.path("ws", watershed, "flowtables", paste0(watershed, "_", site, ".flow")),
    start = "1980 1 1 1",
    #end = "2022 1 1 1",
    end = "2000 7 1 1",
    output_folder = file.path("ws", watershed, "out", paste0("calibration_fire_", site)),
    output_prefix = paste0("calibration_fire_", site),
    outputfilter_name = file.path("ws", watershed, "tecfiles", "calibration_fire", paste0("calibration_fire_", site, ".yml")),
    commandline_options = "-g -asciigrid -firespread 90"
  )

# View(rhessys_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for tec file

tec_table <- watershed_site_table %>% 
  dplyr::mutate(
    print_daily_on = "1980 1 1 1",
    print_monthly_on = "1980 1 1 2",
    print_yearly_on = "1980 1 1 3",
    output_current_state = "2200 12 30 1"
  )

# View(tec_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for hdr file

hdr_table <- watershed_site_table %>% 
  dplyr::mutate(
    def_path = file.path("ws", "defs"),
    fire_path = file.path("ws", watershed, "auxdata"),
    clim_path = file.path("ws", watershed, "clim"),
    basin = "basin.def",
    hillslope = "hill.def",
    zone = "zone.def",
    soil = "patch.def",
    landuse = "lu.def",
    stratum = "veg_redmaple_1.def,veg_sugarmaple_1.def,veg_whiteoak_1.def,veg_redoak_1.def,veg_whitepine_1.def,veg_shortleafpine_1.def,veg_understory_evergreen_1.def,veg_understory_deciduous_1.def,veg_shrub_1.def,veg_grass_1.def,veg_nonveg.def",
    #fire = "fire.def,fire_redmaple.def,fire_whiteoak.def,fire_redoak.def,fire_shortleafpine.def",
    fire = "fire_redmaple.def,fire_whiteoak.def,fire_redoak.def,fire_shortleafpine.def",
    fire_grid_prefix = paste0(watershed, "_watershed"),
    basestations = paste0(watershed, "_gridmet_1979_2023.base")
  )

# View(hdr_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create list for def file changes

def_file_path <- watershed_site_table %>% 
  dplyr::mutate(def_file_path = file.path("ws", "defs"))

def_file_fire_redmaple <- watershed_site_table_expanded %>%
  dplyr::select(watershed, site, area_km2, n_rows, n_cols) %>%
  dplyr::full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = c("watershed")) %>%
  rowwise() %>%
  dplyr::mutate(
    #def_file = "fire.def",
    def_file = "fire_redmaple.def",
    #mean_ign = list(c(parameter_set_replicates, 0.8, 3.0)),
    mean_ign = list(c(parameter_set_replicates, 2.0, 3.0)),
    #mean_ign = list(c(parameter_set_replicates, 3.5, 8.0)),
    load_k1 = list(c(parameter_set_replicates, 3, 6)),
    #load_k2 = list(c(parameter_set_replicates, 0.02, 0.1)),
    load_k2 = list(c(parameter_set_replicates, 0.2, 0.3)),
    #load_k2 = list(c(parameter_set_replicates, 0.03, 0.5)),
    moisture_k1 = list(c(parameter_set_replicates, 2, 6)),
    #moisture_k2 = list(c(parameter_set_replicates, 0.005, 0.02)),
    moisture_k2 = list(c(parameter_set_replicates, 0.02, 0.05)),
    #moisture_k2 = list(c(parameter_set_replicates, 0.05, 0.1)),
    moisture_ign_k1 = list(c(parameter_set_replicates, 2, 6)),
    moisture_ign_k2 = list(c(parameter_set_replicates, 0.005, 0.02)),
    n_rows = list(c(parameter_set_replicates, n_rows, n_rows)),
    n_cols = list(c(parameter_set_replicates, n_cols, n_cols)),
    spread_calc_type = list(c(parameter_set_replicates, 9, 9)),
    fire_write = list(c(parameter_set_replicates, 1, 1))
  ) %>%
  dplyr::select(-parameter_set_replicates, -area_km2) %>%
  ungroup()


def_file_veg_redmaple_1 <- watershed_site_table %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_redmaple_1.def",
    consumption = list(c(parameter_set_replicates, 10000, 10000)),
    overstory_mort_k1 = list(c(parameter_set_replicates, -10, -10)),
    overstory_mort_k2 = list(c(parameter_set_replicates, 0.5, 2))
  ) %>% 
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

# Other tree species will use default fire parameters: overstory_mort_k1 = -10
# and overstory_mort_k2 = 1. Add them here when needed.


# ----
# Create def file list
def_file_list <- list(
  def_file_path = def_file_path,
  def_file_fire_redmaple = def_file_fire_redmaple,
  def_file_veg_redmaple_1 = def_file_veg_redmaple_1
)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create output filter table

outputfilter_table <- watershed_site_table %>% 
  dplyr::cross_join(., tibble(spatial_level = c("patch", "stratum"))) %>% 
  dplyr::mutate(
    timestep = "monthly",
    output_format = "csv",
    output_path = file.path("ws", watershed, "out", paste0("calibration_fire_", site)),
    output_filename = paste0("calibration_fire_", site, "_", spatial_level),
    spatial_ID = "1",
    variables = case_when(spatial_level == "patch" ~ "litterc, burn, fe_litter_c_consumed, fe_soil_c_consumed",
                          spatial_level == "stratum" ~ "totalc, leafc, cwdc, height, fe_cwdc_consumed, fe_prop_mort, fe_prop_c_consumed, fe_prop_c_remain, fe_prop_c_remain_adjusted_leafc")
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
    n_cores = 10,
    # Direct Slurm inputs 
    nodes = 4,
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

evaluation_table_structure <- tribble(
  ~figure_process,
  "fire",
)

evaluation_table <- watershed_site_table %>%
  dplyr::cross_join(., evaluation_table_structure) %>%
  dplyr::mutate(
    evaluation_output_folder = file.path("out_r", watershed, paste0("calibration_fire_", site)),
    fire_type = "natural",
    patch_tree = -9999,
    figure_output_filename = paste0("figure_calibration_fire_", watershed),
    fri_threshold_landfire = NA_character_,        # NA_character_ if not using
    fri_threshold_min = NA_integer_,                                                 # NA_integer_ if not using
    fri_threshold_max = NA_integer_,                                                 # NA_integer_ if not using
    fe_period_break = 1985,
    fe_threshold_min = 0.05,
    fe_threshold_max = 0.2
  )



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
