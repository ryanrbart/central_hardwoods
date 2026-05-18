# Inputs: Patch-level vegetation spinup

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Assign filter type

filter_table <- tibble::tibble(
  filter_type = "watershed",
  filter_inside_type = "",
  deselect_columns = NA_integer_
)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Filter watershed_site_table

watershed_site_table <- watershed_site_table %>% 
  dplyr::filter(!site %in% c("watershed",
                             "watershedredmaple",
                             "watershedwhiteoak",
                             "watershedredoak",
                             "watershedshortleafpine",
                             "watershedgrass",
                             "watershedmodern"))

watershed_site_table_expanded <- watershed_site_table_expanded %>% 
  dplyr::filter(!site %in% c("watershed",
                             "watershedredmaple",
                             "watershedwhiteoak",
                             "watershedredoak",
                             "watershedshortleafpine",
                             "watershedgrass",
                             "watershedmodern"))


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the watershed level

misc_per_watershed_table <- watershed_table %>% 
  dplyr::mutate(
    # Parameter-set parameters
    parameter_set_replicates = case_when(watershed %in% c("barren") ~ 500,
                                         watershed %in% c("lusk") ~ 10,
                                         watershed %in% c("monday") ~ 10,
                                         watershed %in% c("patoka") ~ 10,
                                         watershed %in% c("potomac") ~ 10),
    def_file_parameters_pathname = file.path("out_r", watershed, "def_file_parameters"),
    def_file_method = "lhc",
    parameter_set_behavioral_record_pathname = file.path("out_r", watershed, "parameter_set_behavioral_record"),
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
    new_parameter_set_behavioral_record = case_when(site == "redmaple" ~ TRUE,
                                                    site == "sugarmaple" ~ FALSE,
                                                    site == "whiteoak" ~ FALSE,
                                                    site == "redoak" ~ FALSE,
                                                    site == "whitepine" ~ FALSE,
                                                    site == "shortleafpine" ~ FALSE,
                                                    site == "shrub" ~ FALSE,
                                                    site == "grass" ~ FALSE),
    write_parameters = case_when(site == "redmaple" ~ TRUE,
                                 site == "sugarmaple" ~ FALSE,
                                 site == "whiteoak" ~ FALSE,
                                 site == "redoak" ~ FALSE,
                                 site == "whitepine" ~ FALSE,
                                 site == "shortleafpine" ~ FALSE,
                                 site == "shrub" ~ FALSE,
                                 site == "grass" ~ FALSE),
    include_prior_parameters = case_when(site == "redmaple" ~ FALSE,
                                         site == "sugarmaple" ~ TRUE,
                                         site == "whiteoak" ~ TRUE,
                                         site == "redoak" ~ TRUE,
                                         site == "whitepine" ~ TRUE,
                                         site == "shortleafpine" ~ TRUE,
                                         site == "shrub" ~ TRUE,
                                         site == "grass" ~ TRUE),
    include_new_parameters = case_when(site == "redmaple" ~ TRUE,
                                       site == "sugarmaple" ~ FALSE,
                                       site == "whiteoak" ~ FALSE,
                                       site == "redoak" ~ FALSE,
                                       site == "whitepine" ~ FALSE,
                                       site == "shortleafpine" ~ FALSE,
                                       site == "shrub" ~ FALSE,
                                       site == "grass" ~ FALSE),
    rename_inputted_worldfile_with_ps_ids = FALSE,
    rename_outputted_worldfile_filename = NA_character_,
    rename_outputted_worldfile_with_ps_ids = FALSE,
    # Soil worldfile change parameters
    worldfile_soilc_initialization_path = file.path("ws", watershed, "worldfiles"),
    worldfile_soilc_initialization_in = paste0(watershed, "_", site, ".world"),
    worldfile_soilc_initialization_out = paste0(watershed, "_", site, "_soilc.world"),
    worldfile_soilc_veg_parm_id = list(c(101,121,122,142,71)),
    worldfile_soilc_initialization_value = list(c(6,6,6,6,3)),
    # Worldfile export parameters
    worldfile_export_pathname = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup_veg.world"))
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
    end = "2020 10 1 1",
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
    output_current_state = "3000 9 30 1"
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

# Notes:
# 1) Only one parameter set needs to be generated per watershed, not one per site.
# 2) Can't build these tables in excel due to nested column lists within dataframe

def_file_path <- watershed_site_table %>% 
  dplyr::mutate(def_file_path = file.path("ws", "defs"))

def_file_hill <- watershed_site_table %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "hill.def",
    gw_loss_coeff = list(c(parameter_set_replicates, 0, 0.4))) %>% 
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_patch <- watershed_site_table %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "patch.def",
    m = list(c(parameter_set_replicates, 0.01, 5)),
    Ksat_0 = list(c(parameter_set_replicates, 0.1, 1000)),
    m_z = list(c(parameter_set_replicates, 0.01, 5)),
    pore_size_index = list(c(parameter_set_replicates, 0.01, 5)),
    psi_air_entry = list(c(parameter_set_replicates, 0.01, 4)),
    sat_to_gw_coeff = list(c(parameter_set_replicates, 0.0, 0.7)),
    soil_depth = list(c(parameter_set_replicates, 0.2, 2))) %>% 
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_redmaple_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_redmaple_1.def",
    epc.storage_transfer_prop = list(c(parameter_set_replicates, 0, 0.6)),
    # epc.branch_turnover = list(c(parameter_set_replicates, 0.005, 0.02)),
    # epc.branch_turnover = list(c(parameter_set_replicates,  0.005, 0.02)),
    # epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.1, 0.8)),
    # epc.livewood_turnover = list(c(parameter_set_replicates, 0.2, 0.6)),
    # epc.livewood_cn = list(c(parameter_set_replicates, 150, 200)),
    # epc.min_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.max_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.proj_sla = list(c(parameter_set_replicates, 4.5, 7)),
    # epc.alloc_stemc_leafc = list(c(parameter_set_replicates, 0.7, 1.1)),
    # epc.leaf_turnover = list(c(parameter_set_replicates, 0.3, 0.45)),
    # epc.flnr = list(c(parameter_set_replicates, 0.26, 0.38)),
    # epc.leaf_cn = list(c(parameter_set_replicates, 60, 90)),
    # epc.froot_cn = list(c(parameter_set_replicates, 50, 80)),
    # epc.gr_perc = list(c(parameter_set_replicates, 0.1, 0.3)),
    # epc.gl_smax = list(c(parameter_set_replicates, 0.001, 0.01)),
    # epc.gl_c = list(c(parameter_set_replicates, 0.00005, 0.0002)),
    # mrc.per_N = list(c(parameter_set_replicates, 0.2, 0.35)),
    # epc.flnr_age_mult = list(c(parameter_set_replicates, 0, 0.5)),
    # epc.max_storage_percent = list(c(parameter_set_replicates, 0.25, 0.25)),
    # epc.min_percent_leafg = list(c(parameter_set_replicates, 1, 1)),
    epc.kfrag_base = list(c(parameter_set_replicates, 0.0003, 0.006))) %>%
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_sugarmaple_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_sugarmaple_1.def",
    epc.storage_transfer_prop = list(c(parameter_set_replicates, 0, 0.6)),
    # epc.branch_turnover = list(c(parameter_set_replicates, 0.005, 0.02)),
    # epc.branch_turnover = list(c(parameter_set_replicates,  0.005, 0.02)),
    # epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.1, 0.8)),
    # epc.livewood_turnover = list(c(parameter_set_replicates, 0.2, 0.6)),
    # epc.livewood_cn = list(c(parameter_set_replicates, 150, 200)),
    # epc.min_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.max_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.proj_sla = list(c(parameter_set_replicates, 4.5, 7)),
    # epc.alloc_stemc_leafc = list(c(parameter_set_replicates, 0.7, 1.1)),
    # epc.leaf_turnover = list(c(parameter_set_replicates, 0.3, 0.45)),
    # epc.flnr = list(c(parameter_set_replicates, 0.26, 0.38)),
    # epc.leaf_cn = list(c(parameter_set_replicates, 60, 90)),
    # epc.froot_cn = list(c(parameter_set_replicates, 50, 80)),
    # epc.gr_perc = list(c(parameter_set_replicates, 0.1, 0.3)),
    # epc.gl_smax = list(c(parameter_set_replicates, 0.001, 0.01)),
    # epc.gl_c = list(c(parameter_set_replicates, 0.00005, 0.0002)),
    # mrc.per_N = list(c(parameter_set_replicates, 0.2, 0.35)),
    # epc.flnr_age_mult = list(c(parameter_set_replicates, 0, 0.5)),
    # epc.max_storage_percent = list(c(parameter_set_replicates, 0.25, 0.25)),
    # epc.min_percent_leafg = list(c(parameter_set_replicates, 1, 1)),
    epc.kfrag_base = list(c(parameter_set_replicates, 0.0003, 0.006))) %>%
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_redoak_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_redoak_1.def",
    epc.storage_transfer_prop = list(c(parameter_set_replicates, 0, 0.6)),
    # epc.branch_turnover = list(c(parameter_set_replicates, 0.005, 0.02)),
    # epc.branch_turnover = list(c(parameter_set_replicates,  0.005, 0.02)),
    # epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.1, 0.8)),
    # epc.livewood_turnover = list(c(parameter_set_replicates, 0.2, 0.6)),
    # epc.livewood_cn = list(c(parameter_set_replicates, 150, 200)),
    # epc.min_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.max_daily_mortality = list(c(parameter_set_replicates,  0.0075, 0.0075)),
    # epc.proj_sla = list(c(parameter_set_replicates, 4.5, 7)),
    # epc.alloc_stemc_leafc = list(c(parameter_set_replicates, 0.7, 1.1)),
    # epc.leaf_turnover = list(c(parameter_set_replicates, 0.3, 0.45)),
    # epc.flnr = list(c(parameter_set_replicates, 0.26, 0.38)),
    # epc.leaf_cn = list(c(parameter_set_replicates, 60, 90)),
    # epc.froot_cn = list(c(parameter_set_replicates, 50, 80)),
    # epc.gr_perc = list(c(parameter_set_replicates, 0.1, 0.3)),
    # epc.gl_smax = list(c(parameter_set_replicates, 0.001, 0.01)),
    # epc.gl_c = list(c(parameter_set_replicates, 0.00005, 0.0002)),
    # mrc.per_N = list(c(parameter_set_replicates, 0.2, 0.35)),
    # epc.flnr_age_mult = list(c(parameter_set_replicates, 0, 0.5)),
    # epc.max_storage_percent = list(c(parameter_set_replicates, 0.25, 0.25)),
    # epc.min_percent_leafg = list(c(parameter_set_replicates, 1, 1)),
    epc.kfrag_base = list(c(parameter_set_replicates, 0.0003, 0.006))) %>%
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_understory_deciduous_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_understory_deciduous_1.def",
    epc.storage_transfer_prop = list(c(parameter_set_replicates, 0, 0.6)),
    # epc.branch_turnover = list(c(parameter_set_replicates, 0.025, 0.05)),
    # epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.6, 0.95)),
    # epc.livewood_turnover = list(c(parameter_set_replicates, 0.1, 0.4)),
    # epc.livewood_cn = list(c(parameter_set_replicates, 100, 150)),
    # epc.min_daily_mortality = list(c(parameter_set_replicates, 0.015, 0.015)),
    # epc.max_daily_mortality = list(c(parameter_set_replicates, 0.015, 0.015)),
    # epc.proj_sla = list(c(parameter_set_replicates, 5, 9)),
    # epc.alloc_stemc_leafc = list(c(parameter_set_replicates, 0.2, 0.7)),
    # epc.leaf_turnover = list(c(parameter_set_replicates, 0.3, 0.6)),
    # epc.flnr = list(c(parameter_set_replicates, 0.02, 0.25)),
    # epc.leaf_cn = list(c(parameter_set_replicates, 60, 90)),
    # epc.froot_cn = list(c(parameter_set_replicates, 50, 80)),
    # mrc.per_N = list(c(parameter_set_replicates, 0.2, 0.35)),
    epc.kfrag_base = list(c(parameter_set_replicates, 0.001, 0.001))) %>%
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_shrub_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_shrub_1.def",
    epc.storage_transfer_prop = list(c(parameter_set_replicates, 0, 0.6)),
    # epc.branch_turnover = list(c(parameter_set_replicates, 0.025, 0.05)),
    epc.branch_turnover = list(c(parameter_set_replicates,  0.06, 0.1)),
    # epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.6, 0.95)),
    epc.alloc_livewoodc_woodc = list(c(parameter_set_replicates, 0.8, 1)),
    epc.livewood_turnover = list(c(parameter_set_replicates, 0.1, 0.4)),
    epc.livewood_cn = list(c(parameter_set_replicates, 100, 150)),
    # epc.min_daily_mortality = list(c(parameter_set_replicates, 0.02, 0.02)),
    # epc.max_daily_mortality = list(c(parameter_set_replicates, 0.02, 0.02)),
    epc.min_daily_mortality = list(c(parameter_set_replicates,  0.02, 0.35)),
    epc.max_daily_mortality = list(c(parameter_set_replicates,  0.02, 0.235)),
    # epc.proj_sla = list(c(parameter_set_replicates, 5, 9)),
    epc.proj_sla = list(c(parameter_set_replicates, 4, 8)),
    epc.alloc_stemc_leafc = list(c(parameter_set_replicates, 0.1, 0.3)),
    epc.leaf_turnover = list(c(parameter_set_replicates, 0.3, 0.6)),
    # epc.flnr = case_when(grass_type == "drought" ~ list(c(parameter_set_replicates, 0.08, 0.25)),
    #                      grass_type == "standard" ~ list(c(parameter_set_replicates, 0.05, 0.1))),
    epc.flnr = list(c(parameter_set_replicates, 0.02, 0.25)),
    epc.leaf_cn = list(c(parameter_set_replicates, 50, 65)),
    epc.froot_cn = list(c(parameter_set_replicates, 50, 80)),
    mrc.per_N = list(c(parameter_set_replicates, 0.2, 0.35)),
    epc.max_storage_percent = list(c(parameter_set_replicates, 0.25, 0.25)),
    epc.min_percent_leafg = list(c(parameter_set_replicates, 1, 1)),
    epc.kfrag_base = list(c(parameter_set_replicates, 0.001, 0.001))) %>% 
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()

def_file_veg_grass_1 <- watershed_site_table_expanded %>% 
  dplyr::select(watershed, site) %>% 
  full_join(., dplyr::select(misc_per_watershed_table, watershed, parameter_set_replicates), by = "watershed") %>% 
  rowwise() %>% 
  dplyr::mutate(
    def_file = "veg_grass_1.def",
    epc.day_leafon = list(c(parameter_set_replicates, 105, 105)),
    epc.day_leafoff = list(c(parameter_set_replicates, 245, 245)),
    epc.ndays_expand = list(c(parameter_set_replicates, 30, 30)),
    epc.ndays_litfall = list(c(parameter_set_replicates, 30, 30)),
    epc.proj_sla = list(c(parameter_set_replicates, 40, 60)),
    epc.gl_smax = list(c(parameter_set_replicates, 0.0002, 0.01)),
    epc.leaf_turnover = list(c(parameter_set_replicates, 0.9, 1)),
    epc.froot_turnover = list(c(parameter_set_replicates, 0.5, 0.8)),
    epc.alloc_frootc_leafc = list(c(parameter_set_replicates, 0.5, 2.5)),
    epc.alloc_prop_day_growth = list(c(parameter_set_replicates, 0.05, 0.2)),
    epc.flnr = list(c(parameter_set_replicates, 0.05, 0.3)),
    epc.leaf_cn = list(c(parameter_set_replicates, 20, 50)),
    epc.froot_cn = list(c(parameter_set_replicates, 40, 75)),
    epc.max_storage_percent = list(c(parameter_set_replicates, 0.25, 0.25)),
    epc.min_percent_leafg = list(c(parameter_set_replicates, 1, 1))) %>% 
  dplyr::select(-parameter_set_replicates) %>% 
  ungroup()


# ----
# Create def file list
def_file_list <- list(
  def_file_path = def_file_path,
  def_file_hill = def_file_hill,
  def_file_patch = def_file_patch,
  def_file_veg_redmaple_1 = def_file_veg_redmaple_1,
  def_file_veg_sugarmaple_1 = def_file_veg_sugarmaple_1,
  def_file_veg_redoak_1 = def_file_veg_redoak_1,
  def_file_veg_understory_deciduous_1 = def_file_veg_understory_deciduous_1,
  def_file_veg_shrub_1 = def_file_veg_shrub_1,
  def_file_veg_grass_1 = def_file_veg_grass_1
)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create output filter table

outputfilter_table <- watershed_site_table %>% 
  dplyr::cross_join(., tibble(spatial_level = c("patch", "stratum"))) %>% 
  dplyr::mutate(
    timestep = "yearly",
    output_format = "csv",
    output_path = file.path("ws", watershed, "out", paste0("spinup_veg_", site)),
    output_filename = paste0("spinup_veg_", site, "_", spatial_level),
    spatial_ID = "1",
    variables = case_when(spatial_level == "patch" ~ "totalc, soilc, nonsoilc=totalc-soilc, litterc, litterc_bg",
                          spatial_level == "stratum" ~ "height, totalc, stemc, leafc, rootc, cpool, cwdc")
  )

# View(outputfilter_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create slurm table

computation_table <- watershed_site_table %>% 
  dplyr::mutate(
    combine_by_linking = list(c("rhessys", "tec", "hdr")),
    combine_by_multiplying = list(c("def")),
    return_cmd = FALSE,
    # General computation
    parallel = TRUE,
    parallel_method = "simple",
    n_cores = 2,
    # Direct Slurm inputs 
    nodes = 2,
    cpus_per_node = 30,
    rscript_path = "/home/rbart/.conda/envs/rhessys/bin/Rscript",
    job_name = paste0("4.1_", watershed, "_", site),
    output = file.path("home", "rbart", "carb_rhessys", "out_slurm", paste0("4.1_", watershed, "_", site)),
    # Slurm 'Options'
    partition = "ARB",
    export = "ALL",
    `mail-user` = "rrbart3@gmail.edu",
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

# To avoid using rowwise, which was extremely slow for evaluation_table (but not
# the def_file tables!), pmap was used for mutates that required a list output
# and variable inputs from other columns. See
# https://community.rstudio.com/t/dplyr-alternatives-to-rowwise/8071


# This table contains the types of analyses to be performed for all watersheds
# or watershed_sites.
evaluation_table_structure <- tribble(
  ~variable, ~timestep, ~spatial_level, ~evaluation_type,
  "height", "yearly", "stratum", "threshold",
  "nonsoilc", "yearly", "patch", "threshold",
  "soilc", "yearly", "patch", "threshold",
  "cwdc", "yearly", "stratum", "threshold",
)

evaluation_table <- watershed_site_table %>%
  dplyr::cross_join(., evaluation_table_structure) %>%
  dplyr::left_join(., spatial_level_canopy_table, by = c("spatial_level"), multiple = "all", relationship = "many-to-many") %>%
  dplyr::relocate(evaluation_type, .after = canopy) %>%
  # Filter any non-needed evaluation measures
  dplyr::filter(variable == "nonsoilc" |
                  variable == "soilc" |
                  variable == "height" |
                  variable == "cwdc") %>%
  # Add new variables
  dplyr::mutate(
    evaluation_yearly_index = "yearly",
    evaluation_period_start = 2000,
    evaluation_period_end = 2020,
    evaluation_timestep = "yearly",
    evaluation_spatial_unit = "patch",
    evaluation_spatial_metric = "mean",
    evaluation_output_folder = file.path("out_r", watershed, "spinup_veg_patch_level"),
    # Objective functions
    objective_function = "rmse",
    objective_function_behavioral_threshold_type = "rank_percent",
    objective_function_behavioral_threshold = 1,
    # Behavioral
    behavioral_process = TRUE,
    # Thresholds
    threshold_min = dplyr::case_when(evaluation_type == "threshold" & variable == "height" & canopy == 1 & 
                                       site %in% c("redmaple", "sugarmaple", "whiteoak", "redoak", "whitepine", "shortleafpine") ~ 18,
                                     evaluation_type == "threshold" & variable == "height" & canopy == 2 & 
                                       site %in% c("redmaple", "sugarmaple", "whiteoak", "redoak", "whitepine", "shortleafpine") ~ -9999,
                                     evaluation_type == "threshold" & variable == "height" & site %in% c("shrub", "grass") ~ -9999,
                                     evaluation_type == "threshold" & variable == "nonsoilc" ~ -9999,
                                     evaluation_type == "threshold" & variable == "soilc" ~ -9999,
                                     evaluation_type == "threshold" & variable == "cwdc" ~ -9999),
    threshold_max = dplyr::case_when(evaluation_type == "threshold" & variable == "height" & canopy == 1 & 
                                       site %in% c("redmaple", "sugarmaple", "whiteoak", "redoak", "whitepine", "shortleafpine") ~ 22,
                                     evaluation_type == "threshold" & variable == "height" & canopy == 2 & 
                                       site %in% c("redmaple", "sugarmaple", "whiteoak", "redoak", "whitepine", "shortleafpine") ~ 9999,
                                     evaluation_type == "threshold" & variable == "height" & site %in% c("shrub", "grass") ~ 9999,
                                     evaluation_type == "threshold" & variable == "nonsoilc" ~ 9999,
                                     evaluation_type == "threshold" & variable == "soilc" ~ 9999,
                                     evaluation_type == "threshold" & variable == "cwdc" ~ 9999),
    threshold_use_all_criteria_for_behavioral = FALSE,
    threshold_override = FALSE,
    # Figures
    figure_process = TRUE,
    figure_type = list(c("time_series_yearly")),
    figure_title = pmap(list(watershed = watershed, site = site, variable = variable, spatial_level = spatial_level, canopy = canopy),
                        \(watershed, site, variable, spatial_level, canopy){c(paste0("Veg spinup (patch level): ", watershed, "_", site, "_", variable, "_", spatial_level, "_", canopy))}),
    figure_label_y = case_when(variable == "height" ~ list(c("Height (m)")),
                               variable == "nonsoilc" ~ list(c("Non-Soilc (Kg/m2)")),
                               variable == "soilc" ~ list(c("Soilc (Kg/m2)")),
                               variable == "cwdc" ~ list(c("CWD Carbon (Kg/m2)"))),
    #figure_highlight_type = list(c("behavioral_only")),
    figure_highlight_type = list(c("regular")),
    figure_output_filename = pmap(list(watershed = watershed, site = site, variable = variable, spatial_level = spatial_level, canopy = canopy),
                                  \(watershed, site, variable, spatial_level, canopy){c(paste0("figure_spinup_", watershed, "_", site, "_", variable, "_", spatial_level, "_", canopy))}),
    figure_consolidation_folder = file.path("out_r", "watersheds_all", "spinup_veg")
  )


