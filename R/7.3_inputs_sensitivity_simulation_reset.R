# Inputs: Parameter and worldfile reset for sensitivity simulations

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
    parameter_set_replicates = 1,
    def_file_parameters_pathname = file.path("out_r", watershed, "def_file_parameters"),
    def_file_parameters_out_pathname = file.path("out_r", watershed, "def_file_parameters_sensitivity_-1"),
    parameter_set_behavioral_record_pathname = file.path("out_r", watershed, "parameter_set_behavioral_record"),
    step = 4,
    tec_type = "regular",
    add_ps_ids = TRUE,
    add_fire_variable = TRUE,
    manual_top_ps = NA_integer_
  )

# View(misc_per_watershed_table)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Create table for misc items that vary at the site level

misc_per_site_table <- watershed_site_table %>% 
  dplyr::mutate(
    # Define old worldfile (without ps ID) and new worldfile. If not moving
    # worldfile, make each value NA_character_.
    worldfile_name_old = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup"), paste0(watershed, "_", site, "_spinup.world")),
    worldfile_name_new = file.path("ws", watershed, "worldfiles", paste0(watershed, "_", site, "_spinup_sensitivity"), paste0(watershed, "_", site, "_spinup_sensitivity.world"))
  )

# View(misc_per_site_table)


