# This is a duplicate of wrangle to be used with central_hardwoods.


source("R/8.1_inputs_calibration_fire.R")

#arguments <- c("inputs_file" = "R/8.1_inputs_calibration_fire.R", "watershed" = "barren", "site" = "watershed")
arguments <- c("inputs_file" = "R/8.1_inputs_calibration_fire.R", "watershed" = "barren", "site" = "watershedredmaple")
#arguments <- c("inputs_file" = "R/8.1_inputs_calibration_fire.R", "watershed" = "barren", "site" = "watershedwhiteoak")


# --------------------------------------------------------------------------
# Get data

# Filter input tables by input arguments.
misc_per_watershed_selected <- RHESSysWorkflowinR::filter_tables(table = misc_per_watershed_table,
                                                                 arguments = arguments,
                                                                 filter_type = "watershed")
evaluation_selected_outside <- RHESSysWorkflowinR::filter_tables(table = evaluation_table,
                                                                 arguments = arguments,
                                                                 filter_type = filter_table$filter_type)
outputfilter_selected_outside <- RHESSysWorkflowinR::filter_tables(table = outputfilter_table,
                                                                   arguments = arguments,
                                                                   filter_type = filter_table$filter_type)


# --------------------------------------------------------------------------
# Determine the number of simulations during step

behavioral_parameter_sets_after_previous_step <- readr::read_csv(paste0(misc_per_watershed_selected$parameter_set_behavioral_record_pathname, "_", misc_per_watershed_selected$step -1, ".csv"), show_col_types = FALSE) %>%
  dplyr::pull(!!sym(paste0("run_step", misc_per_watershed_selected$step - 1)))

# Expected number of behavioral parameter sets based on parameter_set_behavioral_record at previous step
num_behavioral_parameter_sets <- length(behavioral_parameter_sets_after_previous_step[behavioral_parameter_sets_after_previous_step != 0])

# If select_num_behavioral_parameter_sets parameter is set, then use minimum of num_behavioral_parameter_sets and select_num_behavioral_parameter_sets
num_behavioral_parameter_sets <- min(num_behavioral_parameter_sets, misc_per_watershed_selected$select_num_behavioral_parameter_sets, na.rm = TRUE)


# --------------------------------------------------------------------------
# Initialize figure outputs

figure_fire_frequency_spatial <- list()
figure_fire_combined_timeseries_all_watershed <- list()
figure_fire_combined_timeseries_tree_watershed <- list()
figure_fire_combined_timeseries_shrub_watershed <- list()
figure_fire_combined_timeseries_grass_watershed <- list()
figure_fire_combined_timeseries_tree_patch <- list()
figure_fire_effects_timeseries_detailed <- list()
figure_fire_monthly_burn_distibution <- list()
figure_fire_monthly_litter_distibution <- list()
figure_fire_aspect_distibution <- list()
iteration <- 0

# --------------------------------------------------------------------------
# Cycle through all iterations within loop
for (bb in seq_len(nrow(evaluation_selected_outside))){
  
  print("-----------------------------")
  print(paste("Evaluation scenario", bb, "of", nrow(evaluation_selected_outside), ":", arguments["watershed"]))
  print("-----------------------------")
  
  evaluation_selected <- RHESSysWorkflowinR::filter_table_inside_loop(evaluation_table = evaluation_selected_outside,
                                                                      outputfilter_table = outputfilter_selected_outside,
                                                                      number = bb,
                                                                      filter_inside_type = filter_table$filter_inside_type,
                                                                      return_type = "evaluation")
  
  outputfilter_selected <- RHESSysWorkflowinR::filter_table_inside_loop(evaluation_table = evaluation_selected_outside,
                                                                        outputfilter_table = outputfilter_selected_outside,
                                                                        number = bb,
                                                                        filter_inside_type = filter_table$filter_inside_type,
                                                                        return_type = "outputfilter")
  
  
  # --------------------------------------------------------------------------
  
  # Get worldfile table
  worldfile_table <- readr::read_csv(file.path("out_r", misc_per_watershed_selected$watershed, paste0("worldfile_table_", misc_per_watershed_selected$watershed, ".csv")), show_col_types = FALSE)
  
  # Get landfire_fri_summary
  if (!is.na(evaluation_selected$fri_threshold_landfire)){
    landfire_fri_summary_selected <- readr::read_csv(evaluation_selected$fri_threshold_landfire, show_col_types = FALSE) %>%
      dplyr::filter(watershed == .env$arguments["watershed"])
    # Assign landfire values to evaluation_selected values
    evaluation_selected$fri_threshold_min = landfire_fri_summary_selected$FRI_min
    evaluation_selected$fri_threshold_max = landfire_fri_summary_selected$FRI_max
  }
  
  # Import all_combinations_table
  all_combinations_table <- readr::read_csv(file.path(outputfilter_selected$output_path[1], "all_combinations_table.csv"), show_col_types = FALSE) %>%
    dplyr::rename("run" = "...1")
  
  
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # --------------------------------------------------------------------------
  # Loop through and process data
  
  # The reason for using a loop instead of apply is that I want to import
  # results only once but process it in many different ways with many different
  # output objects. Apply only returns a single object. So to use Apply, I would
  # either need a complicated return object, or to import results for each
  # output I want to produce. In this script, there may be a speed hit compared
  # to Apply, but as far as I can tell, probably not a memory hit.
  
  # Regarding memory, ggplot objects appear to include parts of the environment
  # when passed from a function. Thus, since I pass large tables to my figure
  # functions, these tables were being passed back. Solution was to remove these
  # tables from the make_figure functions.
  
  # Note: 1 November 2024 - The following loop appears to be obsolete. 'aa' is
  # only initialized but is never used in any part of this code. 'iteration'
  # is used in the code, though need to check if it is also obsolete.
  
  
  for (aa in seq_len(num_behavioral_parameter_sets)){
    
    print("-----------------------------")
    print(paste("Run", aa, "of", num_behavioral_parameter_sets, ":", arguments["watershed"]))
    print("-----------------------------")
    
    out_basin_daily <- NULL
    out_patch_monthly <- NULL
    out_patch_yearly <- NULL
    out_stratum_monthly <- NULL
    out_stratum_yearly <- NULL
    iteration <- iteration + 1
    
    # Process daily basin output (currently not used for fire outputs)
    if (nrow(dplyr::filter(outputfilter_selected, timestep == "daily", spatial_level == "basin")) > 0){
      outputfilter_selected_basin_daily <- outputfilter_selected %>% dplyr::filter(timestep == "daily", spatial_level == "basin")
      out_basin_daily <- RHESSysWorkflowinR::readin_outputfilter_single(filename = file.path(outputfilter_selected_basin_daily$output_path, paste0(outputfilter_selected_basin_daily$output_filename, "_", iteration, ".csv")),
                                                                        timestep = outputfilter_selected_basin_daily$timestep) %>%
        dplyr::select(-c(basinID)) %>%
        RHESSysWorkflowinR::modify_outputfilter_variables(data = .,
                                                          timestep = outputfilter_selected_basin_daily$timestep,
                                                          spatial_level = outputfilter_selected_basin_daily$spatial_level)
    }
    
    # ----
    # Process monthly patch output
    if (nrow(dplyr::filter(outputfilter_selected, timestep == "monthly", spatial_level == "patch")) > 0){
      outputfilter_selected_patch_monthly <- outputfilter_selected %>% dplyr::filter(timestep == "monthly", spatial_level == "patch")
      out_patch_monthly <- RHESSysWorkflowinR::readin_outputfilter_single(filename = file.path(outputfilter_selected_patch_monthly$output_path, paste0(outputfilter_selected_patch_monthly$output_filename, "_", iteration, ".csv")),
                                                                          timestep = outputfilter_selected_patch_monthly$timestep) %>%
        dplyr::select(-c(basinID, hillID, zoneID)) %>%
        RHESSysWorkflowinR::modify_outputfilter_variables(data = .,
                                                          timestep = outputfilter_selected_patch_monthly$timestep,
                                                          spatial_level = outputfilter_selected_patch_monthly$spatial_level) %>%
        dplyr::left_join(., dplyr::select(worldfile_table, c("patch", "lc_overstory", "aspect", "slope")), by = c("patchID" = "patch"))
    }
    
    # Process yearly patch output
    if (nrow(dplyr::filter(outputfilter_selected, timestep == "yearly", spatial_level == "patch")) > 0){
      outputfilter_selected_patch_yearly <- outputfilter_selected %>% dplyr::filter(timestep == "yearly", spatial_level == "patch")
      out_patch_yearly <- RHESSysWorkflowinR::readin_outputfilter_single(filename = file.path(outputfilter_selected_patch_yearly$output_path, paste0(outputfilter_selected_patch_yearly$output_filename, "_", iteration, ".csv")),
                                                                         timestep = outputfilter_selected_patch_yearly$timestep) %>%
        dplyr::select(-c(basinID, hillID, zoneID)) %>%
        RHESSysWorkflowinR::modify_outputfilter_variables(data = .,
                                                          timestep = outputfilter_selected_patch_yearly$timestep,
                                                          spatial_level = outputfilter_selected_patch_yearly$spatial_level) %>%
        dplyr::left_join(., dplyr::select(worldfile_table, c("patch", "lc_overstory", "aspect", "slope")), by = c("patchID" = "patch"))
    }
    
    # ----
    # Process monthly stratum output
    if (nrow(dplyr::filter(outputfilter_selected, timestep == "monthly", spatial_level == "stratum")) > 0){
      outputfilter_selected_stratum_monthly <- outputfilter_selected %>% dplyr::filter(timestep == "monthly", spatial_level == "stratum")
      out_stratum_monthly <- RHESSysWorkflowinR::readin_outputfilter_single(filename = file.path(outputfilter_selected_stratum_monthly$output_path, paste0(outputfilter_selected_stratum_monthly$output_filename, "_", iteration, ".csv")),
                                                                            timestep = outputfilter_selected_stratum_monthly$timestep) %>%
        dplyr::select(-c(basinID, hillID, zoneID)) %>%
        RHESSysWorkflowinR::modify_outputfilter_variables(data = .,
                                                          timestep = outputfilter_selected_stratum_monthly$timestep,
                                                          spatial_level = outputfilter_selected_stratum_monthly$spatial_level) %>%
        dplyr::left_join(., dplyr::select(worldfile_table, c("patch", "lc_overstory", "aspect", "slope")), by = c("patchID" = "patch"))
    }
    
    # Process yearly stratum output
    if (nrow(dplyr::filter(outputfilter_selected, timestep == "yearly", spatial_level == "stratum")) > 0){
      outputfilter_selected_stratum_yearly <- outputfilter_selected %>% dplyr::filter(timestep == "yearly", spatial_level == "stratum")
      out_stratum_yearly <- RHESSysWorkflowinR::readin_outputfilter_single(filename = file.path(outputfilter_selected_stratum_yearly$output_path, paste0(outputfilter_selected_stratum_yearly$output_filename, "_", iteration, ".csv")),
                                                                           timestep = outputfilter_selected_stratum_yearly$timestep) %>%
        dplyr::select(-c(basinID, hillID, zoneID)) %>%
        RHESSysWorkflowinR::modify_outputfilter_variables(data = .,
                                                          timestep = outputfilter_selected_stratum_yearly$timestep,
                                                          spatial_level = outputfilter_selected_stratum_yearly$spatial_level) %>%
        dplyr::left_join(., dplyr::select(worldfile_table, c("patch", "lc_overstory", "aspect", "slope")), by = c("patchID" = "patch"))
    }
    
    # --------------------------------------------------------------------------
    # Wrangle data
    
    # In theory, the data would be wrangled here and then saved to file. Then I
    # could separate the figures and would not have to import the big files
    # above everytime I want to rerun the figures. However, from an ease
    # standpoint of making figures, it is easier to wrangle the data for each
    # figure immediately before creating the figure. This approach also makes it
    # easier to have a common starting point (eg out_patch, out_stratum) for
    # each figure.
    
    # --------------------------------------------------------------------------
    # Change monthly data to annual, if needed, since this script cannot handle monthly
    
    # This only works for carb simulation outputs, because they were only
    # annual. Need to generalize.
    
    if (!is.null(out_patch_monthly)){
      out_patch_yearly <- out_patch_monthly %>%
        dplyr::group_by(year, patchID, lc_overstory) %>%
        dplyr::summarise(litterc = mean(litterc),
                         burn = sum(burn),
                         fe_litter_c_consumed = sum(fe_litter_c_consumed),
                         fe_soil_c_consumed = sum(fe_soil_c_consumed),
                         .groups = "drop") %>% #,
        #redefine_litterc_loss = sum(redefine_litterc_loss),
        #redefine_soilc_loss = sum(redefine_soilc_loss)) %>%
        dplyr::ungroup() #%>%
      #dplyr::left_join(out_patch_yearly, ., by = c("year", "patchID"))
    }
    
    if (!is.null(out_stratum_monthly)){
      out_stratum_yearly <- out_stratum_monthly %>%
        dplyr::group_by(year, patchID, stratumID, lc_overstory, canopy) %>%
        dplyr::summarise(cwdc = mean(cwdc),
                         totalc = mean(totalc),
                         totalc_consumed = mean(totalc_consumed),
                         totalc_remain = mean(totalc_remain),
                         fe_cwdc_consumed = mean(fe_cwdc_consumed),
                         fe_prop_mort = mean(fe_prop_mort),
                         .groups = "drop")
      
      #redefine_cwdc_loss = sum(redefine_cwdc_loss),
      # #redefine_totalc_harvest = sum(redefine_totalc_harvest),
      # #redefine_totalc_remain = sum(redefine_totalc_remain),
      # #redefine_stemc_harvest = sum(redefine_stemc_harvest)) %>%
      # dplyr::ungroup() %>%
      # #dplyr::left_join(out_stratum_yearly, ., by = c("year", "patchID", "stratumID")) %>%
      # dplyr::mutate(fe_prop_mort = fe_c_mort / totalc,
      #               fe_prop_c_consumed = fe_c_consumed / totalc,
      #               totalc_consumed = fe_prop_c_consumed * totalc,
      #               fe_prop_c_remain = fe_prop_mort - fe_prop_c_consumed,
      #               totalc_remain = fe_prop_c_remain * totalc)
    }
    
    
    # --------------------------------------------------------------------------
    # Make fire figures
    
    # Note: These figures only work for yearly. Need to generalize or
    # conditional calls to yearly or monthly.
    
    # Fire frequency spatial
    figure <- RHESSysWorkflowinR::make_figure_fire_frequency_spatial(
      out_patch = out_patch_yearly,
      worldfile_table = worldfile_table,
      watershed_name = misc_per_watershed_selected$label,
      run = iteration,
      output_folder = NULL)
    figure_fire_frequency_spatial[[iteration]] <- figure
    
    
    # ------------------------------
    # Fire timeseries - all-patches watershed
    figure <- RHESSysWorkflowinR::make_figure_fire_combined_timeseries(
      out_patch = out_patch_yearly,
      out_stratum = out_stratum_yearly,
      watershed_name = misc_per_watershed_selected$label,
      run = iteration,
      land_cover = NULL,
      patch_id = NULL,
      output_folder = NULL)
    figure_fire_combined_timeseries_all_watershed[[iteration]] <- figure
    
    # ------------------------------
    # Monthly burn distribution
    
    figure <- out_patch_monthly %>% 
      dplyr::select(month, year, burn) %>% 
      dplyr::mutate(burn_n = ifelse(burn>0, 1,0)) %>% 
      dplyr::select(month, year, burn_n) %>% 
      dplyr::group_by(month, year) %>% 
      dplyr::summarise(burn_n = sum(burn_n), .groups = "drop") %>% 
      dplyr::group_by(month) %>% 
      dplyr::summarise(burn_n = mean(burn_n), .groups = "drop") %>% 
      ggplot(.) +
      geom_col(aes(x = month, y = burn_n)) +
      NULL
    figure_fire_monthly_burn_distibution[[iteration]] <- figure
    
    # ------------------------------
    # Litter monthly
    
    figure <- out_patch_monthly %>% 
      dplyr::select(month, year, litterc) %>% 
      dplyr::group_by(month) %>% 
      dplyr::summarise(litterc = mean(litterc), .groups = "drop") %>% 
      dplyr::mutate(month = factor(month)) %>% 
      ggplot(.) +
      geom_col(aes(x = month, y = litterc)) +
      NULL
    figure_fire_monthly_litter_distibution[[iteration]] <- figure
    
    # ------------------------------
    # Aspect distribution
    
    # Note: Aspect is calculated as slope orientation in degrees clockwise from
    # north. So southern (i.e. warmest) aspects should be between 90 and 270.
    
    figure <- out_patch_monthly %>% 
      dplyr::select(month, year, burn, aspect) %>% 
      dplyr::mutate(burn_n = ifelse(burn>0, 1,0),
                    aspect_bin = cut(aspect, 
                                     breaks = c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
                                     labels = c("0-45", "45-90", "90-135", "135-180", "180-225", "225-270", "270-315", "315-360"),
                                     include.lowest = FALSE)) %>% 
      dplyr::select(burn_n, aspect_bin) %>% 
      dplyr::group_by(aspect_bin) %>% 
      dplyr::summarise(burn_n = sum(burn_n),
                       aspect_sum = n(),
                       .groups = "drop") %>% 
      dplyr::mutate(burn_percent = burn_n/sum(burn_n)*100,
                    aspect_percent = aspect_sum/sum(aspect_sum)*100) %>% 
      tidyr::pivot_longer(cols = c(burn_percent, aspect_percent), names_to = "variable", values_to = "value") %>% 
    
      ggplot(.) +
      geom_col(aes(x = aspect_bin, y = value, fill = variable), position = position_dodge(width = 0.6), width = 0.6) +
      scale_fill_manual(values = c("aspect_percent" = "steelblue", "burn_percent" = "tomato")) + 
      labs(title = "Percent of area burned by aspect", x = "Aspect", y = "Percent") +
      theme_minimal() +
      NULL
    figure_fire_aspect_distibution[[iteration]] <- figure

    
    # ------------------------------
    # Fire timeseries - tree watershed
    figure <- RHESSysWorkflowinR::make_figure_fire_combined_timeseries(
      out_patch = out_patch_yearly,
      out_stratum = out_stratum_yearly,
      watershed_name = misc_per_watershed_selected$label,
      run = iteration,
      land_cover = 101,     # 101 = red maple
      patch_id = NULL,
      output_folder = NULL)
    figure_fire_combined_timeseries_tree_watershed[[iteration]] <- figure
    
    
    # ------------------------------
    # Fire timeseries - shrub watershed
    # figure <- RHESSysWorkflowinR::make_figure_fire_combined_timeseries(
    #   out_patch = out_patch_yearly,
    #   out_stratum = out_stratum_yearly,
    #   watershed_name = misc_per_watershed_selected$label,
    #   run = iteration,
    #   land_cover = 52,
    #   patch_id = NULL,
    #   output_folder = NULL)
    # figure_fire_combined_timeseries_shrub_watershed[[iteration]] <- figure
    # 
    
    # ------------------------------
    # Fire timeseries - grass watershed
    # figure <- RHESSysWorkflowinR::make_figure_fire_combined_timeseries(
    #   out_patch = out_patch_yearly,
    #   out_stratum = out_stratum_yearly,
    #   watershed_name = misc_per_watershed_selected$label,
    #   run = iteration,
    #   land_cover = 71,
    #   patch_id = NULL,
    #   output_folder = NULL)
    # figure_fire_combined_timeseries_grass_watershed[[iteration]] <- figure
    
    
    # ------------------------------
    # # Fire timeseries - tree patch
    # figure <- RHESSysWorkflowinR::make_figure_fire_combined_timeseries(
    #   out_patch = out_patch_yearly,
    #   out_stratum = out_stratum_yearly,
    #   watershed_name = misc_per_watershed_selected$label,
    #   run = iteration,
    #   land_cover = 42,
    #   patch_id = evaluation_selected$patch_tree,
    #   output_folder = NULL)
    # figure_fire_combined_timeseries_tree_patch[[iteration]] <- figure
    # 
    
    # ------------------------------
    # Fire effects timeseries - boxplot
    # figure <- RHESSysWorkflowinR::make_figure_fire_effects_timeseries_detailed(
    #   out_stratum = out_stratum_yearly,
    #   watershed_name = misc_per_watershed_selected$label,
    #   run = iteration,
    #   output_folder = NULL)
    # figure_fire_effects_timeseries_detailed[[iteration]] <- figure
    
    
    # ------------------------------
    # ------------------------------
    # ------------------------------
    # table_fire_return_interval_summary
    out_table <- RHESSysWorkflowinR::make_table_fire_return_interval_summary(
      out_patch = out_patch_yearly,
      watershed_name = misc_per_watershed_selected$watershed,
      run = iteration,
      land_cover = NULL)
    if(iteration == 1) {  # Initialize table_fire_return_interval_summary
      table_fire_return_interval_summary <- out_table
    } else {
      table_fire_return_interval_summary <- dplyr::bind_rows(table_fire_return_interval_summary, out_table)
    }
    
    
    # ------------------------------
    # table_fire_effects_summary
    out_table <- RHESSysWorkflowinR::make_table_fire_effects_summary(
      out_stratum = out_stratum_yearly,
      watershed_name = misc_per_watershed_selected$watershed,
      run = iteration,
      fe_land_cover = NULL,
      fe_canopy = NULL,
      fe_period_break = evaluation_selected$fe_period_break)
    if(iteration == 1) {  # Initialize table_fire_effects_summary
      table_fire_effects_summary <- out_table
    } else {
      table_fire_effects_summary <- dplyr::bind_rows(table_fire_effects_summary, out_table)
    }
    
    # Print the memory used
    # print(paste("\nObject_name:", lobstr::obj_size(object_name)))
    # print(paste("\nTotal memory:", lobstr::mem_used()))
    
  }
}
# End for loops
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make tables and plots that do not require iterations. Also ones that
# incorporate all_combinations_table.


# ------------------------------
# First process the all_combinations_table
all_combinations_table_fire <- RHESSysWorkflowinR::condense_all_combinations_table(all_combinations_table = all_combinations_table,
                                                                                   watershed = misc_per_watershed_selected$watershed,
                                                                                   def_file = "fire.def")
all_combinations_table_veg_tree_1 <- RHESSysWorkflowinR::condense_all_combinations_table(all_combinations_table = all_combinations_table,
                                                                                         watershed = misc_per_watershed_selected$watershed,
                                                                                         def_file = "veg_tree_1.def")
all_combinations_table_processed <- dplyr::full_join(all_combinations_table_fire, all_combinations_table_veg_tree_1, by = "run")


# ------------------------------
# Make a combined fire spread and fire effects table

# table_fire_behavioral_summary <- RHESSysWorkflowinR::make_table_fire_behavioral_summary(table_fire_return_interval_summary = table_fire_return_interval_summary,
#                                                                                         table_fire_effects_summary = table_fire_effects_summary,
#                                                                                         all_combinations_table_processed = all_combinations_table_processed,
#                                                                                         fire_type = evaluation_selected$fire_type,
#                                                                                         FRI_min = evaluation_selected$fri_threshold_min,
#                                                                                         FRI_max = evaluation_selected$fri_threshold_max,
#                                                                                         fe_land_cover = 42,
#                                                                                         fe_canopy = 1,
#                                                                                         fe_period = "regular",
#                                                                                         fe_threshold_min = evaluation_selected$fe_threshold_min,
#                                                                                         fe_threshold_max = evaluation_selected$fe_threshold_max)
# 
# 
# ------------------------------
# Make dotty plot using parameters from all_combinations_table and the response variable fire_return_interval or fe_prop_mort_50

# Temporarily removing these for runs that did not include overstory_mort_k1. Need to create more flexibility in generating outputs.

# figure_dotty_fire_return_interval <- make_figure_dotty_fire_parameters(all_combinations_table_processed = all_combinations_table_processed,
#                                                                        table_fire_behavioral_summary = table_fire_behavioral_summary,
#                                                                        fire_type = evaluation_selected$fire_type,
#                                                                        response_variable = "fire_return_interval",
#                                                                        watershed = misc_per_watershed_selected$watershed,
#                                                                        output_folder = NULL)
#
# figure_dotty_fire_effects <- make_figure_dotty_fire_parameters(all_combinations_table_processed = all_combinations_table_processed,
#                                                                table_fire_behavioral_summary = table_fire_behavioral_summary,
#                                                                fire_type = evaluation_selected$fire_type,
#                                                                response_variable = "fire_effects",
#                                                                watershed = misc_per_watershed_selected$watershed,
#                                                                output_folder = NULL)


# --------------------------------------------------------------------------
# Create evaluation output folder and parameter_set_behavioral_record folder if they do not exist.

if(!dir.exists(evaluation_selected$evaluation_output_folder)){dir.create(evaluation_selected$evaluation_output_folder)}


# --------------------------------------------------------------------------
# Export figures to pdf

# Fire frequency spatial
print(paste("Exporting figure_fire_frequency_spatial:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_frequency_spatial,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_frequency_spatial.pdf")))

# ----
# Fire timeseries - all-patches watershed
print(paste("Exporting figure_fire_combined_timeseries_all_watershed:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_combined_timeseries_all_watershed,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_combined_timeseries_all_watershed.pdf")))

# Fire timeseries - tree watershed
print(paste("Exporting figure_fire_combined_timeseries_tree_watershed:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_combined_timeseries_tree_watershed,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_combined_timeseries_tree_watershed.pdf")))

# # Fire timeseries - shrub watershed
# print(paste("Exporting figure_fire_combined_timeseries_shrub_watershed:", arguments["watershed"]))
# RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_combined_timeseries_shrub_watershed,
#                                                       input_filename = NULL,
#                                                       output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_combined_timeseries_shrub_watershed.pdf")))
# 
# # Fire timeseries - grass watershed
# print(paste("Exporting figure_fire_combined_timeseries_grass_watershed:", arguments["watershed"]))
# RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_combined_timeseries_grass_watershed,
#                                                       input_filename = NULL,
#                                                       output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_combined_timeseries_grass_watershed.pdf")))
# 
# # Fire timeseries - tree patch
# print(paste("Exporting figure_fire_combined_timeseries_tree_patch:", arguments["watershed"]))
# RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_combined_timeseries_tree_patch,
#                                                       input_filename = NULL,
#                                                       output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_fire_combined_timeseries_tree_patch.pdf")))
# 
# Fire effects - figure_fire_monthly_burn_distibution
print(paste("Exporting figure_fire_effects_timeseries_detailed:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_monthly_burn_distibution,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_figure_fire_monthly_burn_distibution.pdf")))

# Fire effects - figure_fire_monthly_litter_distibution
print(paste("Exporting figure_fire_monthly_litter_distibution:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_monthly_litter_distibution,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_figure_fire_monthly_litter_distibution.pdf")))

# Fire effects - figure_fire_aspect_distibution
print(paste("Exporting figure_fire_aspect_distibution:", arguments["watershed"]))
RHESSysWorkflowinR::consolidate_figures_to_single_pdf(input_list = figure_fire_aspect_distibution,
                                                      input_filename = NULL,
                                                      output_filename = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_figure_fire_aspect_distibution.pdf")))


# ----
# Fire return interval summary table
print(paste("Exporting table_fire_return_interval_summary:", arguments["watershed"]))
readr::write_csv(table_fire_return_interval_summary,
                 file = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$table_output_filename, "_fire_return_interval_summary.csv")))

# Fire effects summary table
print(paste("Exporting table_fire_effects_summary:", arguments["watershed"]))
readr::write_csv(table_fire_effects_summary,
                 file = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$table_output_filename, "_fire_effects_summary.csv")))

# # Fire summary table
# print(paste("Exporting table_fire_behavioral_summary:", arguments["watershed"]))
# readr::write_csv(table_fire_behavioral_summary,
#                  file = file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$table_output_filename, "_fire_behavioral_summary.csv")))
# 
# ----
# # Dotty plot of fire parameters: fire return interval
# print(paste("Exporting figure_dotty_fire_return_interval"))
# ggsave(file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_dotty_fire_return_interval.pdf")), plot=figure_dotty_fire_return_interval, width = 7, height = 8)
#
# # Dotty plot of fire parameters: fire effects
# print(paste("Exporting figure_dotty_fire_effects"))
# ggsave(file.path(evaluation_selected$evaluation_output_folder, paste0(evaluation_selected$figure_output_filename, "_dotty_fire_effects.pdf")), plot=figure_dotty_fire_effects, width = 7, height = 8)

# ----
print(paste("Completed wrangle_and_evaluate_fire:", arguments["watershed"]))


  
