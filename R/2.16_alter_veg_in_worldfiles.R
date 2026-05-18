# Modify the watershed-level worldfile


# Functions included in this script
# - assign_land_cover
# - modify_worldfile_landcover
# - modify_worldfile_variables



# Note: After using this script, the worldfile_table may be inaccurate. This
# script does not update worldfile_table. See 2.17.

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make the functions

# Function for assigning land cover based on probabilities.

assign_land_cover <- function(df,
                              lc_ids,                     # e.g. c(101,121,122,142)
                              lc_prob,                    # target global probabilities
                              restriction_list = NULL,    # nested lists of rules per ID
                              target_canopy,
                              vars_col   = "vars",
                              value_col  = "values",
                              target_var = "veg_parm_ID"){
  
  stopifnot(length(lc_ids) == length(lc_prob))
  stopifnot(abs(sum(lc_prob) - 1) < 1e-6)
  
  # Which rows do we assign?
  idx <- df[[vars_col]] == target_var & df$canopy == target_canopy
  subdf <- df[idx, ]
  n_total <- nrow(subdf)
  
  # Probability matrix: each column is a cell, each row is an ID
  prob_mat <- matrix(rep(lc_prob, n_total), nrow = length(lc_ids))
  rownames(prob_mat) <- lc_ids
  
  # ---- APPLY HIERARCHICAL CONSTRAINTS ----
  if (!is.null(restriction_list)) {
    for (id_name in names(restriction_list)) {
      rid <- as.numeric(id_name)
      if (!(rid %in% lc_ids))
        stop(paste("ID", rid, "is in restriction_list but not in lc_ids."))
      
      id_rules <- restriction_list[[id_name]]
      
      # Compute allowed mask from hierarchical constraints
      # Each rule is ANDed together
      allowed_mask <- rep(TRUE, n_total)
      for (rule_fn in id_rules) {
        rule_result <- rule_fn(subdf)
        if (!is.logical(rule_result) || length(rule_result) != n_total) {
          stop(paste("Restriction for ID", rid, "must return a logical vector of length", n_total))
        }
        allowed_mask <- allowed_mask & rule_result
      }
      
      if (!any(allowed_mask))
        stop(paste("ID", rid, "has hierarchical rules but no valid locations."))
      
      # ID index
      r <- which(lc_ids == rid)
      
      # Target global
      target_global <- lc_prob[r]
      expected_count <- target_global * n_total
      
      # Probability inside allowed area
      n_allowed <- sum(allowed_mask)
      p_allowed <- expected_count / n_allowed
      p_allowed <- min(p_allowed, 1)
      
      # Update probabilities
      prob_mat[r, allowed_mask] <- p_allowed
      prob_mat[r, !allowed_mask] <- 0
      
      # Renormalize other IDs within allowed / not allowed
      other_rows <- setdiff(seq_along(lc_ids), r)
      
      # Allowed subset
      remaining_allowed <- 1 - p_allowed
      raw_other <- lc_prob[other_rows] / sum(lc_prob[other_rows])
      
      prob_mat[other_rows, allowed_mask] <-
        raw_other * matrix(remaining_allowed,
                           nrow = length(other_rows),
                           ncol = sum(allowed_mask),
                           byrow = TRUE)
      
      # Not allowed subset: full mass goes to other IDs
      prob_mat[other_rows, !allowed_mask] <-
        raw_other * matrix(1,
                           nrow = length(other_rows),
                           ncol = sum(!allowed_mask),
                           byrow = TRUE)
    }
  }
  
  # Final column-normalization
  prob_mat <- prob_mat / matrix(colSums(prob_mat),
                                nrow = length(lc_ids),
                                ncol = n_total,
                                byrow = TRUE)
  
  # ---- VECTOR SAMPLING ----
  cum_prob <- apply(prob_mat, 2, cumsum)
  u <- runif(n_total)
  
  sampled <- apply(cum_prob > rep(u, each = length(lc_ids)), 2, function(col) {
    lc_ids[which(col)[1]]
  })
  
  # Insert into df
  df$new_value <- df[[value_col]]
  df$new_value[idx] <- sampled
  df <- df %>%
    mutate(!!value_col := new_value) %>%
    select(-new_value)
  
  return(df)
}

# Example restriction list
# restriction_list <- list(
#   "101" = list(
#     function(d) d$aspect >= 315 | d$aspect <= 90,
#     function(d) d$slope > 0,
#     function(d) d$dem > 0
#   ),
#   "102" = list(
#     function(d) d$aspect >= 90
#   )
# )


# --------------------------------------------------------------------------
# Function for converting land cover within worldfile

# Function for selecting scenario to use when converting land cover within worldfile
modify_worldfile_landcover <- function(world_in,
                                       lc_overstory_id = NULL,            # Need to be a char
                                       lc_understory_id = NULL,
                                       target_canopy,
                                       lc_scenario = NULL,
                                       worldfile_table = NULL,
                                       world_out){
  
  # Bring in worldfile
  world <- RHESSysPreprocessing::read_world(world_in)
  
  # Identify canopy and patch associated with canopy_stratum
  world <- world %>%
    dplyr::mutate(canopy = 0) %>% 
    dplyr::mutate(canopy = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d$")) , canopy)) %>% 
    dplyr::mutate(patch = 0) %>% 
    dplyr::mutate(patch = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d+(?=\\d$)")) , patch))
  
  # Change the worldfile
  if (!is.null(lc_overstory_id)){
    # Modify worldfile to single vegetation type (if applicable)
    print("Time to change the overstory!")
    world <- world %>%
      dplyr::mutate(values = dplyr::case_when(vars == "veg_parm_ID" & canopy == 1 ~ lc_overstory_id,
                                              .default = as.character(values)))
  }
  if (!is.null(lc_understory_id)){
    # Modify worldfile to single vegetation type (if applicable)
    print("Time to change the understory!")
    world <- world %>%
      dplyr::mutate(values = dplyr::case_when(vars == "veg_parm_ID" & canopy == 2 ~ lc_understory_id,
                                              .default = as.character(values)))
  }
  if (is.null(lc_overstory_id) & is.null(lc_understory_id)){
    # Modify worldfile to scenarios (if applicable)
    if (lc_scenario == "barren_modern"){
      print("Time for Barren Modern!")
      
      # Get variables from worldfile_table
      world <- world %>% 
        dplyr::left_join(., dplyr::select(worldfile_table, patch, dem, aspect, slope), by = "patch")
      
      # Set new land covers and probabilities (and optional aspect)
      lc_ids <- c(101,121,122,142)              # Sugar maple, white oak, red oak, shortleaf pine
      lc_prob <- c(0.05,0.33,0.33,0.29)
      restriction_list <- list(
        "101" = list(
          function(d) d$aspect >= 315 | d$aspect <= 135
        )
      )
      
      world <- assign_land_cover(
        df = world,
        lc_ids = lc_ids,                   # e.g. c(101,121,122,142)
        lc_prob = lc_prob,             # target global probabilities
        restriction_list = restriction_list,    # nested lists of rules per ID
        target_canopy = 1,
        target_var = "veg_parm_ID"
      )
    }
    
    if (lc_scenario == "50oakgrass"){
      print("Time for 50% oak, 50% grass!")
      
      # Get variables from worldfile_table
      world <- world %>% 
        dplyr::left_join(., dplyr::select(worldfile_table, patch, dem, aspect, slope), by = "patch")
      
      # Set new land covers and probabilities (and optional aspect)
      lc_ids <- c(121,122,71)              # Sugar maple, white oak, red oak, shortleaf pine
      lc_prob <- c(0.25,0.25,0.50)
      restriction_list <- NULL
      
      world <- assign_land_cover(
        df = world,
        lc_ids = lc_ids,                   # e.g. c(101,121,122,142)
        lc_prob = lc_prob,             # target global probabilities
        restriction_list = restriction_list,    # nested lists of rules per ID
        target_canopy = 1,
        target_var = "veg_parm_ID"
      )
    }
    
    if (lc_scenario == "nothing"){
      "Do nothing"
    }
  }
  
  # Export worldfile
  write.table(dplyr::select(world, values, vars), file = world_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="  ")
  print("Writing Worldfile")
}



# --------------------------------------------------------------------------
# Function for modifying stratum variables

modify_worldfile_variables <- function(world_in,
                                       lc_type,
                                       target_variables,
                                       target_values,
                                       target_canopy,
                                       world_out){
  
  # Bring in worldfile
  world <- RHESSysPreprocessing::read_world(world_in)
  
  # Identify canopy and patch
  world <- world %>%
    dplyr::mutate(canopy = 0) %>% 
    dplyr::mutate(canopy = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d$")) , canopy)) %>% 
    dplyr::mutate(patch = 0) %>% 
    dplyr::mutate(patch = dplyr::if_else(level == "canopy_strata", as.numeric(stringr::str_extract(ID, "\\d+(?=\\d$)")) , patch))
  
  target_var <- "nothing"
  target_val <- 1
  target_can <- 0

  # Search worldfile from top to bottom.
  for (aa in seq_len(nrow(world))){
    if (aa%%10000 == 0){print(paste("Worldfile line", aa, "of", nrow(world)))} # Counter
    
    # Check if passing a veg ID
    if (world$vars[aa] == "veg_parm_ID"){
      # Check if the veg ID is equal to one the veg ID that is inputted to be changed.
      if (world$values[aa] %in% lc_type){
        
        # If yes, find the position number of lc_type so that you know which targets to choose.
        lc_pos <- match(world$values[aa], lc_type)
        
        # Determine which value within input vectors to use
        target_var <- target_variables[lc_pos]
        target_val <- target_values[lc_pos]
        target_can <- target_canopy[lc_pos]
      }
    }
    
    if (world$vars[aa] == target_var & world$canopy[aa] == target_can){world$values[aa] = target_val}
  }
  
  # Export worldfile
  world$values <- format(world$values, scientific = FALSE)
  write.table(dplyr::select(world, values, vars), file = world_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="  ")
  print("Writing Worldfile")
}


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Run function

# Import worldfile_tables as a list
worldfile_table_list <- purrr::map(.x = seq_len(nrow(watershed_table)), .f = \(.x){
  RHESSysWorkflowinR::import_worldfile_table(worldfile_table = file.path("out_r",
                                                                         watershed_table$watershed[.x],
                                                                         paste0("worldfile_table_", watershed_table$watershed[.x], ".csv")))
})
names(worldfile_table_list) <-  watershed_table$watershed


# Step through all watershed_sites
worlds <- purrr::map(seq_len(nrow(watershed_table)), \(.x){
  print(paste("Iteration", .x, "out of", nrow(watershed_table)))
  
  lc_scenario <- c("barren_modern", "nothing", "nothing", "nothing", "nothing", "nothing")
  site_name <- c("watershedmodern", "nothing", "nothing", "nothing", "nothing", "nothing")
  
  modify_worldfile_landcover(world_in = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                  paste0(watershed_table$watershed[.x], "_watershed.world")),
                             lc_overstory_id = NULL,            # Need to be a char
                             lc_understory_id = NULL,
                             target_canopy = 1,
                             lc_scenario = lc_scenario[.x],
                             worldfile_table = worldfile_table_list[[.x]],
                             world_out = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                   paste0(watershed_table$watershed[.x], paste0("_", site_name[.x], ".world")))
  )
  
  modify_worldfile_landcover(world_in = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                  paste0(watershed_table$watershed[.x], paste0("_", site_name[.x], ".world"))),
                             lc_overstory_id = NULL,            # Need to be a char
                             lc_understory_id = "71",
                             target_canopy = 2,
                             lc_scenario = NULL,
                             worldfile_table = worldfile_table_list[[.x]],
                             world_out = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                   paste0(watershed_table$watershed[.x], paste0("_", site_name[.x], ".world")))
  )
  
  
  
  modify_worldfile_variables(world_in = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                  paste0(watershed_table$watershed[.x], paste0("_", site_name[.x], ".world"))),
                             lc_type = c(101,121,122,142,71),
                             target_variables = c("cover_fraction","cover_fraction","cover_fraction","cover_fraction","cover_fraction"),
                             target_values = c(0.9, 0.9, 0.9, 0.9, 0.1),
                             target_canopy = c(1,1,1,1,2),
                             world_out = file.path("ws", watershed_table$watershed[.x], "worldfiles", 
                                                   paste0(watershed_table$watershed[.x], paste0("_", site_name[.x], ".world")))
  )
  
  
})









