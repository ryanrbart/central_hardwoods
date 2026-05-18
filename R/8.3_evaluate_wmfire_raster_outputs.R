# Organize and process WMFire raster outputs


source("R/0_utilities.R")


# --------------------------------------------------------------------------
# Function

move_files_by_prefix <- function(dir_path,
                                 grid_names,
                                 sub_dir_name,
                                 overwrite = FALSE,
                                 dry_run = FALSE,
                                 verbose = TRUE) {
  
  sub_dir <- file.path(dir_path, sub_dir_name)
  
  # Create directory if needed
  if (!dir.exists(sub_dir)) {
    if (verbose) message("Creating directory: ", sub_dir)
    if (!dry_run) dir.create(sub_dir, recursive = TRUE)
  }
  
  # Build regex
  pattern <- str_c("^(", str_c(grid_names, collapse = "|"), ")")
  
  # Build file table
  file_tbl <- tibble::tibble(
    from = list.files(dir_path, full.names = TRUE)
  ) %>%
    mutate(
      file_name = basename(from),
      matches = str_detect(file_name, pattern)
    ) %>%
    filter(matches) %>%
    mutate(
      to = file.path(sub_dir, file_name),
      exists = file.exists(to)
    )
  
  if (nrow(file_tbl) == 0) {
    if (verbose) message("No matching files found.")
    return(invisible(file_tbl))
  }
  
  # Handle overwrite
  if (!overwrite) {
    skipped <- file_tbl %>% filter(exists)
    file_tbl <- file_tbl %>% filter(!exists)
    
    if (verbose && nrow(skipped) > 0) {
      message("Skipping ", nrow(skipped), " existing file(s).")
    }
  }
  
  # Dry run
  if (dry_run) {
    if (verbose) {
      message("Dry run: files that would be moved:")
      print(file_tbl %>% select(from, to))
    }
    return(invisible(file_tbl))
  }
  
  # Move files with purrr
  results <- file_tbl %>%
    mutate(success = map2_lgl(from, to, file.rename))
  
  # Logging
  if (verbose) {
    message("Moved ", sum(results$success), " file(s).")
    if (any(!results$success)) {
      message("Failed to move ", sum(!results$success), " file(s).")
    }
  }
  
  return(invisible(results))
}


# ----
summarize_txt_tables <- function(dir_path,
                                 pattern = "\\.txt$",
                                 prefix = NULL) {
  
  files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
  
  # Apply prefix filter if provided
  if (!is.null(prefix)) {
    prefix_pattern <- str_c("^(", str_c(prefix, collapse = "|"), ")")
    files <- files[str_detect(basename(files), prefix_pattern)]
  }
  
  results <- tibble(file = files) %>%
    mutate(
      stats = map(file, function(f) {
        
        df <- read_table(f, col_names = FALSE, show_col_types = FALSE)
        
        vals <- as.numeric(unlist(df))
        if (prefix %in% c("FireFailedIterGrid","FireSpreadIterGrid","PDefGrid","PLoadGrid","PSlopeGrid","PWindGrid")){vals_clean <- vals[!is.na(vals) & vals != -1]}
        if (prefix %in% c("FireSpreadPropGrid","LoadGrid","RelDefGrid","VegLoadGrid")){vals_clean <- vals[!is.na(vals) & vals != 0]}
        if (prefix %in% c("SoilMoistGrid")){vals_clean <- vals[!is.na(vals) & vals != 100]}
        if (prefix %in% c("ETGrid","PETGrid","UnderETGrid","UnderPETGrid")){vals_clean <- vals[!is.na(vals) & vals != 1000]}
        
        if (length(vals_clean) == 0) {
          return(tibble(
            n = 0,
            mean = NA_real_,
            median = NA_real_,
            min = NA_real_,
            max = NA_real_
          ))
        }
        
        tibble(
          n = length(vals_clean),
          mean = round(mean(vals_clean),3),
          median = round(median(vals_clean),3),
          min = round(min(vals_clean),3),
          max = round(max(vals_clean),3)
        )
      })
    ) %>%
    tidyr::unnest(stats) %>%
    dplyr::mutate(file = basename(file))
  
  results <- results %>% 
    mutate(match = str_match(file, "Year(\\d{4})Month(\\d+)"),
           year  = as.integer(match[,2]),
           month = as.integer(match[,3]),
           date = lubridate::make_date(year = year, month = month)) %>%
    relocate(., date, year, month, .after = file) %>% 
    select(-match) %>% 
    arrange(year, month)
    
    return(results)
}


# ----
weighted_summary <- function(summary_tbl) {
  
  # Remove rows with zero or NA weights
  df <- summary_tbl %>%
    filter(!is.na(n), n > 0)
  
  total_n <- sum(df$n)
  
  if (total_n == 0) {
    return(tibble(
      weighted_mean = NA_real_,
      weighted_median = NA_real_,
      weighted_min = NA_real_,
      weighted_max = NA_real_,
      total_n = 0
    ))
  }
  
  tibble(
    weighted_mean   = sum(df$mean   * df$n) / total_n,
    weighted_median = sum(df$median * df$n) / total_n,
    weighted_min    = sum(df$min    * df$n) / total_n,
    weighted_max    = sum(df$max    * df$n) / total_n,
    total_n         = total_n
  )
}



# --------------------------------------------------------------------------
# Move files


grid_names <- c(
  "ETGrid",
  "FireFailedIterGrid",
  "FireSizes",
  "FireSpreadIterGrid",
  "FireSpreadPropGrid",
  "LoadGrid",
  "PDefGrid",
  "PETGrid",
  "PLoadGrid",
  "PSlopeGrid",
  "PWindGrid",
  "RelDefGrid",
  "SoilMoistGrid",
  "UnderETGrid",
  "UnderPETGrid",
  "VegLoadGrid"
)

dir_path <- "./wmfire_output/sim_2"

move_files_by_prefix(
  dir_path = ".",
  grid_names = grid_names,
  sub_dir_name = dir_path,
  dry_run = FALSE 
)


# --------------------------------------------------------------------------
# Look at summaries


# P-load
pload_out <- summarize_txt_tables(
  dir_path = dir_path,
  prefix = "PLoadGrid"
)

# P-def
pdef_out <- summarize_txt_tables(
  dir_path = dir_path,
  prefix = "PDefGrid"
)

# P-wind
pwind_out <- summarize_txt_tables(
  dir_path = dir_path,
  prefix = "PWindGrid"
)

# P-def
pslope_out <- summarize_txt_tables(
  dir_path = dir_path,
  prefix = "PSlopeGrid"
)

# FireSpreadPropGrid
ps_out <- summarize_txt_tables(
  dir_path = dir_path,
  prefix = "FireSpreadPropGrid"
)


View(pload_out)
View(pdef_out)
View(pwind_out)
View(pslope_out)
View(ps_out)



weighted_summary(pload_out)
weighted_summary(pdef_out)
weighted_summary(pwind_out)
weighted_summary(pslope_out)


# --------------------------------------------------------------------------
# Display time-series

# Rename n to mean for convenient plotting
n_out <- pload_out %>% 
  rename(n_old = n, mean_old = mean) %>% 
  rename(n = mean_old, mean = n_old) %>% 
  relocate(n, .after = month)

out_list = list(n = n_out, pLoad = pload_out, pDef = pdef_out, pWind = pwind_out, pSlope = pslope_out)
out <- bind_rows(out_list, .id = "name")

out %>% 
  ggplot(data = .) +
  geom_col(aes(x = date, y = mean)) +
  facet_grid(rows = "name", scale = "free")



# --------------------------------------------------------------------------
# Find key events




# --------------------------------------------------------------------------
# Display a map


spread_data <- readr::read_table("wmfire_output/sim_1/PDefGridYear1980Month7.txt", col_names = FALSE)

plot(spread_data)


x <- ggplot(data = fire_data) +
  geom_rect(aes())
  NULL





