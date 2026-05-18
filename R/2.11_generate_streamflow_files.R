# Process usgs streamflow data

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Process USGS gauges

watershed_table_usgs <- watershed_table %>% 
  dplyr::filter(watershed != "barren")


# Get usgs gauge details
q_gage_details <- get_usgs_streamflow_gage_details(watershed_id = watershed_table_usgs$watershed_id, 
                                                   names = watershed_table_usgs$watershed)

# Get streamflow data
q_usgs <- get_usgs_streamflow_data(usgs_streamflow_gage_details = q_gage_details,
                                   site_name = "watershed",
                                   out = "data/streamflow/q_usgs.csv")

# Make summary table
q_data_summary <- generate_usgs_streamflow_data_summary(usgs_streamflow_gage_details = q_gage_details, 
                                                        usgs_streamflow = q_usgs,
                                                        out = "data/streamflow/q_usgs_summary_table.csv")



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Process Barren streamflow data

area_km2 <- watershed_table %>% 
  dplyr::filter(watershed == "barren") %>% 
  dplyr::pull(area_km2)

q_barren <- readxl::read_excel("data/streamflow/mark_twain/LNA-MBB(WY16-22)avgdailyq_FINAL.xlsx", sheet = "LowerNA(WY16-22)avgdailyq") %>% 
  dplyr::rename(q_m3s = `Avg. Daily Q (m3/s)`) %>% 
  dplyr::mutate(year = lubridate::year(Date),
                month = lubridate::month(Date),
                day = lubridate::day(Date),
                wy = EcoHydroConversions::cy_to_wy(cy=year, m=month, start_month=10),
                cyd = EcoHydroConversions::date_to_cyd(cy=year, m=month, d=day),
                wyd = purrr::pmap_dbl(list(cy=year, cyd=cyd, wyd_start=274), EcoHydroConversions::date_to_wyd),
                # Change q from m3/s to mm
                q_mm = ((q_m3s*86400)/(area_km2*1000000))*1000)
# View(q_barren)

# Start and end dates (Record starts on 9 September 2016 and ends on 30 September 2022)
head(q_barren)
tail(q_barren)

# Identify the gaps
q_barren_na <- q_barren %>% 
  dplyr::filter(is.na(q_mm) == TRUE)

q_barren_na %>% 
  dplyr::group_by(wy) %>% 
  dplyr::summarize(count = n())

# Remove wy2016 because incomplete. While wy2020 is also incomplete, leaving it
# since I need rows for those dates. However, only wy2017, wy2018, wy2019,
# wy2021, wy2022 should be used for comparison. Fill 1 gap in wy2017 and 1 gap
# in wy2022. This latter part was done manually by averaging the values before
# and after the gap.

q_barren <- q_barren %>% 
  dplyr::filter(wy %in% c(2017, 2018, 2019, 2020, 2021, 2022)) %>% 
  dplyr::mutate(q_mm = if_else(year == 2017 & month == 6 & day == 27, 0.138645, q_mm),
                q_mm = if_else(year == 2021 & month == 10 & day == 20, 0.15088, q_mm))


# Reformat so a_barren has same format as usgs q.
# Add watershed, site, watershed id, area_kms, date, and error code.

q_barren <- q_barren %>% 
  dplyr::select(-Days) %>% 
  dplyr::mutate(watershed = "barren",
                site = "watershed",
                watershed_id = "00000000",
                area_km2 = .env$area_km2,
                q_cfs = NA,
                date = as.Date(paste(year, month, day, sep="-")),
                error_code = "NA") %>% 
  dplyr::relocate(watershed,site,
                  watershed_id,
                  date,
                  area_km2,
                  year,
                  wy,
                  month,
                  day,
                  cyd,
                  wyd,
                  q_cfs,
                  q_mm,
                  error_code)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Add Barren to USGS data

# Get usgs q data
q_usgs <- readr::read_csv("data/streamflow/q_usgs.csv")

# Combine barren and usgs
q_central_hardwood <- dplyr::bind_rows(q_barren, q_usgs)

# Write streamflow
readr::write_csv(q_central_hardwood, "data/streamflow/q_central_hardwood.csv")



