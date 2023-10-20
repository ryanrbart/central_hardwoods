# Compare usgs streamflow data in Hoosier

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Comparison of streamflow in three Hoosier watersheds.

watershed_table_indiana <- tribble(
  ~watershed, ~watershed_id,
  "patoka", "03374455",
  "beaver", "03373508",
  "anderson", "03303300",
)

# Get usgs gauge details
q_gage_details <- RHESSysWorkflowinR::get_usgs_streamflow_gage_details(watershed_id = watershed_table_indiana$watershed_id, 
                                                                       names = watershed_table_indiana$watershed)

# Get streamflow data
q <- RHESSysWorkflowinR::get_usgs_streamflow_data(usgs_streamflow_gage_details = q_gage_details,
                                                  out = "data/streamflow/q_carb.csv")

# Make summary table
q_data_summary <- RHESSysWorkflowinR::generate_usgs_streamflow_data_summary(usgs_streamflow_gage_details = q_gage_details, 
                                                                            usgs_streamflow = q,
                                                                            out = "data/streamflow/q_carb_summary_table.csv")
print(q_data_summary)

q_annual <- q %>% 
  dplyr::group_by(watershed, wy) %>% 
  dplyr::summarise(q_annual = sum(q_mm), .groups="drop") %>% 
  tidyr::pivot_wider(names_from = watershed, values_from = q_annual)
View(q_annual)



# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Make some figures

# Look at full time-series
ggplot(data = q) +
  geom_line(aes(x = date, y = q_mm)) +
  facet_wrap(vars(watershed),  nrow = 3, scales = "free_x") +
  NULL


# Look at overlapping years between patoka and anderson
q %>% 
  dplyr::filter(watershed != "beaver", wy %in% c(1980, 1981)) %>%
  ggplot(data = .) +
  geom_line(aes(x = date, y = q_mm)) +
  facet_wrap(vars(watershed),  nrow = 2) +
  NULL


# Look at overlapping years between beaver and anderson
q %>% 
  dplyr::filter(watershed != "patoka", wy %in% c(2015)) %>%
  ggplot(data = .) +
  geom_line(aes(x = date, y = q_mm)) +
  facet_wrap(vars(watershed),  nrow = 2) +
  NULL


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Thoughts

# Anderson appears to have some odd behavior, that is consistent with
# manipulation by reservoirs. A good example is 2015, which has numerous periods
# of abnormally high flow. Similar behavior, though not quite as extensive, can
# be seen in other years, e.g. 2017, 2018

# From visualizing the hydrograph, both patoka and beaver seem fine. Despite
# Patoka having being spring fed, the stream shows flashy behavior and normal
# recession curves. This suggests that the spring consists of relatively new
# water and is not a part of an extensive aquifer. That said, Patoka may still
# be drawing from an area larger than the watershed, as the mean annual
# streamflow in Patoka is much higher than Anderson. On the other hand, mean
# annual streamflow in beaver is much lower than Anderson. So although the
# beaver hydrograph appears normal, it may be a losing stream, as possibly
# indicated during tour. I will need to determine mean annual precipitation to
# get a better assessment of patoka vs beaver.



