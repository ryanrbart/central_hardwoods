# Processes White 2000 parameters

# This could easily be turned into a function with an argument of a vector of
# species.

# Source:
# https://daac.ornl.gov/VEGETATION/guides/white_biome_bgc_parameters.html

source("R/0_utilities.R")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Import the parameters

white_1 <- readr::read_csv("../1_data/parameters/white_model_parameters_652/data/White_spreadsheet1.csv")
white_2 <- readr::read_csv("../1_data/parameters/white_model_parameters_652/data/White_spreadsheet2.csv")
white_3 <- readr::read_csv("../1_data/parameters/white_model_parameters_652/data/White_spreadsheet3.csv")
white_4 <- readr::read_csv("../1_data/parameters/white_model_parameters_652/data/White_spreadsheet4.csv")
white_5 <- readr::read_csv("../1_data/parameters/white_model_parameters_652/data/White_spreadsheet5.csv")


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# QC

white_2 <- white_2 %>% 
  dplyr::mutate(`Foliage Nature` = dplyr::case_when(Species == "Acer saccharum" ~ "Deciduous broad leaf forest",
                                                    Species == "Agropyron repens" ~ "Grass",
                                                    Species == "Agrostis scabra" ~ "Grass",
                                                    .default = `Foliage Nature`))




# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# White table 1

white_1_summary <- white_1 %>% 
  dplyr::filter(Species %in% c("Acer rubrum",	"Acer saccharum",	"Quercus rubra",	"Quercus alba",	"Pinus echinata",	"Pinus strobus")) %>% 
  dplyr::group_by(Parameter, Species) %>% 
  dplyr::summarise(Value = mean(Value), n = n(), .groups = "drop")

View(white_1_summary)


white_1_broad_summary <- white_1 %>% 
 dplyr::group_by(Parameter, `Foliage Nature`) %>% 
  dplyr::summarise(Value = mean(Value), n = n(), .groups = "drop")

View(white_1_broad_summary)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# White table 2

white_2_summary <- white_2 %>% 
  dplyr::mutate(Labile = dplyr::if_else(Labile == -999, NA, Labile),
                Cellulose = dplyr::if_else(Cellulose == -999, NA, Cellulose),
                Lignin = dplyr::if_else(Lignin == -999, NA, Lignin)) %>% 
  dplyr::filter(Species %in% c("Acer rubrum",	"Acer saccharum",	"Quercus rubra",	"Quercus alba",	"Pinus echinata",	"Pinus strobus")) %>% 
  dplyr::group_by(Parameter, Species) %>% 
  dplyr::summarise(Labile = mean(Labile, na.rm = TRUE), Cellulose = mean(Cellulose, na.rm = TRUE), Lignin = mean(Lignin, na.rm = TRUE), n = n(), .groups = "drop") %>% 
  dplyr::mutate(total_percent = Labile + Cellulose + Lignin) %>% 
  # Adjust Labile, Cellulose, and Lignin pools so they sum to 100%, of not already.
  mutate(Labile_adj = round(Labile/(total_percent/100), 3),
         Cellulose_adj = round(Cellulose/(total_percent/100), 3),
         Lignin_adj = round(Lignin/(total_percent/100), 3)) %>% 
  dplyr::mutate(total_percent_adj = Labile_adj + Cellulose_adj + Lignin_adj)

View(white_2_summary)


white_2_broad_summary <- white_2 %>% 
  dplyr::mutate(Labile = dplyr::if_else(Labile == -999, NA, Labile),
                Cellulose = dplyr::if_else(Cellulose == -999, NA, Cellulose),
                Lignin = dplyr::if_else(Lignin == -999, NA, Lignin)) %>% 
  dplyr::group_by(Parameter, `Foliage Nature`) %>% 
  dplyr::summarise(Labile = mean(Labile, na.rm = TRUE), Cellulose = mean(Cellulose, na.rm = TRUE), Lignin = mean(Lignin, na.rm = TRUE), n = n(), .groups = "drop") %>% 
  dplyr::mutate(total_percent = Labile + Cellulose + Lignin) %>% 
  # Adjust Labile, Cellulose, and Lignin pools so they sum to 100%, of not already.
  mutate(Labile_adj = round(Labile/(total_percent/100), 3),
         Cellulose_adj = round(Cellulose/(total_percent/100), 3),
         Lignin_adj = round(Lignin/(total_percent/100), 3)) %>% 
  dplyr::mutate(total_percent_adj = Labile_adj + Cellulose_adj + Lignin_adj)

View(white_2_broad_summary)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# White table 3

white_3_summary <- white_3 %>% 
  dplyr::filter(Species %in% c("Acer rubrum",	"Acer saccharum",	"Quercus rubra",	"Quercus alba",	"Pinus echinata",	"Pinus strobus")) %>% 
  dplyr::group_by(Parameter, Species) %>% 
  dplyr::summarise(Cellulose = mean(Cellulose), Lignin = mean(Lignin), n = n(), .groups = "drop")

View(white_3_summary)


white_3_broad_summary <- white_3 %>% 
  dplyr::filter(!c(Cellulose == -999)) %>% 
  dplyr::group_by(Parameter, `Foliage Nature`) %>% 
  dplyr::summarise(Cellulose = mean(Cellulose), Lignin = mean(Lignin), n = n(), .groups = "drop")

View(white_3_broad_summary)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# White table 4

white_4_summary <- white_4 %>% 
  dplyr::filter(Species %in% c("Acer rubrum",	"Acer saccharum",	"Quercus rubra",	"Quercus alba",	"Pinus echinata",	"Pinus strobus")) %>% 
  dplyr::group_by(Parameter, Species) %>% 
  dplyr::summarise(Initial = mean(Initial), Final = mean(Final), n = n(), .groups = "drop")

View(white_4_summary)


white_4_broad_summary <- white_4 %>% 
  dplyr::group_by(Parameter, `Foliage Nature`) %>% 
  dplyr::summarise(Initial = mean(Initial), Final = mean(Final), n = n(), .groups = "drop")

View(white_4_broad_summary)


# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# White table 5

white_5_summary <- white_5 %>% 
  dplyr::filter(Species %in% c("Acer rubrum",	"Acer saccharum",	"Quercus rubra",	"Quercus alba",	"Pinus echinata",	"Pinus strobus")) %>% 
  dplyr::group_by(Parameter, Species) %>% 
  dplyr::summarise(Initial = mean(Initial), Final = mean(Final), n = n(), .groups = "drop")

View(white_5_summary)


white_5_broad_summary <- white_5 %>% 
  dplyr::group_by(Parameter, `Foliage Nature`) %>% 
  dplyr::summarise(Initial = mean(Initial), Final = mean(Final), n = n(), .groups = "drop")

View(white_5_broad_summary)



