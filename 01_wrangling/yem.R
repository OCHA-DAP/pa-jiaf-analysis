library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Yemen")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "JIAF Yemen dataset_consolidated version 6.7_Final.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "Overall"
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

#renaming the columns with the same name to their cluster name
#adm1 pcodes can be extracted from adm2 pcodes
df_all <- df_ocha_raw %>%
  mutate(
    rmms = sum_row(refugee, migrant, na.rm = T),
    intersectoral = jiaf_refugee_migrant,
    protection = total_pi_n_64,
    wash = total_pi_n_66,
    shelter = total_pi_n_69,
    nutrition = total_pi_n_72,
    education = total_pi_n_75,
    fsac = total_pi_n_78,
    cccm = total_pi_n_81,
    health = total_pi_n_84
  ) %>%
  pivot_longer(
    cols = rmms:health,
    names_to = "sector",
    values_to = "pin"
  ) %>% 
  transmute(
    adm0_en = "Yemen",
    adm0_pcode = "YEM",
    adm1_name = x2,
    adm1_pcode = gsub("[0-9]{2}$", "", pcode),
    amd2_name = district,
    adm2_pcode = pcode,
    population_group = pop_group,
    sector,
    pin = round(replace_na(pin, 0)),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_all,
  file_paths$save_path
)

