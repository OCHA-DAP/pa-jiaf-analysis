library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Chad")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "TCD_HPC2022_JIAF_Aggregation_20220106_WithCheck.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  sheet = "All_PiN_Cible"
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha_raw %>%
  filter(
    !is.na(adm1_state)
  ) %>%
  pivot_longer(
    cols = c(matches("^pi_n_|final_pi_n_hpc2022")),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Chad",
    adm0_pcode = "TCD",
    adm1_name = adm1_state,
    adm1_pcode,
    adm2_name = adm2_county,
    adm2_pcode,
    sector = ifelse(
      sector == "final_pi_n_hpc2022",
      "intersectoral",
      gsub("^pi_n_", "", sector)
    ),
    pin = replace_na(pin, 0),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)
