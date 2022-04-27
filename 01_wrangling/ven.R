library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Venezuela")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "12012022_Preliminar_Venezuela_PIN_HNO_2022_Total.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  sheet = "Si_PiN X cluster2022"
) %>%
  clean_names() %>%
  drop_na(pcode)

########################
#### DATA WRANGLING ####
########################

df_all <- df_ocha_raw %>%
  pivot_longer(
    cols = -c(pcode, estado),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Venezuela",
    adm0_pcode = "VEN",
    adm1_name = estado,
    adm1_pcode = pcode,
    sector = ifelse(sector == "total_pin", "intersectoral", sector),
    pin = round(pin),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_all,
  file_paths$save_path
)
