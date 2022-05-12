library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Mali")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "MALI - HNO 2022 - Données par indicateur_compilation - VF.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "Analyse_InterSect_PIN"
) %>%
  clean_names() %>%
  drop_na(phases_de_l_ipc)

########################
#### DATA WRANGLING ####
########################


df_mli <- df_ocha_raw %>%
  rename(
    abris = pin_21,
    education = pin_22,
    wash = pin_23,
    nutrition = pin_24,
    protection = pin_25,
    health = pin_26,
    food_security = pin_27,
    intersectoral = pin_indicateurs_par_cercle
  ) %>%
  pivot_longer(
    cols = c(abris:food_security, intersectoral),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Mali",
    adm0_pcode = "MLI",
    adm1_name = region,
    adm1_pcode = pcode_reg,
    adm2_name = cercle,
    adm2_pcode = pcode_cer,
    sector,
    pin = round(pin),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_mli,
  file_paths$save_path
)
