library(tidyverse)
library(readxl)
library(janitor)
library(expss)

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

# renaming the columns with the same name to their cluster name
# adm1 pcodes can be extracted from adm2 pcodes
df_yem <- df_ocha_raw %>%
  filter(pop_group == "Resident") %>%
  mutate(
    rmms = sum_row(refugee, migrant, na.rm = TRUE),
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
    adm0_name = "Yemen",
    adm0_pcode = "YEM",
    adm1_name = x2,
    adm1_pcode = gsub("[0-9]{2}$", "", pcode),
    adm2_name = district,
    adm2_pcode = pcode,
    affected_population = round(total_pop),
    sector,
    pin = round(replace_na(pin, 0)),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  group_by(
    adm2_name
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin,
        max(pin),
        affected_population
      ),
    affected_population = max(affected_population)
  )

df_yem_sev <- df_ocha_raw %>%
  filter(pop_group == "Resident") %>%
  mutate(
    rmms = NA_real_,
    intersectoral = humanitarian_condition_score,
    protection = NA_real_,
    wash = severity_68,
    shelter = severity_71,
    nutrition = severity_74,
    education = severity_77,
    fsac = severity_80,
    cccm = severity_83,
    health = severity_86
  ) %>%
  pivot_longer(
    cols = rmms:health,
    names_to = "sector",
    values_to = "severity"
  ) %>%
  transmute(
    adm0_name = "Yemen",
    adm0_pcode = "YEM",
    adm1_name = x2,
    adm1_pcode = gsub("[0-9]{2}$", "", pcode),
    adm2_name = district,
    adm2_pcode = pcode,
    affected_population = df_yem$affected_population[match(
      adm2_pcode,
      df_yem$adm2_pcode
    )],
    sector,
    pin = df_yem$pin[match(adm2_pcode, df_yem$adm2_pcode)],
    severity = round(severity),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_yem,
  file_paths$save_path
)

write_csv(
  df_yem_sev,
  file_paths$save_path_sev
)
