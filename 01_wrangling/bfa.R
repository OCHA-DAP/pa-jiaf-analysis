library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Burkina Faso")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "BFA_HPC2022_Cible_17112021.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  sheet = "Cible"
  ) %>%
  clean_names() %>%
  drop_na(x1)

# column names are duplicated
# among those of interest and those not
# dropping all that are not needed and
# renaming the headers
col_indexes <- grep("condition_de_vie", names(df_ocha_raw))
col_indexes <- sort(c(col_indexes, col_indexes + 1, col_indexes + 2))

df_ocha <- df_ocha_raw %>%
  select(
    x1:x7,
    x9,
    x26799,
    all_of(col_indexes)
  )

names(df_ocha) <- df_ocha[1,]

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha %>%
  clean_names() %>%
  filter(id != "ID") %>%
  select(!c(matches("_total|pe$"))) %>%
  pivot_longer(
    cols = matches("^refugies|^pi_n_pdi|^pi_n_non_pdi"),
    names_to = "population_group") %>%
  mutate(
    sector = gsub("^refugies|^pi_n_pdi|^pi_n_non_pdi", "", population_group)
    ) %>%
  transmute(
    adm0_en = "Burkina Faso",
    adm0_pcode = "BFA",
    adm1_en = adm1_state,
    adm1_pcode,
    adm2_en = adm2_county,
    adm2_pcode,
    adm3_en = adm3_county,
    adm3_pcode,
    population_group = case_when(grepl("pi_n_pdi", population_group) ~ "pdi",
                                 grepl("pi_n_non_pdi", population_group) ~ "non_pdi",
                                 T ~ population_group),
    sector = ifelse(sector == "", "intersectoral", gsub("_", "", sector)),
    score = severity,
    pin = ifelse(is.na(value) | value == "-", 0, as.numeric(value)),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)

