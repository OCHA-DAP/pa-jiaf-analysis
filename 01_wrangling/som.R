library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Somalia")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "combined_cluster_pin_hxl.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "All PiN"
) %>%
  remove_empty("cols") %>%
  clean_names()

df_pcodes <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "som-administrative-division-names-and-p-codes.xlsx"
  ),
  sheet = "Admin2"
) %>%
  select(
    adm1_pcode = admin1Pcode,
    adm1_name = admin1Name_en,
    adm2_pcode = admin2Pcode,
    adm2_name = admin2Name_en,
  )

########################
#### DATA WRANGLING ####
########################

df_severity <- df_ocha_raw %>%
  rename(
    number_inneed_shelter_returnees = x69,
    number_inneed_shelter_refugees = x70
  ) %>%
  pivot_longer(
    cols = matches("severity"),
    names_to = "sector",
    values_to = "score"
  ) %>%
  select(
    number_adm2_name,
    sector,
    score
  ) %>%
  mutate(
    sector = gsub("number_inneed_|_severity|_[0-9]+", "", sector)
  )


df_pin <- df_ocha_raw %>%
  rename(
    number_inneed_shelter_returnees = x69,
    number_inneed_shelter_refugees = x70
  ) %>%
  pivot_longer(
    cols = !matches("severity|adm"),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  select(
    number_adm2_name,
    sector,
    pin
  ) %>%
  mutate(
    sector = gsub("number_inneed_|_severity|_[0-9]+", "", sector),
    population_group = case_when(
      grepl("non_displaced", sector) ~ "non_displaced",
      grepl("total", sector) ~ "total",
      grepl("displaced", sector) ~ "displaced",
      grepl("returnees", sector) ~ "returnees",
      grepl("refugees", sector) ~ "refugees"
    ),
    sector = str_replace(sector, paste0("_", population_group), "")
  )

df_all <-
  left_join(
    df_pin,
    df_severity,
    by = c("number_adm2_name", "sector")
  ) %>%
  transmute(
    adm0_name = "Somalia",
    adm0_pcode = "SOM",
    adm2_name = str_replace(
      number_adm2_name,
      "_",
      " "
    ),
    adm2_name = case_when( # since no pcodes, have to match CODAB file
      adm2_name == "Kismayo" ~ "Kismaayo",
      adm2_name == "Garowe" ~ "Garoowe",
      adm2_name == "Bandarbayla" ~ "Bandarbeyla",
      adm2_name == "Baidoa" ~ "Baydhaba",
      adm2_name == "Xardheere" ~ "Xarardheere",
      TRUE ~ adm2_name
    ),
    population_group,
    sector = ifelse(sector == "inter_sectoral", "intersectoral", sector),
    pin = round(pin),
    score,
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  left_join(
    df_pcodes,
    by = c("adm2_name")
  ) %>%
  relocate(
    adm1_pcode:adm2_pcode,
    .before = adm2_name
  )

write_csv(
  df_all,
  file_paths$save_path
)
