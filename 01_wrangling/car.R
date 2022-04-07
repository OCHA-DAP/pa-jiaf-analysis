library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("CAR")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "JIAF 1.1 final aggregation file_OCHA_CAR_Subprefecture_VF.xlsx"
)

# too many columns with the same name
# extracting only the part that is necessary and
# aligns with published HNO
df_ocha_raw <- read_excel(
  ocha_fp,
  range = "AM2:AW186",
  sheet = "Analyse") %>%
  clean_names()

# reading a sheet to extract pcodes
df_ocha_pcodes <- read_excel(
  ocha_fp,
  sheet = "Pop_SPref2022HorsRef") %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

df_ocha <- df_ocha_raw %>%
  transmute(
    adm0_en = "Central African Republic",
    adm0_pcode = "CAR",
    adm1_en = prefecture,
    adm1_pcode = df_ocha_pcodes$pcode_pref_2[match(prefecture, df_ocha_pcodes$prefecture_1)],
    adm2_en = sous_prefecture,
    adm2_pcode = df_ocha_pcodes$pcode_sous_pref[match(sous_prefecture, df_ocha_pcodes$sous_prefecture)],
    adm2_pcode = ifelse(adm2_en == "Bangui", "CF711", adm2_pcode),
    sector = "intersectoral",
    population_group = pop_groupe,
    pin = pin_expert,
    source = "ocha",
    sector_general = "intersectoral"
  )

write_csv(
  df_ocha,
  paste0(jiaf_dir, "/Data aggregated for analysis/car_pins_2022.csv")
)



