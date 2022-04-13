library(tidyverse)
library(readxl)
library(expss)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Ukraine")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "2022_JIAF1_UKR_Aggregation_final.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 1,
  sheet = "Step 6-PiN"
) %>%
  clean_names() %>%
  transmute(
    sector = "intersectoral",
    key_unit = zone_pop_group,
    population_group = ifelse(grepl("IDP", key_unit), "IDPs", "residents"),
    pin = total_pi_n
  )  

#education cluster dataset
#duplicated keys are different population groups
#GCA is government controlled area and NGCA is opposite
#last 9 records are IDPs while the rest are residents
#pin is calculated from the sum of score 3 and 4
#for every 10 students, a teacher is affected as well
df_edu_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_Education.xlsx"
  ),
  skip = 1,
  sheet = "+EDU Total PIN+"
) %>%
  clean_names() %>%
  filter(!is.na(key_unit)) %>%
  mutate(
    sector = "education",
    population_group = ifelse(row_number() < 17, "residents", "IDPs"),
    student = sum_row(as.numeric(x3_severe_16), as.numeric(x4_extreme_17),na.rm = T),
    teacher = student * 0.1,
    pin = teacher + student
  ) %>%
  select(
    sector,
    key_unit,
    population_group,
    pin
  )

#food security cluster dataset
df_fslc_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_FSLC.xlsx"
  ),
  skip = 1,
  sheet = "FSLC PiN"
) %>%
  clean_names() %>%
  filter(pi_n_number != "-") %>%
  mutate(
    sector = "fslc",
    population_group = ifelse(row_number() < 17, "residents", "IDPs")
  ) %>%
  select(
    sector,
    key_unit,
    population_group,
    pin = pi_n_number
  )

#health cluster dataset
#the name of the sheet does have 2021
#but the name of the file is 2022 and
#the total health pin matches HNO
#IDPs and residents are grouped
df_health_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_HEALTH.xlsx"
  ),
  sheet = "Health_Cluster_2021_Severit_PIN"
) %>%
  clean_names() %>%
  filter(!is.na(key_unit)) %>%
  transmute(
    sector = "health",
    key_unit,
    population_group = "total",
    pin = updated_pin
  )

#protection cluster data
df_prot_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_Protection.xlsx"
  ),
  skip = 11,
  sheet = "PC HNO 2022 PIN"
) %>%
  clean_names() %>%
  filter(!is.na(side)) %>%
  transmute(
    sector = "protection",
    key_unit = zone_ocha,
    population_group = pop_group,
    pin = x13
  )

########################
#### DATA WRANGLING ####
########################

#only two areas, extracted the pcodes manually
df_all <- rbind(
  df_ocha_raw,
  df_edu_raw,
  df_fslc_raw,
  df_health_raw,
  df_prot_raw
) %>%
  transmute(
    adm0_name = "Ukraine",
    adm1_pcode = "UKR",
    adm1_name = case_when(
      grepl("DON", key_unit) ~ "Donetska",
      grepl("LUN", key_unit) ~ "Luhanska",
      T ~ "Other Blasts"
    ),
    adm1_pcode = case_when(
      grepl("DON", key_unit) ~ "UK14",
      grepl("LUN", key_unit) ~ "Uk44",
      T ~ "Other"),  
    administration = case_when(
      grepl("NGCA", key_unit) ~ "non_governement_controlled",
      grepl("GCA", key_unit) ~ "governement_controlled",
      T ~ "Other"),
    population_group,
    sector,
    pin = round(as.numeric(pin)),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_all,
  file_paths$save_path
)


