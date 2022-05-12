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

# education cluster dataset
# duplicated keys are different population groups
# GCA is government controlled area and NGCA is opposite
# last 9 records are IDPs while the rest are residents
# pin is calculated from the sum of score 3 and 4
# for every 10 students, a teacher is affected as well
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
    student = sum_row(
      as.numeric(x3_severe_16),
      as.numeric(x4_extreme_17),
      na.rm = TRUE
    ),
    teacher = student * 0.1,
    pin = teacher + student
  ) %>%
  select(
    sector,
    key_unit,
    population_group,
    pin
  )

# food security cluster dataset
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

# health cluster dataset
# the name of the sheet does have 2021
# but the name of the file is 2022 and
# the total health pin matches HNO
# IDPs and residents are grouped
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

# protection cluster data
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

# shelter cluster indicators
df_shelter_raw_1 <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_Shelter.xlsx"
  ),
  range = "B2:H18",
  sheet = "Indicator_Severity_1"
) %>%
  clean_names() %>%
  mutate(
    pin1 = sum_row(
      (x3_severe * population_baseline) +
        (x4_extreme * population_baseline) +
        (x5_catastrophic * population_baseline)
    )
  )

df_shelter_raw_2 <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_Shelter.xlsx"
  ),
  range = "B34:H50",
  sheet = "Indicator_Severity_1"
) %>%
  clean_names() %>%
  mutate(
    pin2 = sum_row(
      (x3_severe * population_baseline) +
        (x4_extreme * population_baseline) +
        (x5_catastrophic * population_baseline)
    )
  )

df_shelter <- left_join(
  df_shelter_raw_1,
  df_shelter_raw_2,
  by = "key_unit"
) %>%
  transmute(
    sector = "shelter",
    key_unit,
    population_group = "residents",
    pin = max_row(pin1, pin2, na.rm = TRUE)
  )

# wash cluster data
df_wash_residents <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_WASH.xlsx"
  ),
  range = "A4:P20",
  sheet = "WASH"
) %>%
  clean_names() %>%
  transmute(
    sector = "wash",
    key_unit = key,
    population_group = "residents",
    pin = by_area
  )

df_wash_idps <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "JIAF_data_UKR_sharing/Cluster data/2022_JIAF_UKR_WASH.xlsx"
  ),
  range = "A28:P38",
  sheet = "WASH"
) %>%
  clean_names() %>%
  transmute(
    sector = "wash",
    key_unit = replace_na(key, "Other"),
    population_group = "IDPs",
    pin = by_area
  )

df_wash <- rbind(df_wash_residents, df_wash_idps)

########################
#### DATA WRANGLING ####
########################

# only two areas, extracted the pcodes manually

df_organized <- rbind(
  df_ocha_raw,
  df_edu_raw,
  df_fslc_raw,
  df_health_raw,
  df_prot_raw,
  df_shelter,
  df_wash
) %>%
  transmute(
    adm0_pcode = "UKR",
    adm0_name = "Ukraine",
    adm1_name = case_when(
      grepl("DON", key_unit) ~ "Donetska",
      grepl("LUN", key_unit) ~ "Luhanska",
      TRUE ~ "Other Blasts"
    ),
    adm1_pcode = case_when(
      grepl("DON", key_unit) ~ "UK14",
      grepl("LUN", key_unit) ~ "Uk44",
      TRUE ~ "Other"
    ),
    adm2_pcode = gsub("GCA_|NGCA_|IDPS__|IDPS_", "", key_unit),
    adm2_name = adm2_pcode,
    administration = case_when(
      grepl("NGCA", key_unit) ~ "non_governement_controlled",
      grepl("GCA", key_unit) ~ "governement_controlled",
      TRUE ~ "Other"
    ),
    population_group,
    sector,
    pin = round(as.numeric(pin)),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

# deleting those administrations that don't have any PiN for a specific sectoral PiN
df_summarized_administration <- df_organized %>%
  group_by(sector, administration) %>%
  summarize(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

df_ukr <- df_organized %>% 
  filter(
    paste0(adm2_name, population_group) %in% paste0(df_summarized_pops$adm2_name, df_summarized_pops$population_group),
    paste0(sector, administration) %in% paste0(df_summarized_administration$sector, df_summarized_administration$administration)
  ) 

abc <- df_ukr %>% filter(sector != "intersectoral") %>% group_by(adm1_name, administration, population_group) %>%
  summarize(pin = max(pin))

write_csv(
  df_ukr,
  file_paths$save_path
)
