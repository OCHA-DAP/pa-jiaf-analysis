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
    affected_population = population,
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
    pin = teacher + student,
    affected_population = NA_integer_
  ) %>%
  select(
    sector,
    key_unit,
    population_group,
    affected_population,
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
    affected_population = population_baseline,
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
    population_group = "all",
    affected_population = population_baseline,
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
    affected_population = total_population,
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
    affected_population = population_baseline.x,
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
    affected_population = population,
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
    affected_population = population,
    pin = by_area
  )

df_wash <- rbind(df_wash_residents, df_wash_idps)

df_indicators <- read_excel(
  ocha_fp,
  sheet = "Step 4-Long Data"
) %>%
  clean_names() %>%
  filter(row_number() > 2)

df_sev <- read_excel(
  ocha_fp,
  sheet = "Step 5-Severity",
  skip = 1
) %>%
  clean_names() %>%
  filter(row_number() > 1) %>%
  mutate(
    population_group = ifelse(row_number() < 17, "residents", "IDPs")
  )

########################
#### DATA WRANGLING ####
########################

# only two areas, extracted the pcodes manually
df_organized <- rbind(
  df_ocha_raw,
  df_fslc_raw,
  df_edu_raw,
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
      grepl("LUH", key_unit) ~ "Luhanska",
      TRUE ~ "Other Blasts"
    ),
    adm1_pcode = case_when(
      grepl("DON", key_unit) ~ "UK14",
      grepl("LUH", key_unit) ~ "Uk44",
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
    affected_population = round(replace_na(affected_population, 0)),
    sector,
    pin = round(as.numeric(pin)),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  group_by(
    adm2_name,
    administration,
    population_group
  ) %>%
  mutate(
    affected_population = max(affected_population)
  ) %>%
  ungroup()

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

# deleting those administrations that don't have any PiN for a specific sector
df_summarized_administration <- df_organized %>%
  group_by(sector, administration) %>%
  summarize(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_ukr <- df_organized %>%
  filter(
    paste0(
      adm2_name,
      population_group
    ) %in% paste0(
      df_summarized_pops$adm2_name,
      df_summarized_pops$population_group
    ),
    paste0(
      sector,
      administration
    ) %in% paste0(
      df_summarized_administration$sector,
      df_summarized_administration$administration
    )
  ) %>%
  filter(
    population_group != "total"
  )

df_ukr_sev <- df_sev %>%
  type.convert() %>%
  pivot_longer(
    severity_corrected_for_critical_max_rounded:wash_severity,
    names_to = "sector",
    values_to = "severity"
  ) %>%
  transmute(
    adm0_pcode = "UKR",
    adm0_name = "Ukraine",
    adm1_name = case_when(
      grepl("DON", zone_pop_group) ~ "Donetska",
      grepl("LUH", zone_pop_group) ~ "Luhanska",
      TRUE ~ "Other Blasts"
    ),
    adm1_pcode = case_when(
      grepl("DON", zone_pop_group) ~ "UK14",
      grepl("LUH", zone_pop_group) ~ "Uk44",
      TRUE ~ "Other"
    ),
    adm2_pcode = gsub("GCA_|NGCA_|IDPS__|IDPS_", "", zone_pop_group),
    adm2_name = adm2_pcode,
    administration = case_when(
      grepl("NGCA", zone_pop_group) ~ "non_governement_controlled",
      grepl("GCA", zone_pop_group) ~ "governement_controlled",
      TRUE ~ "Other"
    ),
    population_group,
    sector = gsub("_severity", "", sector),
    sector = case_when(
      sector == "severity_corrected_for_critical_max_rounded" ~ "intersectoral",
      sector == "pro" ~ "protection",
      sector == "ed" ~ "education",
      sector == "fsl" ~ "fslc",
      TRUE ~ sector
    ),
    severity
  ) %>%
  left_join(df_ukr) %>%
  group_by(
    adm2_name,
    administration,
    population_group
  ) %>%
  mutate(
    affected_population = max(affected_population, na.rm = TRUE),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  ungroup()

df_ukr_indicator <- df_indicators %>%
  mutate(
    administration = case_when(
      grepl("NGCA", admin_2_p_code) ~ "non_governement_controlled",
      grepl("GCA", admin_2_p_code) ~ "governement_controlled",
      TRUE ~ "Other"
    ),
    population_group = case_when(
      grepl("IDPS", admin_2_p_code) ~ "IDPs",
      TRUE ~ "residents"
    ),
    adm2_pcode = gsub("IDPS_|NGCA_|GCA_", "", admin_2_p_code)
  ) %>%
  left_join(
    df_ukr %>%
      select(adm1_name, adm1_pcode, adm2_name, adm2_pcode) %>%
      unique(),
    by = "adm2_pcode"
  ) %>%
  transmute(
    adm0_name = "Ukraine",
    adm0_pcode = "UKR",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    population_group,
    administration,
    indicator_number,
    indicator_desc = indicator_text,
    pin = round(as.numeric(calculated_pi_n)),
    severity = calculated_severity
  )

write_csv(
  df_ukr,
  file_paths$save_path
)

write_csv(
  df_ukr_sev,
  file_paths$save_path_sev
)

write_csv(
  df_ukr_indicator,
  file_paths$save_path_indicator
)

write_csv(
  df_ukr_indicator,
  file_paths$save_path_indicator_sev
)
