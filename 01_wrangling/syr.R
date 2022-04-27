library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Syria")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Syria IS_PiN_2022_master_SHARE TO HQ.xlsx"
)

# reading the intersectoral pin
df_ocha_pin <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "Master",
  guess_max = 500
) %>%
  clean_names() %>%
  rename(
    intersectoral_pin = total_pin_after_expert_review
  )

# reading the intersectoral severity
df_ocha_severity <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "SD severity summary",
  guess_max = 500
) %>%
  clean_names() %>%
  transmute(
    admin3pcode = row_labels,
    intersectoral_severity = subdistrict_severity
  )

# reading the clusteral pin and severity
df_clusters <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "2022_PiN Severity - Sectors PIN-Severity shared to HQ for JIAF2.xlsx"
  ),
  sheet = "Sector PIN-Severity",
  guess_max = 500
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

# combining all three datasets
# taking out subdistricts that have no population
# casting the data types of numeric variables to be numeric
df_combined_all <- left_join(
  df_ocha_pin,
  df_ocha_severity,
) %>%
  left_join(
    df_clusters,
    by = c(
      "admin1name_en",
      "admin1pcode",
      "admin2name_en",
      "admin2pcode",
      "admin3name_en",
      "admin3pcode"
    )
  ) %>%
  filter(df_ocha_pin$final_est_of_total_pop_aug_2021 != 0) %>%
  mutate(
    across(
      matches("_pin|_severity"),
      as.numeric
    )
  )

# pivoting the pins
df_pins <- df_combined_all %>%
  pivot_longer(
    cols = matches("_pin$"),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  mutate(
    sector = gsub("_pin", "", sector),
    pin = round(replace_na(pin, 0))
  )

# pivoting the severities
df_scores <- df_combined_all %>%
  pivot_longer(
    cols = matches("_severity$"),
    names_to = "sector",
    values_to = "score"
  ) %>%
  mutate(
    sector = gsub("_severity", "", sector),
    score = round(replace_na(score, 0))
  )

# combining the pins and severities and reformatting
df_all <- left_join(
  df_pins,
  df_scores,
  by = c(
    "admin1name_en",
    "admin1pcode",
    "admin2name_en",
    "admin2pcode",
    "admin3name_en",
    "admin3pcode",
    "sector"
  )
) %>%
  transmute(
    adm0_name = "Syria",
    adm0_pcode = "SYR",
    adm1_name = admin1name_en,
    adm1_pcode = admin1pcode,
    adm2_name = admin2name_en,
    adm2_pcode = admin2pcode,
    adm3_name = admin3name_en,
    adm3_pcode = admin3pcode,
    sector,
    pin,
    score = ifelse(pin == 0, 0, score),
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
