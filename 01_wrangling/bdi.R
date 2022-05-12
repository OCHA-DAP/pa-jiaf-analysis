library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Burundi")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Burundi HNO 2022 jiaf1.1 Calculation File-OCHAL17243904.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 2,
  sheet = "PIN Overview-Expert Judgement"
) %>%
  clean_names() %>%
  drop_na(x1)

df_ocha_refugees <- read_excel(
  ocha_fp,
  skip = 2,
  sheet = "BASELINE REFUGIES SADD"
) %>%
  clean_names() %>%
  drop_na(province_43)

df_ocha_pcode_extract <- read_excel(
  ocha_fp,
  sheet = "Step 5-Severity"
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha_raw %>%
  pivot_longer(
    cols = c(abris:secal, pin_final),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Burundi",
    adm0_pcode = "BDI",
    adm1_name = province,
    adm1_pcode = df_ocha_pcode_extract$adm1_pcode[match(
      province,
      df_ocha_pcode_extract$adm1_state
    )],
    population_group = gsub("[0-9]_", "", population),
    sector = ifelse(sector == "pin_final", "intersectoral", sector),
    pin,
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

df_refugees_cleaned <- df_ocha_refugees %>%
  transmute(
    adm0_name = "Burundi",
    adm0_pcode = "BDI",
    adm1_name = province_43,
    adm1_pcode = df_ocha_pcode_extract$adm1_pcode[match(
      province_43,
      df_ocha_pcode_extract$adm1_state
    )],
    population_group = "refugees",
    sector = "refugees",
    pin = total_64,
    source = "ocha",
    sector_general = "sectoral"
  )

df_all <- rbind(
  df_cleaned,
  df_refugees_cleaned
)

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_all %>%
  group_by(adm1_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

df_bdi <- df_all %>% 
  filter(
    paste0(adm1_name, population_group) %in% paste0(df_summarized_pops$adm1_name, df_summarized_pops$population_group)) %>%
  mutate(
    pin = round(pin, 0)
  )

write_csv(
  df_bdi,
  file_paths$save_path
)
