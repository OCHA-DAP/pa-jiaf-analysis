library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Cameroon")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Cmr_hno22_Sectoral_PIN_TGT_Compilation_(20220104)_v2.xlsx"
)

df_ocha_raw <- map_dfr(
  c(
    "Early Recovery",
    "Education",
    "Food security",
    "Health",
    "Nutrition",
    "Protection",
    "PRT",
    "CP",
    "GBV",
    "HLP",
    "Refugee Response",
    "Shelter and NFI",
    "WASH",
    "population PIN & Targets 2022"
  ),
  ~ read_excel(
    ocha_fp,
    skip = ifelse(.x == "Health", 3, 5),
    sheet = .x
  ) %>%
    clean_names() %>%
    drop_na(region) %>%
    mutate(sector = .x)
) %>%
  bind_rows()


########################
#### DATA WRANGLING ####
########################

df_organized <- df_ocha_raw %>%
  pivot_longer(
    cols = n_pdi:n_other,
    names_to = "population_group"
  ) %>%
  transmute(
    adm0_name = "Cameroon",
    adm0_pcode = "CMR",
    adm1_name = region,
    adm1_pcode = substr(adm2_pcode, 1, 6),
    adm2_name = division,
    adm2_pcode,
    sector = ifelse(
      sector == "population PIN & Targets 2022",
      "intersectoral",
      sector
    ),
    age,
    sex = sexe,
    population_group = gsub("n_", "", population_group),
    pin = replace_na(value, 0),
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

# deleting those age-sex groups that don't have any PiN for a specific sectoral PiN
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_sex = paste0(age, sex)) %>%
  summarize(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

df_cmr <- df_organized %>% 
  filter(
    paste0(adm2_name, population_group) %in% paste0(df_summarized_pops$adm2_name, df_summarized_pops$population_group),
    paste0(sector, age, sex) %in% paste0(df_summarized_age_sex$sector, df_summarized_age_sex$age_sex)
  ) 

write_csv(
  df_cmr,
  file_paths$save_path
)
