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

df_ocha <- df_ocha_raw %>%
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
    sex = sexe,
    age,
    population_group = gsub("n_", "", population_group),
    pin = value,
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  filter(
    !is.na(pin)
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized <- df_ocha %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin)) %>%
  filter(tot_pin != 0)

df_cleaned <- df_ocha %>% 
  filter(
    paste0(adm2_name, population_group) %in% paste0(df_summarized$adm2_name, df_summarized$population_group)
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)
