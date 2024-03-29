library(tidyverse)
library(readxl)
library(janitor)
library(expss)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Somalia")

############################
#### MSNA Indicator DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Copy of SOM_2021_Intersectoral PiN Estimate.xlsx"
)

df <- read_excel(ocha_fp,
  sheet = "B-HH_data"
) %>%
  filter(row_number() > 1) %>%
  clean_names() %>%
  type_convert() %>%
  select(
    -key
  )

names(df) <- c(
  "hh_id",
  "adm2_name",
  "population_group",
  1:14
)

df_som_pops <- read_excel(ocha_fp,
  sheet = "E-REF_HNO_pop",
  skip = 5
) %>%
  clean_names() %>%
  transmute(
    adm2_name = district,
    idp = of_whom_id_ps,
    res = of_whom_non_displaced
  ) %>%
  pivot_longer(
    cols = -adm2_name,
    values_to = "target_population",
    names_to = "population_group",
    values_drop_na = TRUE
  ) %>%
  filter(target_population > 0)

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

df_cleaned <- df %>%
  mutate(
    hh_id = paste0("SOM", row_number()),
    population_group = case_when(
      population_group == "IDP" ~ "idp",
      population_group == "HC" ~ "res"
    ),
    # some names of adm2 are not aligned with OCHA names
    adm2_name = gsub("_", " ", adm2_name),
    adm2_name = case_when(
      adm2_name == "Baidoa" ~ "Baydhaba",
      adm2_name == "Kismayo" ~ "Kismaayo",
      adm2_name == "Bandarbayla" ~ "Bandarbeyla",
      adm2_name == "Garowe" ~ "Garoowe",
      TRUE ~ adm2_name
    )
  ) %>%
  pivot_longer(
    cols = matches("^[0-9]"),
    names_to = "indicator",
    values_to = "severity",
    values_drop_na = TRUE
  ) %>%
  left_join(df_som_pops, by = c("adm2_name", "population_group")) %>%
  left_join(df_pcodes, by = "adm2_name") %>%
  transmute(
    hh_id,
    adm0_name = "Somalia",
    adm0_pcode = "SOM",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    population_group,
    target_population = round(target_population),
    sector = case_when(
      indicator %in% c(3, 4) ~ "protection",
      indicator == 5 ~ "GBV",
      indicator == 6 ~ "CP",
      indicator == 7 ~ "HLP",
      indicator == 6 ~ "CP",
      indicator == 13 ~ "education",
      indicator == 1 ~ "food security",
      indicator == 14 ~ "health",
      indicator %in% c(8, 9) ~ "SNFI",
      indicator == 2 ~ "Nutrition",
      indicator %in% c(10, 11, 12) ~ "WASH"
    ),
    indicator,
    severity,
    weight = 1
  ) %>%
  # excluding Zdummy district which is not available in many places
  filter(!is.na(target_population))

write_csv(
  df_cleaned,
  file_paths$save_path_hh_data
)
