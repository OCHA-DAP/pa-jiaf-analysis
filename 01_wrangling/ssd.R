library(tidyverse)
library(readxl)
library(janitor)
library(expss)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("South Sudan")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "SS_20220127_HNO_2022_JIAF1.1_AggregationTemplate_Scenario-B.xlsx"
)

df_ocha_raw <- read_excel(
    ocha_fp,
    skip = 1,
    sheet = "SADD based on clusters data"
  ) %>%
    clean_names()

########################
#### DATA WRANGLING ####
########################

# the column names are random, reorganizing the names
# grouping the age categories to only child and adult
df_ocha <- df_ocha_raw %>%
  transmute(
    adm1_name = x1,
    adm1_pcode = x2,
    adm2_name = x3,
    adm2_pcode = x4,
    intersectoral_male_child     = male_child,
    intersectoral_female_child   = female_child_0_5_7,
    intersectoral_male_adult     = adult_male_18_59_8,
    intersectoral_female_adult   = adult_female_18_59_9,
    intersectoral_total_total    = total_pi_n,
    cccm_male_child              = sum_row(male_child_0_5, male_6_17, na.rm = T),
    cccm_female_child            = sum_row(female_child_0_5_14,female_6_17, na.rm = T),
    cccm_male_adult              = sum_row(adult_male_18_59_17,elderly_male_60, na.rm = T),
    cccm_female_adult            = sum_row(adult_female_18_59_18,elderly_female_60, na.rm = T),
    education_male_child         = ci_n_boys,
    education_female_child       = ci_n_girls,
    education_male_adult         = adult_male,
    education_female_adult       = adult_female,
    fsl_male_child               = sum_row(no_of_male_children_under_5_25, no_of_male_children_aged_5_17_years_27, na.rm = T),
    fsl_female_child             = sum_row(no_of_female_children_under_5_26, no_of_female_children_aged_5_17_years_28, na.rm = T),
    fsl_male_adult               = sum_row(no_of_male_adults_aged_18_60_29, no_of_male_adults_aged_over_60_31, na.rm = T),
    fsl_female_adult             = sum_row(no_of_female_adults_aged_18_60_30, no_female_adults_aged_over_60_32, na.rm = T),
    health_male_child            = sum_row(no_of_male_children_under_5_33, no_of_male_children_aged_5_17_years_35, na.rm = T),
    health_female_child          = sum_row(no_of_female_children_under_5_34, no_of_female_children_aged_5_17_years_36, na.rm = T),
    health_male_adult            = sum_row(no_of_male_adults_aged_18_60_37, no_of_male_adults_aged_over_60_39, na.rm = T),
    health_female_adult          = sum_row(no_of_female_adults_aged_18_60_38, no_female_adults_aged_over_60_40, na.rm = T),
    nutrition_male_child         = hc_cu5_boys,
    nutrition_female_child       = hc_cu5_girls,
    nutrition_male_adult         = x43,
    nutrition_female_adult       = hc_plw,
    protection_male_child        = pc_boys,
    protection_female_child      = pc_girls,
    protection_male_adult        = pc_men,
    protection_female_adult      = pc_women,
    snfi_male_child              = sum_row(number_of_male_children_under_5, number_of_male_children_aged_5_17_years, na.rm = T),
    snfi_female_child            = sum_row(number_of_female_children_under_5, number_of_female_children_aged_5_17_years, na.rm = T),
    snfi_male_adult              = sum_row(number_of_male_adults_aged_18_60, number_of_male_adults_aged_over_60, na.rm = T),
    snfi_female_adult            = sum_row(number_of_female_adults_aged_18_60, number_of_female_adults_aged_over_60, na.rm = T),
    wash_male_child = boys,
    wash_female_child = girls,
    wash_male_adult = men,
    wash_female_adult = women
  ) %>%
  remove_empty("cols") %>%
  pivot_longer(
    cols = intersectoral_male_child:wash_female_adult,
    names_to = "sector",
    values_to = "pin"
  ) %>%
  separate(sector, c("sector", "sex", "age"), sep = "_") %>%
  transmute(
    adm0_name = "South Sudan",
    adm0_pcode = "SSD",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    sector,
    sex,
    age,
    pin = round(replace_na(pin, 0)),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_ocha,
  file_paths$save_path
)


