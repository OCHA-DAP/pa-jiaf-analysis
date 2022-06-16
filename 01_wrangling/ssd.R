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

df_indicators <- read_excel(
  ocha_fp,
  sheet = "Step 4-Long Data"
) %>%
  clean_names()

df_sev <- read_excel(
  ocha_fp,
  sheet = "Step 5-Severity",
  skip = 1
) %>%
  clean_names() %>%
  pivot_longer(
    severity_corrected_for_critical_max_rounded:wash_severity,
    names_to = "sector",
    values_to = "severity"
  ) %>%
  mutate(
    sector = ifelse(sector == "severity_corrected_for_critical_max_rounded",
      "intersectoral",
      gsub("_severity", "", sector)
    )
  ) %>%
  select(
    adm2_pcode,
    sector,
    severity
  )

########################
#### DATA WRANGLING ####
########################

# the column names are random, reorganizing the names
# grouping the age categories to only child and adult
df_organized <- df_ocha_raw %>%
  transmute(
    adm1_name = x1,
    adm1_pcode = x2,
    adm2_name = x3,
    adm2_pcode = x4,
    intersectoral_male_child = male_child,
    intersectoral_female_child = female_child_0_5_7,
    intersectoral_male_adult = adult_male_18_59_8,
    intersectoral_female_adult = adult_female_18_59_9,
    intersectoral_total_total = total_pi_n,
    cccm_male_child = sum_row(
      male_child_0_5,
      male_6_17,
      na.rm = TRUE
    ),
    cccm_female_child = sum_row(
      female_child_0_5_14,
      female_6_17,
      na.rm = TRUE
    ),
    cccm_male_adult = sum_row(
      adult_male_18_59_17,
      elderly_male_60,
      na.rm = TRUE
    ),
    cccm_female_adult = sum_row(
      adult_female_18_59_18,
      elderly_female_60,
      na.rm = TRUE
    ),
    education_male_child = ci_n_boys,
    education_female_child = ci_n_girls,
    education_male_adult = adult_male,
    education_female_adult = adult_female,
    fsl_male_child = sum_row(
      no_of_male_children_under_5_25,
      no_of_male_children_aged_5_17_years_27,
      na.rm = TRUE
    ),
    fsl_female_child = sum_row(
      no_of_female_children_under_5_26,
      no_of_female_children_aged_5_17_years_28,
      na.rm = TRUE
    ),
    fsl_male_adult = sum_row(
      no_of_male_adults_aged_18_60_29,
      no_of_male_adults_aged_over_60_31,
      na.rm = TRUE
    ),
    fsl_female_adult = sum_row(
      no_of_female_adults_aged_18_60_30,
      no_female_adults_aged_over_60_32,
      na.rm = TRUE
    ),
    health_male_child = sum_row(
      no_of_male_children_under_5_33,
      no_of_male_children_aged_5_17_years_35,
      na.rm = TRUE
    ),
    health_female_child = sum_row(
      no_of_female_children_under_5_34,
      no_of_female_children_aged_5_17_years_36,
      na.rm = TRUE
    ),
    health_male_adult = sum_row(
      no_of_male_adults_aged_18_60_37,
      no_of_male_adults_aged_over_60_39,
      na.rm = TRUE
    ),
    health_female_adult = sum_row(
      no_of_female_adults_aged_18_60_38,
      no_female_adults_aged_over_60_40,
      na.rm = TRUE
    ),
    nutrition_male_child = hc_cu5_boys,
    nutrition_female_child = hc_cu5_girls,
    nutrition_male_adult = x43,
    nutrition_female_adult = hc_plw,
    protection_male_child = pc_boys,
    protection_female_child = pc_girls,
    protection_male_adult = pc_men,
    protection_female_adult = pc_women,
    snfi_male_child = sum_row(
      number_of_male_children_under_5,
      number_of_male_children_aged_5_17_years,
      na.rm = TRUE
    ),
    snfi_female_child = sum_row(
      number_of_female_children_under_5,
      number_of_female_children_aged_5_17_years,
      na.rm = TRUE
    ),
    snfi_male_adult = sum_row(
      number_of_male_adults_aged_18_60,
      number_of_male_adults_aged_over_60,
      na.rm = TRUE
    ),
    snfi_female_adult = sum_row(
      number_of_female_adults_aged_18_60,
      number_of_female_adults_aged_over_60,
      na.rm = TRUE
    ),
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
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  filter(sex != "total")

# deleting those age-sex groups that don't have any PiN for a specific sector
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_sex = paste0(age, sex)) %>%
  summarize(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_ssd <- df_organized %>%
  filter(
    paste0(sector, age, sex) %in% paste0(
      df_summarized_age_sex$sector,
      df_summarized_age_sex$age_sex
    ),
    age != "total",
    sex != "total"
  )

temp <- df_ssd %>%
  group_by(
    adm0_name,
    adm0_pcode,
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    sector
  ) %>%
  summarize(
    pin = sum(pin),
    .groups = "drop"
  )

df_ssd_sev <- temp %>%
  left_join(df_sev) %>%
  filter(severity > 0)

df_ssd_indicator <- df_indicators %>%
  left_join(
    df_ssd %>%
      select(adm1_name, adm1_pcode, adm2_name, adm2_pcode) %>%
      unique(),
    by = c("admin_2_p_code" = "adm2_pcode")
  ) %>%
  transmute(
    adm0_name = "South Sudan",
    adm0_pcode = "SSD",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode = admin_2_p_code,
    indicator_number,
    critical = critical_status == "Yes",
    indicator_desc = indicator_text,
    pin = replace_na(round(calculated_pi_n), 0),
    severity = calculated_severity
  )

write_csv(
  df_ssd,
  file_paths$save_path
)

write_csv(
  df_ssd_sev,
  file_paths$save_path_sev
)

write_csv(
  df_ssd_indicator,
  file_paths$save_path_indicator
)

write_csv(
  df_ssd_indicator %>% filter(severity > 0),
  file_paths$save_path_indicator_sev
)
