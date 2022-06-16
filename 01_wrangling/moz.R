library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Mozambique")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "MOZ_HNO2022_INTERSECTORAL_PIN_&_TARGET_20211116-v11_final .xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Combined_Intersectoral_PIN"
) %>%
  drop_na(ADM1_PROVINCE)

# severity dataset
df_sev <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HNO_SECTOR_PIN_SUMMARY-20211122-final v5.xlsx"
  ),
  sheet = "combined_Cluster_Indicators_PIN",
  skip = 4
) %>%
  clean_names() %>%
  drop_na(adm1_province) %>%
  transmute(
    adm0_name = "Mozambique",
    adm0_pcode = "MOZ",
    adm1_name = adm1_province,
    adm1_pcode = adm1_pcode,
    adm2_name = adm2_district,
    adm2_pcode = adm2_pcode,
    affected_population = total_pop,
    fsl_pin = x11,
    cccm_pin = x12,
    edu_pin = x13,
    health_pin = x14,
    nutrition_pin = x15,
    protection_pin = x19,
    snfi_pin = x20,
    wash_pin = x21,
    intersectoral_pin = overall_max,
    fsl_sev = fsl,
    cccm_sev = ccm,
    edu_sev = edu,
    health_sev = health,
    nutrition_sev = nut,
    protection_sev = pro,
    snfi_sev = shelter,
    wash_sev = wash,
    intersectoral_sev = overall_severity
  ) %>%
  pivot_longer(
    cols = ends_with("pin") | ends_with("sev"),
    names_to = c("sector", ".value"),
    names_pattern = "(.*)_(pin$|sev$)"
  ) %>%
  mutate(
    pin = round(pin),
    severity = round(as.numeric(sev)),
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  select(-sev) %>%
  drop_na(severity)

########################
#### DATA WRANGLING ####
########################


df_moz <- df_ocha_raw %>%
  clean_names() %>%
  transmute(
    adm0_name = "Mozambique",
    adm0_pcode = "MOZ",
    adm1_name = adm1_province,
    adm1_pcode = adm1_pcode,
    adm2_name = adm2_district,
    adm2_pcode = adm2_pcode,
    affected_population = round(total_pop),
    source = "ocha",
    fsc = as.numeric(number_of_people_in_ipc_phases),
    nutrition = as.numeric(
      number_children_6_59_months_with_global_acute_malnutrition_gam_based_on_weight_for_height_z_score_whz_2_and_or_bilateral_pitting_oedema # nolint
    ),
    protection = as.numeric(
      indicator_6_7_indicator_6_availability_of_core_gbv_services_gbv_case_management_individual_psychosocial_support_pss_clinical_management_of_rape_cmr_medical_services_for_ipv_other_physical_violence_mental_health_indicator_7_number_of_gbv_risk_factors_per_location # nolint
    ),
    shelter = as.numeric(
      number_of_individuals_currently_living_in_unsustainable_shelter_situations # nolint
    ),
    wash = as.numeric(
      number_of_people_not_accessing_a_sufficient_quantity_of_safe_water_for_drinking # nolint
    ), # styler: on
    intersectoral = as.numeric(overall_max)
  ) %>%
  mutate(
    across(
      fsc:intersectoral,
      replace_na,
      0
    )
  ) %>%
  pivot_longer(
    cols = fsc:intersectoral,
    names_to = "sector",
    values_to = "pin"
  ) %>%
  mutate(
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    ),
    pin = round(pin)
  ) %>%
  group_by(
    adm2_pcode
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin,
        max(pin),
        affected_population
      ),
    affected_population = max(affected_population)
  ) %>%
  ungroup()

df_moz_indicator <- df_ocha_raw %>%
  type_convert() %>%
  pivot_longer(
    cols = `# of people in IPC phases`:`# of people without access to appropriate hygiene facilities`, # nolint
    values_to = "pin",
    names_to = "indicator_desc"
  ) %>%
  clean_names() %>%
  transmute(
    adm0_name = "Mozambique",
    adm0_pcode = "MOZ",
    adm1_name = adm1_province,
    adm1_pcode = adm1_pcode,
    adm2_name = adm2_district,
    adm2_pcode = adm2_pcode,
    indicator_number = as.integer(factor(indicator_desc)),
    critical = indicator_number %in% c(6, 8, 9, 13, 17),
    indicator_desc = ifelse(
      indicator_desc == "...16",
      "Health Indicator",
      indicator_desc
    ),
    pin = replace_na(round(pin), 0)
  )

write_csv(
  df_moz,
  file_paths$save_path
)

write_csv(
  df_sev,
  file_paths$save_path_sev
)

write_csv(
  df_moz_indicator,
  file_paths$save_path_indicator
)
