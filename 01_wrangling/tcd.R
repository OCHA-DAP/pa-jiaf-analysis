library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Chad")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "TCD_HPC2022_JIAF_Aggregation_20220106_WithCheck.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  sheet = "All_PiN_Cible"
) %>%
  clean_names()

df_population <- read_excel(
  ocha_fp,
  sheet = "Step 6-PiN",
  skip = 1
) %>%
  clean_names()

df_sev <- read_excel(
  ocha_fp,
  sheet = "Step 5-Severity",
  skip = 1
) %>%
  clean_names() %>%
  filter(!is.na(adm1_state)) %>%
  transmute(
    adm0_name = "Chad",
    adm0_pcode = "TCD",
    adm1_name = adm1_state,
    adm1_pcode,
    adm2_name = adm2_county,
    adm2_pcode,
    intersectoral = severity_corrected_for_critical_max_rounded,
    intersectoral_unadjusted = mean_of_max_50_percent_here_of_23_severity
  ) %>%
  type_convert() %>%
  pivot_longer(
    c(intersectoral, intersectoral_unadjusted),
    names_to = "sector",
    values_to = "severity"
  )

df_ind_sev <- read_excel(
  ocha_fp,
  sheet = "Step 4-Long Data"
) %>%
  clean_names() %>%
  filter(row_number() > 2) %>%
  left_join(df_sev, by = c("admin_2_p_code" = "adm2_pcode")) %>%
  transmute(
    adm0_name,
    adm0_pcode,
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode = admin_2_p_code,
    indicator_number,
    indicator_desc = indicator_text,
    critical = critical_status == "Yes",
    severity = calculated_severity,
    sector = case_when(
      indicator_number %in% 1:3 ~ "Education",
      indicator_number == 4 ~ "Nutrition",
      indicator_number %in% 5:6 ~ "Health",
      indicator_number == 7 ~ "FS/FSL",
      indicator_number == 8 ~ "WASH"
    )
  )

########################
#### DATA WRANGLING ####
########################

df_cleaned <- df_ocha_raw %>%
  filter(
    !is.na(adm1_state)
  ) %>%
  pivot_longer(
    cols = c(matches("^pi_n_|final_pi_n_hpc2022")),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Chad",
    adm0_pcode = "TCD",
    adm1_name = adm1_state,
    adm1_pcode,
    adm2_name = adm2_county,
    adm2_pcode,
    affected_population = df_population$population[match(
      adm2_name,
      df_population$adm2_county
    )],
    sector = ifelse(
      sector == "final_pi_n_hpc2022",
      "intersectoral",
      gsub("^pi_n_", "", sector)
    ),
    pin = round(replace_na(pin, 0)),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  group_by(
    adm2_pcode
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin,
        max(pin),
        round(affected_population)
      ),
    affected_population = max(affected_population)
  ) %>%
  ungroup()

df_tcd_sev <- df_sev %>%
  left_join(
    df_cleaned %>%
      filter(sector == "intersectoral") %>%
      select(-sector)
  ) %>%
  mutate(
    severity = round(severity)
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)

write_csv(
  df_tcd_sev,
  file_paths$save_path_sev
)

write_csv(
  df_ind_sev,
  file_paths$save_path_indicator_sev
)
