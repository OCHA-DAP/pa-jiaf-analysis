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

indicator_fp <- file.path(
  file_paths$ocha_dir,
  "cmr_JIAF1.1_AggregationTemplate_Scenario-B_21Nov15_v3.0-OCHAL17243904.xlsx"
)

df_indicators <- read_excel(
  indicator_fp,
  sheet = "Step 4-Long Data"
) %>%
  clean_names()

df_sev <- read_excel(
  indicator_fp,
  sheet = "Step 5-Severity",
  skip = 1
) %>%
  clean_names() %>%
  # final severity is the same as JIAF1.1 severity
  pivot_longer(
    cols = severity_corrected_for_critical_max_rounded:wash,
    values_to = "severity",
    names_to = "sector"
  ) %>%
  mutate(
    sector = case_when(
      sector == "severity_corrected_for_critical_max_rounded" ~ "intersectoral",
      sector == "er" ~ "Early Recovery",
      sector == "fsl" ~ "Food security",
      sector == "nut" ~ "Nutrition",
      sector == "pro" ~ "Protection",
      sector == "refugees" ~ "Refugee Response",
      sector == "wash" ~ "WASH",
      sector == "edu" ~ "Education",
      sector == "health" ~ "Health",
      sector == "esnfi" ~ "Shelter and NFI"
    )
  ) %>%
  transmute(
    adm0_name = "Cameroon",
    adm0_pcode = "CMR",
    adm1_name = adm1_state,
    adm1_pcode,
    adm2_name = adm2_county,
    adm2_pcode,
    sector,
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    ),
    severity
  )

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
    pin = round(replace_na(value, 0)),
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
  summarise(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

# deleting those age-sex groups that don't have any PiN for a specific sector
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_sex = paste0(age, sex)) %>%
  summarize(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_cmr <- df_organized %>%
  filter(
    paste0(adm2_name, population_group) %in% paste0(
      df_summarized_pops$adm2_name,
      df_summarized_pops$population_group
    ),
    paste0(sector, age, sex) %in% paste0(
      df_summarized_age_sex$sector,
      df_summarized_age_sex$age_sex
    )
  )

temp <- df_cmr %>%
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

df_cmr_sev <- df_sev %>%
  left_join(temp)

df_cmr_indicator <- df_indicators %>%
  separate(
    col = key,
    into = c("adm2_pcode", "population_group"),
    sep = "-",
    extra = "merge"
  ) %>%
  left_join(
    df_ocha_raw %>%
      select(adm1_name = region, adm2_name = division, adm2_pcode)
      %>% unique(),
    by = "adm2_pcode"
  ) %>%
  transmute(
    adm0_name = "Cameroon",
    adm0_pcode = "CMR",
    adm1_name,
    adm1_pcode = substr(adm2_pcode, 1, 6),
    adm2_name,
    adm2_pcode,
    population_group,
    indicator_number,
    critical = critical_status == "Yes",
    indicator_desc = indicator_text,
    pin = round(calculated_pi_n),
    severity = calculated_severity
  )

df_cmr_indicator_sev <- df_cmr_indicator %>%
  filter(severity > 0)

write_csv(
  df_cmr,
  file_paths$save_path
)

write_csv(
  df_cmr_sev,
  file_paths$save_path_sev
)

write_csv(
  df_cmr_indicator,
  file_paths$save_path_indicator
)

write_csv(
  df_cmr_indicator_sev,
  file_paths$save_path_indicator_sev
)
