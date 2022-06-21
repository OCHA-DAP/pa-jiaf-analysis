library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Mali")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  stringi::stri_unescape_unicode(
    "MALI - HNO 2022 - Donn\\u00e9es par indicateur_compilation - VF.xlsx"
  )
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "Analyse_InterSect_PIN"
) %>%
  clean_names() %>%
  drop_na(types_population)

# severity column names
sev_col_names <- c(
  "adm1_name",
  "adm1_pcode",
  "adm2_name",
  "adm2_pcode",
  "pop",
  "pdi_sev",
  "pdi_affect",
  "pdi_pin",
  "pdi_cible",
  "ret_sev",
  "ret_affect",
  "ret_pin",
  "ret_cible",
  "rap_sev",
  "rap_affect",
  "rap_pin",
  "rap_cible",
  "autr_sev",
  "autr_affect",
  "autr_pin",
  "autr_cible",
  "sector"
)

# abris severity
df_abris <- read_excel(
  ocha_fp,
  range = "AD6:AX56",
  sheet = "ABRIS"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  mutate(sector = "abris")

names(df_abris) <- sev_col_names

# education severity
df_edu <- read_excel(
  ocha_fp,
  range = "Z6:AT56",
  sheet = "EDUCATION"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  mutate(sector = "education")

names(df_edu) <- sev_col_names

# EHA severity
df_eha <- read_excel(
  ocha_fp,
  range = "AE6:AY56",
  sheet = "EHA"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  mutate(sector = "wash")

names(df_eha) <- sev_col_names

# Nutrition severity
df_nut <- read_excel(
  ocha_fp,
  range = "Y6:AS56",
  sheet = "Nutrition"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  mutate(sector = "nutrition")

names(df_nut) <- sev_col_names

# Protection severity
df_pro <- read_excel(
  ocha_fp,
  range = "X6:AR56",
  sheet = "PROTECTION"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  mutate(sector = "protection")

names(df_pro) <- sev_col_names

# Health severity
df_health <- read_excel(
  ocha_fp,
  range = "Z6:AU56",
  sheet = "SANTE"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  select(-x5) %>%
  mutate(sector = "health")

names(df_health) <- sev_col_names

# Health severity
df_fs <- read_excel(
  ocha_fp,
  range = "X6:AS56",
  sheet = "SECURITE ALIMENTAIRE"
) %>%
  clean_names() %>%
  drop_na(x1) %>%
  select(-x5) %>%
  mutate(sector = "food_security")

names(df_fs) <- sev_col_names

########################
#### DATA WRANGLING ####
########################

df_mli <- df_ocha_raw %>%
  rename(
    abris = pin_21,
    education = pin_22,
    wash = pin_23,
    nutrition = pin_24,
    protection = pin_25,
    health = pin_26,
    food_security = pin_27,
    intersectoral = pin_indicateurs_par_cercle
  ) %>%
  pivot_longer(
    cols = c(abris:food_security, intersectoral),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm0_name = "Mali",
    adm0_pcode = "MLI",
    adm1_name = region,
    adm1_pcode = pcode_reg,
    adm2_name = cercle,
    adm2_pcode = pcode_cer,
    population_group = types_population,
    affected_population = estimation_population_en_2021,
    sector,
    pin = round(pin),
    affected_population = round(affected_population),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  group_by(
    adm2_pcode,
    population_group
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

df_mli_sev <- rbind(
  df_abris,
  df_edu,
  df_eha,
  df_fs,
  df_health,
  df_nut,
  df_pro
) %>%
  pivot_longer(
    cols = ends_with("pin") | ends_with("sev"),
    names_to = c("population_group", ".value"),
    names_pattern = "(.*)_(pin$|sev$)"
  ) %>%
  transmute(
    adm0_name = "Mali",
    adm0_pcode = "MLI",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    sector,
    population_group = case_when(
      population_group == "autr" ~ "Autres populations",
      population_group == "pdi" ~ "PDI",
      population_group == "rap" ~ "Rapatries",
      population_group == "ret" ~ "Retournes"
    ),
    affected_population = df_mli$affected_population[
      match(
        paste0(adm2_pcode, population_group),
        paste0(df_mli$adm2_pcode, df_mli$population_group)
      )
    ],
    severity = sev,
    pin = round(pin),
    sector_general = "sectoral"
  ) %>%
  filter(!is.na(severity))

write_csv(
  df_mli,
  file_paths$save_path
)

write_csv(
  df_mli_sev,
  file_paths$save_path_sev
)
