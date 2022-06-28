library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Venezuela")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "12012022_Preliminar_Venezuela_PIN_HNO_2022_Total.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  sheet = "Si_PiN X cluster2022"
) %>%
  clean_names() %>%
  drop_na(pcode)

df_population <- read_excel(
  ocha_fp,
  sheet = "SEGAL 2022",
  skip = 2
) %>%
  clean_names() %>%
  drop_na(pcode)

df_ocha_sev <- read_excel(
  ocha_fp,
  sheet = "Priorizacion2022",
  skip = 1
) %>%
  clean_names() %>%
  filter(!is.na(x1)) %>%
  transmute(
    adm1_pcode = x1,
    nut = x3,
    edu = x7,
    fsl = x11,
    wash = x16,
    snfi = x20,
    pro = x24,
    cp = x28,
    gbv = x32,
    health = x36
  ) %>%
  pivot_longer(
    nut:health,
    names_to = "sector",
    values_to = "severity"
  )

########################
#### DATA WRANGLING ####
########################

df_ven <- df_ocha_raw %>%
  pivot_longer(
    cols = -c(pcode, estado),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  transmute(
    adm1_name = estado,
    adm1_pcode = pcode,
    sector = case_when(
      sector == "total_pin" ~ "intersectoral",
      sector == "seg_alimentaria" ~ "fsl",
      sector == "educacion" ~ "edu",
      sector == "proteccion" ~ "pro",
      sector == "alojamiento_energia_y_enseres" ~ "snfi",
      sector == "nutricion" ~ "nut",
      sector == "salud" ~ "health",
      TRUE ~ sector
    ),
    pin
  ) %>%
  full_join(df_ocha_sev) %>%
  transmute(
    adm0_name = "Venezuela",
    adm0_pcode = "VEN",
    adm1_name = df_population$estado[match(
      adm1_pcode,
      df_population$pcode
    )],
    adm1_pcode,
    affected_population = df_population$poblacion_total_2021[match(
      adm1_pcode,
      df_population$pcode
    )],
    sector,
    pin = round(pin),
    severity = ifelse(severity == 0.5, 1, round(severity)),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_ven %>% filter(!is.na(pin)),
  file_paths$save_path
)

write_csv(
  df_ven,
  file_paths$save_path_sev
)
