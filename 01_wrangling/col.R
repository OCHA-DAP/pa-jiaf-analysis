library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Colombia")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Resultados PIN 2022 intersectorial y sectorial por municipio.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 1,
  sheet = "PIN Sectorial e Intersectorial"
) %>%
  clean_names()

########################
#### DATA WRANGLING ####
########################
# one code to bring them all

# get pcodes data to bring in names and adm1 pcodes later
df_pcodes <- df_ocha_raw %>%
  transmute(
    adm1_es = departamento,
    adm2_file_code = codigo_divipola,
    adm2_pcode = paste0("CO", str_pad(codigo_divipola, 5, pad = 0)),
    adm2_es = municipio,
    adm1_pcode = substr(adm2_pcode, 1, 4),
    adm0_pcode = "COL"
  )

# Get all ocha provided data in one file.
# Need to rename to ensure severity sector
# names match pin by removing proteccion.
# Also, only consider intersectoral pin
# when severity >= 3
# TODO: confirm sectoral PiN and severity
# analysis needs (waiting for Kashif)
df_ocha <- df_ocha_raw %>%
  rename_with(
    ~ str_replace(.x, "_proteccion_", "_")
  ) %>%
  mutate( # add severity for subset of FSN PiN
    severidad_san_nutricion = severidad_san,
    severidad_seguridad_alimentaria = severidad_san
  ) %>%
  pivot_longer(
    cols = matches("^pin|^sever"),
    names_to = c(".value", "sector"),
    names_pattern = "(^severidad|^pin)_(.*)"
  ) %>%
  transmute(
    source = "ocha",
    sector,
    adm2_file_code = codigo_divipola,
    pin = ifelse(
      severidad < 3 & sector == "intersectorial",
      0,
      pin
    )
  )

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_col <- right_join(
  df_pcodes,
  df_ocha,
  by = "adm2_file_code"
) %>%
  filter(!is.na(adm1_es)) %>%
  transmute(
    adm0_name = "Colombia",
    adm0_pcode,
    adm1_name = adm1_es,
    adm1_pcode,
    adm2_name = adm2_es,
    adm2_pcode,
    sector,
    pin = round(replace_na(pin, 0), 0),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectorial",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  filter(sector != "san")

write_csv(
  df_col,
  file_paths$save_path
)
