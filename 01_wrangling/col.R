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

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "PIN intersectorial_Indicadores_CH_VF2022.xlsx"
)

df_indicators <- read_excel(
  ocha_fp,
  sheet = "FINAL PIN y NODOS",
  col_names = FALSE
)

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
    affected_population = poblacion_dane_2021,
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
    affected_population,
    sector,
    pin = round(replace_na(pin, 0), 0),
    # 7 cases where the pin is bit higher than the population
    pin = ifelse(pin > affected_population, affected_population, pin),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectorial",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  filter(!sector %in% c("san", "seguridad_alimentaria"))

indicator_desc <- data.frame(t(df_indicators[c(3, 7), ]), row.names = NULL) %>%
  filter(X1 %in% c(1:14))

names(df_indicators) <-
  paste0(df_indicators[7, ], replace_na(as.character(df_indicators[8, ]), ""))

df_col_indicator <- df_indicators %>%
  clean_names() %>%
  filter(row_number() > 8 & municipio != "X") %>%
  select(
    departamento,
    municipio,
    crit_1:crit_14,
    na_cri_1:na_cri_14
  ) %>%
  pivot_longer(
    cols = matches("^crit|^na_cri"),
    names_to = c(".value", "indicator"),
    names_pattern = "(^crit|^na_cri)_(.*)"
  ) %>%
  left_join(
    df_col %>%
      select(adm1_name, adm1_pcode, adm2_name, adm2_pcode) %>%
      unique(),
    by = c("municipio" = "adm2_name")
  ) %>%
  left_join(
    indicator_desc,
    by = c("indicator" = "X1")
  ) %>%
  transmute(
    adm0_name = "Colombia",
    adm0_pcode = "COL",
    adm1_name,
    adm1_pcode,
    adm2_name = municipio,
    adm2_pcode,
    indicator_number = indicator,
    indicator_desc = X2,
    pin = round(as.numeric(crit)),
    severity = na_cri
  )

write_csv(
  df_col,
  file_paths$save_path
)

write_csv(
  df_col_indicator,
  file_paths$save_path_indicator
)
