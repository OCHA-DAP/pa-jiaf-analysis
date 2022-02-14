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

###############################
#### CLUSTER PROVIDED DATA ####
###############################

df_edu_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Colombia - Education PiN - 2022 HNO.xlsx"
  ),
  sheet = "Hoja1"
) %>%
  clean_names()

df_prot_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "colombia_protection_pin_15_10_21.xlsx"
  ),
  sheet = "PIN",
  skip = 1
) %>%
  clean_names()

df_gbv_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "colombia_protection_pin_15_10_21.xlsx"
  ),
  sheet = "PIN VBG",
  skip = 1
) %>%
  clean_names()

df_ninez_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "colombia_protection_pin_15_10_21.xlsx"
  ),
  sheet = "PIN Niñez",
  skip = 1
) %>%
  clean_names()

df_minas_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "colombia_protection_pin_15_10_21.xlsx"
  ),
  sheet = "PIN Minas",
  skip = 1
) %>%
  clean_names()

# only look at FSN total
# for sector data because
# it's always higher than
# subsets and those data
# are much messier
df_fsn_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "FS and Nutrition",
    "pin_san_2022_hdx.xlsx"
  )
) %>%
  slice(-1) %>%
  type_convert() %>%
  clean_names()

df_health_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Health",
    "pin_salud_2022_hdx.xlsx"
  )
) %>%
  slice(-1) %>%
  type_convert() %>%
  clean_names()

df_wash_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "WASH",
    "pin_wash_2022_hdx.xlsx"
  )
) %>%
  slice(-1) %>%
  type_convert() %>%
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
    pin_3_plus = ifelse(
      severidad >= 3,
      pin,
      0
    ),
    pin_2_plus = ifelse(
      severidad >= 3,
      pin,
      0
    )
  )

# cluster provided data
df_edu <- df_edu_raw %>%
  transmute(
    source = "cluster",
    sector = "educacion",
    adm2_file_code = divipola,
    sev = severidad_sectorial,
    pin = pin_ectorial_hno
  )

df_fsn <- df_fsn_raw %>%
  transmute(
    source = "cluster",
    sector = "san",
    adm2_file_code = as.numeric(divipola),
    sev = sev_sectorial,
    pin = pin_sectorial
  )

df_health <- df_health_raw %>%
  transmute(
    source = "cluster",
    sector = "salud",
    adm2_file_code = as.numeric(divipola),
    sev = as.numeric(sev_sectorial),
    pin = as.numeric(pin_sectorial)
  )

df_wash <- df_wash_raw %>%
  transmute(
    source = "cluster",
    sector = "wash",
    adm2_file_code = as.numeric(divipola),
    sev = sev_sectorial,
    pin = pin_sectorial
  )

df_prot <- df_prot_raw %>%
  transmute(
    source = "cluster",
    sector = "proteccion",
    adm2_file_code = divipola,
    sev = severidad_municipal,
    pin
  )

df_gbv <- df_gbv_raw %>%
  transmute(
    source = "cluster",
    sector = "vbg",
    adm2_file_code = divipola,
    sev = severidad_municipal,
    pin
  )

df_ninez <- df_ninez_raw %>%
  transmute(
    source = "cluster",
    sector = "ninez",
    adm2_file_code = divipola,
    sev = severidad_municipal,
    pin
  )

df_minas <- df_minas_raw %>%
  transmute(
    source = "cluster",
    sector = "minas",
    adm2_file_code = divipola,
    sev = severidad_ma_ao_r,
    pin
  )

# generate full cluster data
# and calculate the full PiN
# based on severity
df_clusters <- bind_rows(
  df_edu,
  df_fsn,
  df_prot,
  df_gbv,
  df_ninez,
  df_minas,
  df_wash,
  df_health
) %>%
  mutate(
    pin_3_plus = ifelse(
      sev >= 3,
      pin,
      0
    ),
    pin_2_plus = ifelse(
      sev >= 2,
      pin,
      0
    )
  )


############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_col <- right_join(
  df_pcodes,
  bind_rows(
    df_ocha,
    df_clusters
  ),
  by = "adm2_file_code"
) %>%
  filter(!is.na(adm1_es)) %>%
  mutate(
    sector_general = ifelse(
      sector == "intersectorial",
      "intersectoral",
      "sectoral"
    ),
    across(starts_with("pin"), replace_na, 0)
  ) %>%
  select(-c(adm2_file_code, sev, pin))

# Create long format PiN data with
# 2+ and 3+ severity methods
# included, just make sure the
# reported intersectoral is always
# the same
df_col <- df_col %>%
  mutate(
    pin_2_plus = ifelse(
      sector == "intersectorial",
      pin_3_plus,
      pin_2_plus
    )
  ) %>%
  pivot_longer(
    starts_with("pin"),
    names_to = "pin_type",
    values_to = "pin",
    names_prefix = "pin"
  ) %>%
  mutate(
    sector = paste0(sector, pin_type)
  ) %>%
  select(-pin_type)

write_csv(
  df_col,
  file_paths$save_path
)
