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

df_nutr_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "PIN Nutrition HRP 2022 Cluster SAN CO _FV.xlsx"
  ),
  skip = 4,
) %>%
  slice(-c(1:5)) %>%
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

# get all ocha provided data in one file
df_ocha <- df_ocha_raw %>%
  pivot_longer(
    cols = starts_with("pin"),
    names_to = c("sector"),
    names_prefix = "pin_",
    values_to = "pin"
  ) %>%
  transmute(
    source = "ocha",
    sector,
    adm2_file_code = codigo_divipola,
    pin
  )

# cluster provided data
df_edu <- df_edu_raw %>%
  transmute(
    source = "cluster",
    sector = "educacion",
    adm2_file_code = divipola,
    pin = pin_ectorial_hno
  )

df_fsn <- df_fsn_raw %>%
  transmute(
    source = "cluster",
    sector = "san",
    adm2_file_code = as.numeric(divipola),
    pin = pin_sectorial
  )

# nutrition pin is separate from full FSN PiN
# calculated across 5 columns, but not aggregated
# fully in dataset, so having to recalculate PiN
df_nutr <- df_nutr_raw %>%
  rowwise() %>%
  mutate(
    pin = sum(c_across(c(x3_11, x4_13, x5_15, x4_26, x5_28)))
  ) %>%
  ungroup() %>%
  transmute(
    source = "cluster",
    sector = "san_nutricion",
    adm2_file_code = municipality_code,
    pin
  )

df_health <- df_health_raw %>%
  transmute(
    source = "cluster",
    sector = "salud",
    adm2_file_code = as.numeric(divipola),
    pin = as.numeric(pin_sectorial)
  )

df_wash <- df_wash_raw %>%
  transmute(
    source = "cluster",
    sector = "wash",
    adm2_file_code = as.numeric(divipola),
    pin = pin_sectorial
  )

df_prot <- df_prot_raw %>%
  transmute(
    source = "cluster",
    sector = "proteccion",
    adm2_file_code = divipola,
    pin
  )

df_gbv <- df_gbv_raw %>%
  transmute(
    source = "cluster",
    sector = "vbg",
    adm2_file_code = divipola,
    pin
  )

df_ninez <- df_ninez_raw %>%
  transmute(
    source = "cluster",
    sector = "ninez",
    adm2_file_code = divipola,
    pin
  )

df_minas <- df_minas_raw %>%
  transmute(
    source = "cluster",
    sector = "minas",
    adm2_file_code = divipola,
    pin
  )

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_col <- right_join(
  df_pcodes,
  bind_rows(
    df_ocha,
    df_edu,
    df_fsn,
    df_nutr,
    df_prot,
    df_gbv,
    df_ninez,
    df_minas,
    df_wash,
    df_health
  ),
  by = "adm2_file_code"
) %>%
  filter(!is.na(adm1_es))

# write_csv(
#   df_col,
#   file_paths$save_path
# )
