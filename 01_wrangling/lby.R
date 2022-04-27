library(tidyverse)
library(readxl)
library(janitor)

###################
#### DATA DIRS ####
###################

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths("Libya")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "libya-2022-hpc-intersectoral-and-sectoral-targets-and-pin-2022_15nov2021.xlsx" # nolint
)

df_ocha_clusters_raw <- read_excel(
  ocha_fp,
  sheet = "Consolidated PiN 2022 (sectors)"
)

df_ocha_is_raw <- read_excel(ocha_fp, sheet = "Intersectoral PiN 2022")

########################
#### DATA WRANGLING ####
########################
# one code to bring them all

# saving pcodes and names to ensure unique names at end

df_pcodes <- df_ocha_clusters_raw %>%
  select(
    adm2_en = `ADM2_Manti (EN)`,
    adm3_en = Balad_ADM3,
    ends_with("Pcode")
  ) %>%
  rename_with(tolower) %>%
  distinct()

# intersectoral pins

df_ocha_is <- df_ocha_is_raw %>%
  clean_names() %>%
  slice(-1) %>%
  type_convert() %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  left_join(
    df_pcodes,
    by = c("mantika" = "adm2_en", "baladiya" = "adm3_en")
  ) %>%
  select(-key, -mantika, -baladiya) %>%
  mutate(
    source = "ocha",
    sector = "intersectoral",
    .before = 1
  )

# ocha provided cluster pins

df_ocha_clusters <- df_ocha_clusters_raw %>%
  clean_names() %>%
  select(
    sector,
    ends_with("pcode"),
    population_group,
    matches("male")
  ) %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  mutate(
    source = "ocha",
    .before = 1
  )
############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_lby <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  left_join(
    df_pcodes,
    by = c("adm2_pcode", "adm3_pcode")
  ) %>%
  select(
    source:adm2_pcode,
    adm2_en,
    adm3_pcode,
    adm3_en,
    population_group:pin
  ) %>%
  mutate(
    adm0_pcode = "LBY",
    adm0_en = "Libya",
    adm1_pcode = substr(adm2_pcode, 1, 4),
    .before = adm2_pcode
  ) %>%
  mutate(
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  rename_at(
    vars(ends_with("_en")),
    ~ str_replace(.x, "_en", "_name")
  )

write_csv(
  df_lby,
  file_paths$save_path
)
