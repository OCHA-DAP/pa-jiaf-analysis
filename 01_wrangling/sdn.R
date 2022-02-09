library(tidyverse)
library(readxl)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("Sudan")

############################
#### OCHA PROVIDED DATA ####
############################

df_ocha_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "SDN HNO 2022 Baseline -JIAF exercise.xlsx"
  ),
  sheet = "PIN and Severity",
  skip = 6
) %>%
  rename_all(tolower) %>%
  rename(adm2_pcode = pcode) %>%
  drop_na(adm2_pcode)

df_ocha_clusters <- df_ocha_raw %>%
  select(-c(pin, sev)) %>%
  pivot_longer(
    cols = contains("_pin_") | contains("_sev_"),
    names_to = c("sector", ".value", "condition", "population_group"),
    names_sep = "_",
  ) %>%
  select(
    adm2_pcode, sector, population_group, condition, pin
  ) %>%
  mutate(pin = ifelse(is.na(pin), 0, pin))

df_ocha_is <- df_ocha_raw %>%
  select(adm2_pcode, pin) %>%
  mutate(
    pin = as.numeric(pin), sector = "intersectoral",
    population_group = "all", condition = "all"
  )

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1)

######################
#### CLUSTER DATA ####
######################

# GBV
gbv_fp <- file.path(
  file_paths$cluster_dir,
  "20210927_Sudan_2022_HNO_PiN_FINAL GBV.xlsx"
)

df_gbv <- purrr::map2_dfr(
  c("IDPs", "Returnees", "Vulnerable Hosts", "Overall"),
  c("idp", "ret", "vul", "all"),
  ~ read_excel(
    gbv_fp,
    sheet = .x,
    skip = 18
  ) %>%
    rename_all(tolower) %>%
    mutate(population_group = .y)
) %>%
  select(
    adm2_pcode = admin2pcode,
    pin
  ) %>%
  drop_na(adm2_pcode) %>%
  mutate(sector = "gbv", condition = "all")

# Education
df_edu_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Sudan - Education PiN calculation - 2022 HNO.xlsx"
  ),
  sheet = "EDU", skip = 15
) %>%
  rename_all(tolower)


df_edu <- df_edu_raw %>%
  select(
    adm2_pcode = `pcode admin2`,
    edu_pin_all = edu_pin,
    edu_pin_idp,
    edu_pin_ret,
    edu_pin_vul,
    edu_pin_ref
  ) %>%
  pivot_longer(
    cols = contains("_pin_"),
    names_to = c("sector", ".value", "population_group"),
    names_sep = "_",
  ) %>%
  mutate(condition = "all")

# Combine clusters
df_clusters <- bind_rows(
  df_gbv,
  df_edu
) %>% mutate(source = "cluster")


###############
### PCODES ####
###############

# OCHA data missing admin1 pcodes, use the edu data instead

df_pcodes <- df_edu_raw %>%
  select(
    adm1_en = state,
    adm1_pcode = `p-code admin1`,
    adm2_en = locality,
    adm2_pcode = `pcode admin2`
  ) %>%
  unique()

############################
#### GENERATE FULL DATA ####
############################

df_sdn <- bind_rows(
  df_ocha,
  df_clusters,
) %>%
  left_join(df_pcodes,
    by = "adm2_pcode",
  ) %>%
  relocate(adm1_en:adm2_en, .before = adm2_pcode) %>%
  mutate(
    adm0_en = "Sudan",
    adm0_pcode = "SDN",
    .before = adm1_en,
  ) %>%
  mutate(sector_general = ifelse(
    sector == "intersectoral",
    "intersectoral",
    "sectoral"
  ))

write_csv(
  df_sdn,
  file_paths$save_path
)
