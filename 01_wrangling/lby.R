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

df_adm1 <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "lby_adminboundaries_tabulardata.xlsx"
  ),
  sheet = "Admin1"
) %>%
  select(
    adm1_pcode = admin1Pcode,
    adm1_name = admin1Name_en
  )

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
    severity,
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

df_organized <- bind_rows(
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
    dplyr::vars(ends_with("_en")),
    ~ str_replace(.x, "_en", "_name")
  ) %>%
  left_join(
    df_adm1,
    by = c("adm1_pcode")
  ) %>%
  relocate(
    adm1_name,
    .before = adm2_pcode
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm3_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

# deleting those age-sex groups that don't have any PiN for a specific sectoral PiN
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_sex = paste0(age, sex)) %>%
  summarize(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

df_lby <- df_organized %>% 
  filter(
    paste0(adm3_name, population_group) %in% paste0(df_summarized_pops$adm3_name, df_summarized_pops$population_group),
    paste0(sector, age, sex) %in% paste0(df_summarized_age_sex$sector, df_summarized_age_sex$age_sex)
  ) 

write_csv(
  df_lby,
  file_paths$save_path
)
