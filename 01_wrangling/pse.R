library(tidyverse)
library(readxl)
library(stringr)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("oPt", "Palestine")

############################
#### OCHA PROVIDED DATA ####
############################

fp_ocha <- file.path(
  file_paths$ocha_dir,
  "master_hno_2022_statistics.xlsx"
)

df_ocha_clusters <- read_excel(
  fp_ocha,
  sheet = "Cluster_PIN_Severity",
  range = "A16:P22"
) %>%
  select(
    sector = Cluster,
    Gaza:H2
  ) %>%
  pivot_longer(
    Gaza:H2,
    names_to = "adm2_en",
    values_to = "pin"
  ) %>%
  mutate(
    pin = pin %>%
      str_replace_all(
        pattern = c(
          "(?<!,[0-9]{1})K" = "e3",
          "K" = "00"
        )
      ) %>%
      parse_number(),
    adm2_en = case_when(
      adm2_en == "Area A&B" ~ "Area A & B",
      adm2_en == "EJ" ~ "East Jerusalem",
      TRUE ~ adm2_en
    )
  )

df_ocha_is_raw <- read_excel(
  fp_ocha,
  sheet = "PIN_Geography_Severity",
  range = "A1:L10"
)

df_ocha_is <- df_ocha_is_raw %>%
  select(
    adm2_en = Area,
    pin = PIN
  ) %>%
  mutate(sector = "intersectoral")

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1)

###############
### PCODES ####
###############

df_pcodes <- df_ocha_is_raw %>%
  select(adm1_en = Region, adm2_en = Area) %>%
  unique()

############################
#### GENERATE FULL DATA ####
############################

# note below we generate fake pcode columns
# this is because we want to only use pcodes
# for analysis. However, it looks like the
# areas used for analysis for Palestine are a
# mix of pcoded areas and non-pcoded areas,
# so for simplicity just ignoring that for now.

df_pse <- df_ocha %>%
  left_join(df_pcodes,
    by = "adm2_en",
  ) %>%
  drop_na(adm1_en) %>%
  transmute(
    adm0_name = "oPt",
    adm0_pcode = "PSE",
    adm1_name = adm1_en,
    adm1_pcode = adm1_en,
    adm2_name = adm2_en,
    adm2_pcode = adm2_en,
    sector,
    pin = round(pin),
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_pse,
  file_paths$save_path
)
