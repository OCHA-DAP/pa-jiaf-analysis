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

######################
#### CLUSTER DATA ####
######################

# Shelter
df_shelter <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "OPT_MSNA-Dataset_OPT2101_16082021_shelter_Cluster06092021 26092021.xlsx"
  ),
  sheet = "Calculated PIN-JIAF",
  skip = 2
) %>%
  select(
    adm2_en = `Row Labels`,
    pin = PIN
  ) %>%
  drop_na() %>%
  mutate(adm2_en = case_when(
    adm2_en == "Area_A_B" ~ "Area A & B",
    adm2_en == "East_Jerusalem" ~ "East Jerusalem",
    adm2_en == "Deir al Balah" ~ "Deir al-Balah",
    TRUE ~ adm2_en
  )) %>%
  mutate(sector = "Shleter&NFIs")

# Education
df_edu <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "OpT - Education PiN and severity calculations - 2022 HNO.xlsx"
  ),
  sheet = "Total PiN",
  skip = 3
) %>%
  select(
    adm2_en = Area,
    pin = `Estimated JIAF PIN\r\n(Severity classes 3, 4 & 5)`
  ) %>%
  drop_na() %>%
  mutate(adm2_en = case_when(
    adm2_en == "area_ab" ~ "Area A & B",
    adm2_en == "area_c" ~ "Area C",
    adm2_en == "deir_balah" ~ "Deir al-Balah",
    adm2_en == "ej" ~ "East Jerusalem",
    TRUE ~ adm2_en
  )) %>%
  mutate(
    adm2_en = adm2_en %>%
      str_replace("_", " ") %>%
      str_to_title(),
    sector = "Education"
  )

# GBV
df_gbv <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "20211015_oPT_2022_HNO_PiN_FINAL GBV.xlsx"
  ),
  sheet = "GBV PiN-Summary",
  skip = 13
) %>%
  select(
    adm2_en = `HNO Strata`,
    pin = `GBV PiN`
  ) %>%
  drop_na() %>%
  mutate(
    adm2_en = case_when(
      adm2_en == "Area_A_B" ~ "Area A & B",
      adm2_en == "East_Jerusalem" ~ "East Jerusalem",
      adm2_en == "Deir al Balah" ~ "Deir al-Balah",
      TRUE ~ adm2_en
    ),
    sector = "gbv"
  )

# WASH
df_wash <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "WASH Palestine",
    "2022",
    "WASH PIN calculation_SoP HNO 2022.xlsx"
  ),
  sheet = "WASH PIN",
  skip = 3
) %>%
  select(
    adm2_en = `Geo-areas`,
    indicator = `...7`,
    pin = `...18`
  ) %>%
  filter(cumany(indicator == "Total PIN\r\nJIAF")) %>%
  select(-indicator) %>%
  mutate(
    adm2_en = case_when(
      adm2_en == "Area_A_B" ~ "Area A & B",
      adm2_en == "Gaza strip" ~ "Gaza Strip",
      TRUE ~ adm2_en
    ),
    sector = "WASH"
  )

# Combine clusters
df_clusters <- bind_rows(
  df_shelter,
  df_edu,
  df_gbv,
  df_wash
) %>%
  mutate(source = "cluster")

############################
#### GENERATE FULL DATA ####
############################

# note below we generate fake pcode columns
# this is because we want to only use pcodes
# for analysis. However, it looks like the
# areas used for analysis for Palestine are a
# mix of pcoded areas and non-pcoded areas,
# so for simplicity just ignoring that for now.

df_pse <- bind_rows(
  df_ocha,
  df_clusters
) %>%
  left_join(df_pcodes,
    by = "adm2_en",
  ) %>%
  drop_na(adm1_en) %>%
  relocate(adm2_en, .before = sector) %>%
  relocate(adm1_en, .before = adm2_en) %>%
  mutate(
    adm0_en = "oPt",
    adm0_pcode = "pse",
    adm1_pcode = adm1_en,
    .before = adm1_en
  ) %>%
  mutate(
    adm2_pcode = adm2_en,
    .before = adm2_en
  ) %>%
  mutate(
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_pse,
  file_paths$save_path
)
