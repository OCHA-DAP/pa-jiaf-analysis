library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("CAR", "Central African Republic")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "JIAF 1.1 final aggregation file_OCHA_CAR_Subprefecture_VF.xlsx"
)

# too many columns with the same name
# extracting only the part that is necessary and
# aligns with published HNO
df_ocha_raw <- read_excel(ocha_fp,
  range = "AM2:AW186",
  sheet = "Analyse"
) %>%
  clean_names() %>%
  transmute(prefecture,
    sous_prefecture,
    pop_groupe,
    sector = "intersectoral",
    pin = pin_expert
  )

df_ocha_pcodes <- read_excel(
  ocha_fp,
  sheet = "Pop_SPref2022HorsRef"
) %>%
  filter(row_number() < 80) %>%
  clean_names()

# CCCM/NFI/Shelter data
df_cccm <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_CCCM-NFI-ABRIS.xlsx"
  ),
  sheet = "2-PIN2022Subpref",
  range = "K4:U232"
) %>%
  clean_names() %>%
  transmute(prefecture,
    sous_prefecture,
    pop_groupe,
    pin = pin_expert_2022,
    sector = "cccm/nfi/shelter"
  )

# Education cluster data
df_education <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_Education.xlsx"
  ),
  sheet = "Final_pop_numbers_vFINAL_OCHA"
) %>%
  clean_names() %>%
  filter(!is.na(pcode_pref)) %>%
  transmute(
    prefecture,
    pcode_pref,
    sous_prefecture,
    pcode_sous_pref,
    pop_groupe = "all",
    pin = total,
    sector = "education"
  ) %>%
  mutate(
    adm1_lookup = gsub("_[0-9]+$", "", make_clean_names(prefecture, )),
    adm2_lookup = make_clean_names(sous_prefecture)
  )

# Food Security cluster data
df_fs <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_IPC_AcuteFoodInsec_Sep2021.xlsx"
  ),
  sheet = "IPC-Projection Avril-Aout 2022 ",
  skip = 3
) %>%
  clean_names() %>%
  filter(!is.na(sous_prefecture)) %>%
  transmute(
    prefecture,
    sous_prefecture,
    pop_groupe = "all",
    pin = phase_3_number_2,
    sector = "food security"
  )

# Nutrition cluster data
df_nutr <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_NUTRITION_AMN_RCA.xlsx"
  ),
  sheet = "PIN NUT+Facteurs contributif",
  range = "AJ2:AQ74"
) %>%
  clean_names() %>%
  transmute(
    prefecture = pref,
    sous_prefecture = sp,
    pop_groupe = "all",
    pin = total,
    sector = "nutrition"
  )


# CP cluster data
df_cp <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_Protection_Enfant.xlsx"
  ),
  sheet = "2-PIN2021_SubPref",
  range = "CA4:CK200"
) %>%
  clean_names() %>%
  transmute(prefecture,
    sous_prefecture,
    pop_groupe,
    pin = pin_expert,
    sector = "cp"
  )

# Protection cluster data
df_prot <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_Protection_VF.xlsx"
  ),
  sheet = "2-PIN2022Subpref",
  range = "K4:U232"
) %>%
  clean_names() %>%
  transmute(prefecture,
    sous_prefecture,
    pop_groupe,
    pin = pin_expert,
    sector = "protection"
  )

# Health cluster data
df_health <- read_excel(
  file.path(
    file_paths$ocha_dir,
    stringi::stri_unescape_unicode("CAR_HNO_2022_Sant\u00e9_VF.xlsx")
  ),
  sheet = "2-PIN2021Subpref",
  range = "CT5:DG234"
) %>%
  clean_names() %>%
  transmute(
    prefecture = gsub("(-/-)(.*)$", "", prefecture),
    sous_prefecture,
    pop_groupe,
    pin = expert3,
    sector = "health"
  )

# Protection GBV cluster data, at admin 1 level
df_gbv <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_VBG.xlsx"
  ),
  sheet = "3-PIN"
) %>%
  clean_names() %>%
  transmute(prefecture,
    pop_groupe = "all",
    pin = pin_global,
    sector = "protection_gbv"
  )

# WASH cluster data
df_wash <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_WASH.xlsx"
  ),
  sheet = "2-PIN2021_SubPref",
  range = "BE4:BO200"
) %>%
  clean_names() %>%
  transmute(prefecture,
    sous_prefecture,
    pop_groupe,
    pin = pin_expert,
    sector = "wash"
  )

# Pcodes
df_pcodes <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "CAR_HNO_2022_Protection_VF.xlsx"
  ),
  sheet = "Pop_SPref2022HorsRef"
) %>%
  clean_names() %>%
  filter(row_number() < 80) %>%
  mutate(
    adm1_lookup = gsub("_[0-9]+$", "", make_clean_names(prefecture_1, )),
    adm2_lookup = make_clean_names(sous_prefecture)
  )


########################
#### DATA WRANGLING ####
########################

df_car <- bind_rows(
  df_ocha_raw,
  df_fs,
  df_gbv,
  df_prot,
  df_cp,
  df_nutr,
  df_health,
  df_education,
  df_prot,
  df_wash
) %>%
  # excluding population group disaggregation since only 4 sectors have it
  group_by(
    prefecture,
    sous_prefecture,
    sector
  ) %>%
  summarise(
    pin = sum(round(pin), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    adm1_lookup = gsub(
      "_[0-9]+$",
      "",
      make_clean_names(
        replace_na(prefecture, "Bamingui-Bangoran"),
      )
    ),
    adm2_lookup = gsub(
      "_[0-9]+$",
      "",
      make_clean_names(sous_prefecture)
    ),
    adm2_lookup = case_when(
      adm2_lookup %in% c("nanga_boguila", "nana_boguila") ~ "nangha_boguila",
      adm2_lookup == "bossemtele" ~ "bossemptele",
      adm2_lookup == "aba" ~ "abba",
      adm2_lookup == "dede_mokouba" ~ "dede_makouba",
      adm2_lookup == "bantangafo" ~ "batangafo",
      TRUE ~ adm2_lookup
    ),
  ) %>%
  left_join(
    df_pcodes %>% select(
      adm2_lookup,
      adm1_name = prefecture_1,
      adm1_pcode = pcode_pref_2,
      adm2_name = sous_prefecture,
      adm2_pcode = pcode_sous_pref
    ),
    by = "adm2_lookup"
  ) %>%
  transmute(
    adm0_name = "Central African Republic",
    adm0_pcode = "CAR",
    adm1_name = ifelse(is.na(adm1_name), prefecture, adm1_name),
    adm1_pcode =
      ifelse(
        is.na(adm1_pcode),
        df_pcodes$pcode_pref_2[match(
          adm1_name,
          df_pcodes$prefecture_1
        )],
        adm1_pcode
      ),
    adm2_name = ifelse(is.na(adm2_name), adm1_name, adm2_name),
    adm2_pcode = ifelse(is.na(adm2_pcode), adm1_pcode, adm2_pcode),
    sector,
    pin,
    source = "ocha",
    sector_general =
      ifelse(sector == "intersectoral", "intersectoral", "sectoral")
  )

write_csv(
  df_car,
  gsub("caf_pin", "car_pin", file_paths$save_path)
)
