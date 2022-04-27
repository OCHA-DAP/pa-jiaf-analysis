library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Haiti")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Haiti HPC 2022 - JIAF_v1.1.xlsx"
)

df_ocha <- read_excel(
  ocha_fp,
  skip = 1,
  sheet = "PiN summary"
) %>%
  clean_names() %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = dep_p_code,
    adm2_name = commune,
    adm2_pcode = com_p_code,
    sector = "intersectoral",
    pin = as.numeric(pi_n),
    score = as.numeric(vc),
    source = "ocha",
    sector_general = "intersectoral"
  )

# food security
df_cluster_fs <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - SECURITE ALIMENTAIRE.xlsx"
  ),
  skip = 4,
  sheet = "SECURITE ALIMENTAIRE"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = pcode_com,
    sector = "fsc",
    pin = as.numeric(pin),
    score = as.numeric(x18),
    source = "ocha",
    sector_general = "sectoral"
  )

# WASH
df_cluster_wash <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - EPAH.xlsx"
  ),
  skip = 4,
  sheet = "EPAH"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = ifelse(
      is.na(pcode_com),
      df_ocha$adm2_pcode[match(adm2_name, df_ocha$adm2_name)],
      pcode_com
    ),
    sector = "wash",
    pin = as.numeric(x53),
    score = as.numeric(x56),
    source = "ocha",
    sector_general = "sectoral"
  )

# nutrition
df_cluster_nutrition <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - NUTRITION.xlsm"
  ),
  skip = 2,
  sheet = "PiN et sévérité 2022"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = df_ocha$adm1_name[match(
      admin_2_pour_les_zones_dinteret_affectees,
      df_ocha$adm2_name
    )],
    adm1_pcode = df_ocha$adm1_pcode[match(
      admin_2_pour_les_zones_dinteret_affectees,
      df_ocha$adm2_name
    )],
    adm2_name = admin_2_pour_les_zones_dinteret_affectees,
    adm2_pcode = df_ocha$adm2_pcode[match(
      admin_2_pour_les_zones_dinteret_affectees,
      df_ocha$adm2_name
    )],
    sector = "nutrition",
    pin = as.numeric(pi_n_2022_13),
    score = as.numeric(severite_2022),
    source = "ocha",
    sector_general = "sectoral"
  )

# child protection
df_cluster_cp <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - PROTECTION DE L'ENFANT.xlsx"
  ),
  skip = 4,
  sheet = "PROTECTION"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = pcode_com,
    sector = "child_protection",
    pin = as.numeric(x45),
    score = ifelse(pin == 0, 0, as.numeric(x46)),
    source = "ocha",
    sector_general = "sectoral"
  )

# protection migrants
df_cluster_mig <- read.csv(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - PROTECTION MIGRANTS.csv"
  ),
) %>%
  clean_names() %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = pcode_com,
    sector = "migrants",
    pin = parse_number(pin),
    score = ifelse(pin == 0, 0, as.numeric(severite)),
    source = "ocha",
    sector_general = "sectoral"
  )


# GBV protection
df_cluster_gbv <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - PROTECTION VBG.xlsx"
  ),
  skip = 3,
  sheet = "PIN"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = pcode_com,
    sector = "gbv",
    pin = as.numeric(pin),
    score = ifelse(pin == 0, 0, as.numeric(severite)),
    source = "ocha",
    sector_general = "sectoral"
  )

# Health
df_cluster_health <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - SANTE.xlsx"
  ),
  skip = 2,
  sheet = "Feuil1"
) %>%
  clean_names() %>%
  filter(!is.na(departement)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = dep_p_code,
    adm2_name = communes_prioritaires_en_orange,
    adm2_pcode = com_p_code,
    sector = "health",
    pin = as.numeric(pin),
    score = ifelse(
      pin == 0,
      0,
      as.numeric(ronde_utiliser_pour_repartition_du_pin_entre_niveaux)
    ),
    source = "ocha",
    sector_general = "sectoral"
  )




############################
#### Cluster PROVIDED DATA ####
############################

# education
df_cluster_edu <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Haiti - Education PiN & target calculation - 2022 HNO.xlsx"
  ),
  sheet = "140 communes"
) %>%
  clean_names() %>%
  filter(!is.na(code_dpt)) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = dept,
    adm1_pcode = code_dpt,
    adm2_name = commune,
    adm2_pcode = code_commune,
    sector = "education",
    pin = as.numeric(pin),
    score = NA_real_,
    source = "cluster",
    sector_general = "sectoral"
  )

########################
#### DATA WRANGLING ####
########################

df_all <- bind_rows(
  df_ocha,
  df_cluster_cp,
  df_cluster_fs,
  df_cluster_gbv,
  df_cluster_health,
  df_cluster_mig,
  df_cluster_nutrition,
  df_cluster_wash
)

write_csv(
  df_all,
  file_paths$save_path
)
c
