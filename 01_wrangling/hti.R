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
    affected_population = population,
    sector = "intersectoral",
    severity = round(vc),
    severity_unadjusted = round(vs),
    pin = as.numeric(pi_n),
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
    severity = round(x18),
    pin = as.numeric(pin),
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
    severity = as.numeric(x56),
    pin = as.numeric(x53),
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
    severity = as.numeric(severite_2022),
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
    severity = as.numeric(x46),
    source = "ocha",
    sector_general = "sectoral"
  )

# protection migrants
df_cluster_mig <- read.csv(
  file.path(
    file_paths$ocha_dir,
    "HTI_HNO 2022 - PROTECTION MIGRANTS.csv"
  )
)

names(df_cluster_mig) <-
  stringi::stri_trans_general(
    names(df_cluster_mig),
    "ASCII"
  )

df_cluster_mig <- df_cluster_mig %>%
  clean_names() %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = i_da_partement,
    adm1_pcode = pcode_dep,
    adm2_name = commune,
    adm2_pcode = pcode_com,
    sector = "migrants",
    pin = parse_number(pin),
    severity = as.numeric(severite),
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
    severity = as.numeric(severite),
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
    severity = as.numeric(ronde_utiliser_pour_repartition_du_pin_entre_niveaux),
    source = "ocha",
    sector_general = "sectoral"
  )

df_indicator <- read_excel(ocha_fp,
  skip = 5,
  sheet = "Ind data"
) %>%
  clean_names() %>%
  filter(row_number() > 17)

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
    affected_population = pop,
    sector = "education",
    pin = as.numeric(pin),
    severity = NA_real_,
    source = "cluster",
    sector_general = "sectoral"
  )

########################
#### DATA WRANGLING ####
########################

df_hti <- bind_rows(
  df_ocha,
  df_cluster_cp,
  df_cluster_fs,
  df_cluster_gbv,
  df_cluster_health,
  df_cluster_mig,
  df_cluster_nutrition,
  df_cluster_wash
) %>%
  mutate(affected_population = ifelse(
    is.na(affected_population),
    df_ocha$affected_population[match(
      adm2_pcode,
      df_ocha$adm2_pcode
    )],
    affected_population
  ))

df_hti_sev <- df_hti %>%
  bind_rows(
    df_hti %>%
      filter(sector == "intersectoral") %>%
      mutate(
        severity = severity_unadjusted,
        sector = "intersectoral_unadjusted"
      )
  ) %>%
  select(-severity_unadjusted)

df_hti <- df_hti %>%
  select(-severity_unadjusted)

df_hti_cleaning <- df_indicator %>%
  select(-matches("^x[1-5]"), -c(
    disaster_risk,
    insecurity,
    access,
    population
  ))

indicator_names <- data.frame(cur_name = names(df_hti_cleaning)[5:46]) %>%
  mutate(new_names = ifelse(
    row_number() %% 2 == 1, # reminder of 2, if 1 it's odd
    paste0(cur_name, ".severity"),
    paste0(lag(cur_name), ".pin")
  ))

names(df_hti_cleaning)[5:46] <- indicator_names$new_names

df_hti_indicator <- df_hti_cleaning %>%
  pivot_longer(
    cols = matches("pin$|severity$"),
    names_to = c("indicator_desc", ".value"),
    names_pattern = "(.*).(pin|severity)"
  ) %>%
  mutate(indicator_number = as.numeric(factor(indicator_desc))) %>%
  transmute(
    adm0_name = "Haiti",
    adm0_pcode = "HTI",
    adm1_name = departement,
    adm1_pcode = dep_p_code,
    adm2_name = commune,
    adm2_pcode = com_p_code,
    indicator_number,
    critical = indicator_number %in% c(2, 3, 5, 7, 9, 10, 12:16, 21),
    indicator_desc,
    pin,
    severity,
    sector = case_when(
      indicator_number %in% c(1, 3, 6, 20) ~ "Protection (MA)",
      indicator_number == 2 ~ "WASH",
      indicator_number %in% c(4, 9, 15, 19) ~ "Health",
      indicator_number == 5 ~ "Shelter",
      indicator_number %in% 7:8 ~ "Education",
      indicator_number %in% c(10, 17) ~ "Nutrition",
      indicator_number %in% c(11, 21) ~ "Protection (GBV)",
      indicator_number == 12 ~ "FS/FSL",
      indicator_number %in% 13:14 ~ "Protection",
      indicator_number %in% c(16, 18) ~ "Protection (CP)"
    )
  )

write_csv(
  df_hti,
  file_paths$save_path
)

write_csv(
  df_hti,
  file_paths$save_path_sev
)

write_csv(
  df_hti_indicator,
  file_paths$save_path_indicator
)

write_csv(
  df_hti_indicator,
  file_paths$save_path_indicator_sev
)
