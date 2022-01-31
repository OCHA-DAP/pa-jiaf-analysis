library(tidyverse)
library(readxl)
library(janitor)
library(zoo)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### FUNCTIONS ####
###################

read_in_disagg <- function(filename) {
  df_in_camp_idps <- read_excel(
    filename,
    sheet = "In-Camp-IDPs-District",
    skip = 4
  ) %>%
    mutate(population_group = "In-Camp IDPs")
  df_out_of_camp_idps <- read_excel(
    filename,
    sheet = "Out-of-Camp-IDPs-District",
    skip = 4
  ) %>%
    mutate(population_group = "Out-of-Camp IDPs")
  df_returnees <- read_excel(
    filename,
    sheet = "Returnees-District",
    skip = 4
  ) %>%
    mutate(population_group = "Returnees")
  df_overall <- read_excel(
    filename,
    sheet = "Overall-District",
    skip = 4
  ) %>%
    mutate(population_group = "Overall")

  df <- bind_rows(
    df_in_camp_idps,
    df_out_of_camp_idps,
    df_returnees,
    df_overall
  ) %>%
    rename(
      adm1_pcode = admin1Pcode, adm2_pcode = admin2Pcode,
      adm1_en = gov_name, adm2_en = dist_name
    )
  return(df)
}

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Iraq")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Iraq 2022 HNO Final Intersectoral & Cluster PIN Estimates - Updated 20211129.xlsx"
)

df_ocha_clusters_raw <- read_in_disagg(ocha_fp)

# Needs some cleaning of the header
# https://paul.rbind.io/2019/02/01/tidying-multi-header-excel-data-with-r/

df_ocha_is_raw <- read_excel(ocha_fp,
  col_names = TRUE, sheet = "Gov. PIN & AcutePIN", skip = 2
)

head1 <- names(df_ocha_is_raw) %>%
  str_replace("...\\d", NA_character_) %>%
  zoo::na.locf0()

head2 <- df_ocha_is_raw[1, ] %>%
  unlist(use.names = F)

headers <- ifelse(
  !is.na(head1),
  paste(head1, head2, sep = "_"),
  head2
)

df_ocha_is_raw <- df_ocha_is_raw %>%
  rename_with(~headers) %>%
  slice(-1) %>%
  type_convert() %>%
  mutate(sector = "intersectoral")

########################
#### CREATE OCHA DF ####
########################

# Pivot to make clusters and PINs,
# drop strange empty columns
df_ocha_clusters <- df_ocha_clusters_raw %>%
  pivot_longer(
    cols = ends_with("pin") | ends_with("acute") | ends_with("sev"),
    names_to = c("sector", ".value"),
    names_pattern = "(\\w+)_(\\w+)"
  ) %>%
  select(-c(mcna, pop_num, pop_sch, `pop_0-17`, acute, sev)) %>%
  drop_na(adm1_en) %>%
  mutate(adm1_en = na_if(adm1_en, "Total"))

# Pcodes needed for IS table,
# but only admin 1
df_pcodes <- df_ocha_clusters %>%
  select(adm1_pcode, adm1_en) %>%
  distinct() %>%
  drop_na()

# Pivoting the IS table
df_ocha_is <- df_ocha_is_raw %>%
  pivot_longer(
    cols = ends_with("Population") | ends_with("PIN") | ends_with("Acute PIN"),
    names_to = c("population_group", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  rename(pin = PIN, adm1_en = Governorate) %>%
  left_join(df_pcodes, by = "adm1_en") %>%
  select(-c(Population, `Acute PIN`)) %>%
  mutate(adm1_en = na_if(adm1_en, "Total"))

# Create final OCHA set
df_ocha <- bind_rows(df_ocha_clusters, df_ocha_is) %>%
  mutate(source = "ocha")

######################
#### CLUSTER DATA ####
######################

# Food security
# Duplicates per admin 1 - sort by date and take the latest
df_fs <- read_csv(
  file.path(
    file_paths$cluster_dir,
    "Iraq Food Security & Malnutrition Rates - Food Security @ Governorate Level.csv"
  )
) %>%
  slice(-1) %>%
  rename(
    adm1_pcode = PCode,
    pin = `Insufficient food consumption`
  ) %>%
  mutate(date = as.Date(Date, format = "%d/%m/%Y"), 
         sector = "fs",
         pin = as.numeric(gsub(",", "", pin))) %>%
  arrange(desc(date)) %>%
  distinct(adm1_pcode, .keep_all = TRUE) %>%
  select(adm1_pcode, pin) %>%
  left_join(df_pcodes, by = "adm1_pcode")

# Education
ed_fp <- file.path(
  file_paths$cluster_dir,
  "Iraq - Education PiN & JIAF calculation - 2022 HNO.xlsx"
)

df_ed <- read_in_disagg(ed_fp) %>%
  select(adm1_pcode, adm1_en, adm2_pcode, adm2_en, pin) %>%
  drop_na() %>%
  mutate(sector = "ed")

# WASH
wash_fp <- file.path(
  file_paths$cluster_dir,
  "WASH Iraq",
  "2022",
  "Iraq 2022 HNO Analysis - WASH Cluster - 5 Oct.xlsx"
)
# It looks like the Overall tab final PIN is in pin2
df_wash <- read_in_disagg(wash_fp) %>%
  mutate(pin = ifelse(population_group == "Overall",
    pin2,
    pin
  )) %>%
  select(adm1_pcode, adm1_en, adm2_pcode, adm2_en, pin) %>%
  drop_na() %>%
  mutate(sector = "wash")

# Combine the clusters
df_clusters <- bind_rows(
  df_fs,
  df_ed,
  df_wash
) %>%
  mutate(source = "cluster", .before = 1)

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them


df_irq <- bind_rows(
  df_ocha,
  df_clusters
) %>%
  mutate(
    adm0_pcode = "IRQ",
    adm0_en = "irq",
    .before = adm1_en,
  )

# write_csv(
#   df_irq,
#   file_paths$save_path
# )
