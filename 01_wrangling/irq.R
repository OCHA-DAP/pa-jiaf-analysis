library(tidyverse)
library(readxl)
library(janitor)
library(zoo)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### FUNCTIONS ####
###################

#' @importFrom dplyr %>%
read_in_disagg <- function(fp) {
  groups <- c("In-Camp-IDPs", "Out-of-Camp-IDPs", "Returnees")
  purrr::map_dfr(
    groups,
    ~ readxl::read_excel(
      fp,
      sheet = paste0(.x, "-District"),
      skip = 4
    ) %>%
      dplyr::mutate(
        population_group = .x
      ) %>%
      dplyr::rename(
        adm1_pcode = admin1Pcode,
        adm2_pcode = admin2Pcode,
        adm1_en = gov_name,
        adm2_en = dist_name
      )
  )
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
  paste(
    "Iraq 2022 HNO Final Intersectoral",
    "& Cluster PIN Estimates - Updated 20211129.xlsx"
  )
)

df_ocha_raw <- read_in_disagg(ocha_fp)

########################
#### CREATE OCHA DF ####
########################

# Pivot to make clusters and PINs,
# drop strange empty columns
df_ocha <- df_ocha_raw %>%
  pivot_longer(
    cols = ends_with("pin"),
    names_to = c("sector", ".value"),
    names_sep = "_"
  ) %>%
  select(adm2_pcode, population_group, sector, pin) %>%
  drop_na(adm2_pcode) %>%
  mutate(
    source = "ocha"
  )

# Pcodes needed for IS table,
# but only admin 1
df_pcodes <- df_ocha_raw %>%
  select(starts_with("adm")) %>%
  distinct() %>%
  drop_na()

######################
#### CLUSTER DATA ####
######################

# Education
ed_fp <- file.path(
  file_paths$cluster_dir,
  "Iraq - Education PiN & JIAF calculation - 2022 HNO.xlsx"
)

df_ed <- read_in_disagg(ed_fp) %>%
  select(adm2_pcode, population_group, pin) %>%
  drop_na() %>%
  mutate(sector = "ed")

# WASH
wash_fp <- file.path(
  file_paths$cluster_dir,
  "WASH Iraq",
  "2022",
  "Iraq 2022 HNO Analysis - WASH Cluster - 5 Oct.xlsx"
)

df_wash <- read_in_disagg(wash_fp) %>%
  select(adm2_pcode, population_group, pin) %>%
  drop_na() %>%
  mutate(sector = "wash")

# Combine the clusters
df_clusters <- bind_rows(
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
  left_join(
    df_pcodes,
    by = "adm2_pcode"
  ) %>%
  relocate(adm1_en:adm2_en,
    .before = 1
  ) %>%
  mutate(
    adm0_pcode = "IRQ",
    adm0_en = "Iraq",
    .before = adm1_en,
  ) %>%
  mutate(
    sector_general = ifelse(
      sector == "itc",
      "intersectoral",
      "sectoral"
    )
  )

write_csv(
  df_irq,
  file_paths$save_path
)
