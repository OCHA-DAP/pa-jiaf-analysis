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
    cols = ends_with("pin") | ends_with("sev"),
    names_to = c("sector", ".value"),
    names_sep = "_"
  ) %>%
  select(adm2_pcode, population_group, sector, pin, severity = sev) %>%
  drop_na(adm2_pcode) %>%
  mutate(
    source = "ocha"
  )

# Pcodes needed for IS table,
# but only admin 1
df_pcodes <- df_ocha_raw %>%
  select(dplyr::starts_with("adm")) %>%
  distinct() %>%
  drop_na()

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_organized <- df_ocha %>%
  left_join(
    df_pcodes,
    by = "adm2_pcode"
  ) %>%
  transmute(
    adm0_name = "Iraq",
    adm0_pcode = "IRQ",
    adm1_name = adm1_en,
    adm1_pcode,
    adm2_name = adm2_en,
    adm2_pcode,
    population_group,
    sector,
    pin = round(pin),
    severity = ifelse(pin == 0, 1, severity),
    source = "ocha",
    sector_general = ifelse(
      sector == "itc",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  rename_at(
    dplyr::vars(ends_with("_en")),
    ~ str_replace(.x, "_en", "_name")
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized <- df_organized %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin)) %>%
  filter(tot_pin != 0)

df_irq <- df_organized %>% 
  filter(
    paste0(adm2_name, population_group) %in% paste0(df_summarized$adm2_name, df_summarized$population_group)
  )

write_csv(
  df_cleaned,
  file_paths$save_path
)
