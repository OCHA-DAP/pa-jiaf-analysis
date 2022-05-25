library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Afghanistan")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "afg_hno_pin_2022_Clusters.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 4,
  sheet = "Total"
) %>%
  clean_names() %>%
  mutate(
    sector = "intersectoral"
  ) %>%
  rename(
    number_admin1_code = number_adm1_code,
    number_admin1_name = number_adm1_name,
    number_sector_name = number_sector
  )

df_clusters <- map_dfr(
  c("EDU", "SHL", "FSA", "HEA", "NUT", "PRO", "WSH"),
  ~ read_excel(
    ocha_fp,
    skip = 5,
    sheet = .x
  ) %>%
    clean_names() %>%
    mutate(sector = .x)
) %>%
  bind_rows()


########################
#### DATA WRANGLING ####
########################

# one code to bring them all
df_combined_all <- bind_rows(
  df_ocha_raw,
  df_clusters
)
df_combined_all <- df_combined_all %>%
  mutate(
    number_inneed_m_children = rowSums(
      df_combined_all[, grep("m_children_", names(df_combined_all))],
      na.rm = TRUE
    ),
    number_inneed_f_children = rowSums(
      df_combined_all[, grep("f_children_", names(df_combined_all))],
      na.rm = TRUE
    ),
    number_inneed_m_adult = rowSums(
      df_combined_all[, grep("m_adult_", names(df_combined_all))],
      na.rm = TRUE
    ),
    number_inneed_f_adult = rowSums(
      df_combined_all[, grep("f_adult_", names(df_combined_all))],
      na.rm = TRUE
    )
  ) %>%
  select(
    !c(
      number_inneed_f,
      number_inneed_children,
      number_inneed_adult,
      number_inneed_elderly,
      number_inneed_disabled
    )
  )

names(df_combined_all) <- gsub("_x_", "_", names(df_combined_all))

df_organized <- df_combined_all %>%
  pivot_longer(
    cols = matches("^number_inneed"),
    names_to = c(".value", "group"),
    names_pattern = "(^number_inneed)_(.*)"
  ) %>%
  mutate(
    population_group = gsub(
      "(^m_children_|^f_children_|^m_adult_|^f_adult_|^total_)",
      "",
      group
    ),
    age_gender_group = str_replace(
      string = group,
      pattern = paste0("_", population_group),
      ""
    ),
    population_group = ifelse(
      age_gender_group == population_group,
      "total",
      population_group
    ),
    adm0_pcode = "AFG",
    adm0_name = "Afghanistan",
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  filter(
    !grepl("total", group),
    number_admin1_name != "Total"
  ) %>%
  select(
    adm0_pcode,
    adm0_name,
    adm1_pcode = number_admin1_code,
    adm1_name = number_admin1_name,
    population_group,
    age_gender_group,
    sector,
    pin = number_inneed,
    source,
    sector_general
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm1_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

# deleting those age-sex groups that don't have any PiN for a specific sector
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_gender_group) %>%
  summarize(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_afg <- df_organized %>%
  filter(
    paste0(adm1_name, population_group) %in% paste0(
      df_summarized_pops$adm1_name,
      df_summarized_pops$population_group
    ),
    paste0(sector, age_gender_group) %in% paste0(
      df_summarized_age_sex$sector,
      df_summarized_age_sex$age_gender_group
    ),
    population_group != "total"
  ) %>%
  separate(age_gender_group, into = c("sex", "age")) %>%
  mutate(
    pin = round(pin, 0)
  )

write_csv(
  df_afg,
  file_paths$save_path
)
