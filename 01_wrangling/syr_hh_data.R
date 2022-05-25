library(tidyverse)
library(readxl)
library(janitor)
library(expss)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Syria")

############################
#### MSNA Indicator DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "Syria IS_PiN_2022_master_SHARE TO HQ.xlsx"
)

df <- read_excel(ocha_fp,
  sheet = "aggregation per HH",
  skip = 1
) %>%
  clean_names() %>%
  type_convert() %>%
  select(
    hh_id = id,
    area,
    population_group,
    reduced_coping_strategies_index:
    percent_of_id_ps_and_returnees_vis_a_vis_host_population,
    weight
  ) %>%
  filter(!is.na(hh_id))

names(df) <- c(
  "hh_id", "area",
  "population_group",
  1:19,
  "weight"
)

df_syr_pops <- read_excel(ocha_fp,
  sheet = "Master",
  skip = 3
) %>%
  clean_names() %>%
  transmute(
    adm0_name = "Syria",
    area = admin3pcode,
    res = final_est_of_res_pop_aug_2021,
    ret = final_est_of_spontaneous_idp_returnees_jan_aug_2021,
    idp_out = final_est_of_total_id_ps_aug_2021 -
      final_est_of_id_ps_in_sites_aug_2021_included_in_the_total_id_ps,
    idp_in = final_est_of_id_ps_in_sites_aug_2021_included_in_the_total_id_ps
  ) %>%
  pivot_longer(
    cols = -c(adm0_name, area),
    values_to = "target_population",
    names_to = "population_group"
  ) %>%
  filter(target_population > 0)

df_cleaned <- df %>%
  mutate(
    hh_id = paste0("SYR", row_number()),
    population_group = case_when(
      population_group == "IDPs out of camps" ~ "idp_out",
      population_group == "IDPs in camps" ~ "idp_in",
      population_group == "Returnees" ~ "ret",
      population_group == "Residents" ~ "res"
    )
  ) %>%
  pivot_longer(
    cols = matches("^[0-9]"),
    names_to = "indicator",
    values_to = "severity",
    values_drop_na = TRUE
  ) %>%
  left_join(df_syr_pops, by = c("area", "population_group")) %>%
  transmute(
    hh_id,
    adm0_name = "Syria",
    area,
    population_group,
    target_population = round(target_population),
    sector = case_when(
      indicator %in% c(7, 8, 11, 15, 17) ~ "protection",
      indicator == 16 ~ "education",
      indicator == 19 ~ "CCCM",
      indicator %in% c(3, 4) ~ "emergency recovery",
      indicator %in% c(1, 2) ~ "food security",
      indicator == 12 ~ "health",
      indicator %in% c(9, 18) ~ "Shelter",
      indicator == 10 ~ "NFI",
      indicator %in% c(13, 14) ~ "Nutrition",
      indicator %in% c(5, 6) ~ "WASH"
    ),
    indicator,
    severity,
    weight
  ) %>%
  # excluding 16 surveys in an area that is not targetted
  filter(!is.na(target_population))

write_csv(
  df_cleaned,
  file_paths$save_path_hh_data
)
