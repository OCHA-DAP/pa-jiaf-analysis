library(tidyverse)
library(readxl)
library(janitor)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Somalia")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "combined_cluster_pin_hxl.xlsx"
)

df_ocha_raw <- read_excel(
  ocha_fp,
  skip = 3,
  sheet = "All PiN"
) %>%
  remove_empty("cols") %>%
  clean_names()

df_pcodes <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "som-administrative-division-names-and-p-codes.xlsx"
  ),
  sheet = "Admin2"
) %>%
  select(
    adm1_pcode = admin1Pcode,
    adm1_name = admin1Name_en,
    adm2_pcode = admin2Pcode,
    adm2_name = admin2Name_en,
  )

df_population1 <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "Copy of Population 15.7.xlsx"
  ),
  sheet = "Sheet1",
  skip = 5
) %>%
  clean_names() %>%
  filter(!is.na(district)) %>%
  transmute(
    adm2_name = district,
    displaced = of_whom_id_ps,
    non_displaced = of_whom_non_displaced
  ) %>%
  pivot_longer(
    cols = -adm2_name,
    values_to = "affected_population",
    names_to = "population_group"
  )

df_population2 <- map_dfr(
  c("Refugees", "Returnees"),
  ~ read_excel(
    file.path(
      file_paths$ocha_dir,
      "hno-2022-refugees-and-returnees.xlsx"
    ),
    sheet = .x,
    skip = 1
  ) %>%
    clean_names() %>%
    filter(!is.na(district)) %>%
    transmute(
      adm2_name = district,
      population_group = tolower(.x),
      affected_population = grand_total
    )
)

df_population <- rbind(
  df_population1,
  df_population2
) %>%
  mutate(
    adm2_name = case_when( # since no pcodes, have to match CODAB file
      adm2_name == "Kismayo" ~ "Kismaayo",
      adm2_name == "Garowe" ~ "Garoowe",
      adm2_name == "Bandarbayla" ~ "Bandarbeyla",
      adm2_name == "Baidoa" ~ "Baydhaba",
      adm2_name == "Xardheere" ~ "Xarardheere",
      TRUE ~ adm2_name
    )
  )
########################
#### DATA WRANGLING ####
########################

df_severity <- df_ocha_raw %>%
  rename(
    number_inneed_shelter_returnees = x69,
    number_inneed_shelter_refugees = x70
  ) %>%
  pivot_longer(
    cols = matches("severity"),
    names_to = "sector",
    values_to = "score"
  ) %>%
  select(
    number_adm2_name,
    sector,
    score
  ) %>%
  mutate(
    sector = gsub("number_inneed_|_severity|_[0-9]+", "", sector)
  )


df_pin <- df_ocha_raw %>%
  rename(
    number_inneed_shelter_returnees = x69,
    number_inneed_shelter_refugees = x70
  ) %>%
  pivot_longer(
    cols = !matches("severity|adm"),
    names_to = "sector",
    values_to = "pin"
  ) %>%
  select(
    number_adm2_name,
    sector,
    pin
  ) %>%
  mutate(
    sector = gsub("number_inneed_|_severity|_[0-9]+", "", sector),
    population_group = case_when(
      grepl("non_displaced", sector) ~ "non_displaced",
      grepl("total", sector) ~ "total",
      grepl("displaced", sector) ~ "displaced",
      grepl("returnees", sector) ~ "returnees",
      grepl("refugees", sector) ~ "refugees"
    ),
    sector = str_replace(sector, paste0("_", population_group), "")
  )

df_organized <-
  left_join(
    df_pin,
    df_severity,
    by = c("number_adm2_name", "sector")
  ) %>%
  filter(population_group != "total") %>%
  transmute(
    adm0_name = "Somalia",
    adm0_pcode = "SOM",
    adm2_name = str_replace(
      number_adm2_name,
      "_",
      " "
    ),
    adm2_name = case_when( # since no pcodes, have to match CODAB file
      adm2_name == "Kismayo" ~ "Kismaayo",
      adm2_name == "Garowe" ~ "Garoowe",
      adm2_name == "Bandarbayla" ~ "Bandarbeyla",
      adm2_name == "Baidoa" ~ "Baydhaba",
      adm2_name == "Xardheere" ~ "Xarardheere",
      TRUE ~ adm2_name
    ),
    population_group,
    affected_population = df_population$affected_population[match(
      paste0(adm2_name, population_group),
      paste0(df_population$adm2_name, df_population$population_group)
    )],
    sector = ifelse(sector == "inter_sectoral", "intersectoral", sector),
    pin = round(pin),
    severity = ifelse(pin == 0, 1, score),
    source = "ocha",
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  left_join(
    df_pcodes,
    by = c("adm2_name")
  ) %>%
  relocate(
    adm1_pcode:adm2_pcode,
    .before = adm2_name
  ) %>%
  mutate(
    affected_population = replace_na(affected_population, 0)
  ) %>%
  group_by(
    adm2_pcode,
    population_group
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin, max(pin), affected_population),
    affected_population = max(affected_population)
  ) %>%
  ungroup()



# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_som <- df_organized %>%
  filter(
    paste0(adm2_name, population_group) %in% paste0(
      df_summarized_pops$adm2_name,
      df_summarized_pops$population_group
    ),
    population_group != "total"
  )

write_csv(
  df_som,
  file_paths$save_path
)
