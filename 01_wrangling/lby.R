library(tidyverse)
library(readxl)
library(janitor)

###################
#### DATA DIRS ####
###################

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths("Libya")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  "libya-2022-hpc-intersectoral-and-sectoral-targets-and-pin-2022_15nov2021.xlsx" # nolint
)

df_ocha_clusters_raw <- read_excel(
  ocha_fp,
  sheet = "Consolidated PiN 2022 (sectors)"
)

df_ocha_is_raw <- read_excel(ocha_fp, sheet = "Intersectoral PiN 2022")

df_adm1 <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "lby_adminboundaries_tabulardata.xlsx"
  ),
  sheet = "Admin1"
) %>%
  select(
    adm1_pcode = admin1Pcode,
    adm1_name = admin1Name_en
  )

indicator_fp <- file.path(
  file_paths$ocha_dir,
  "Libya - Aggregation File 9Sep2021 HNO 2022.xlsx" # nolint
)

df_indicators <- read_excel(
  indicator_fp,
  skip = 2,
  sheet = "Step 4-Long Data"
) %>%
  clean_names() %>%
  filter(pcode != "Total")

df_population <- read_excel(
  indicator_fp,
  sheet = "Baseline Population"
) %>%
  clean_names() %>%
  filter(mantika_pcode != "Total") %>%
  pivot_longer(
    cols = male_below_18:female_60,
    names_to = "sex_age",
    values_to = "affected_population"
  ) %>%
  separate(sex_age, c("sex", "age"), sep = "_", extra = "merge")

# severity datasets
df_cluster_sev <- df_ocha_clusters_raw %>%
  clean_names() %>%
  transmute(
    adm3_name = balad_adm3,
    adm3_pcode,
    sector,
    population_group,
    pin = pi_n,
    severity
  )

df_is_sev <- read_excel(
  ocha_fp,
  sheet = "Severity"
) %>%
  clean_names() %>%
  transmute(
    adm3_name = adm3_baladiya,
    sector = "intersectoral",
    population_group = "Non-displaced", # temporarly for matching
    severity
  )

########################
#### DATA WRANGLING ####
########################
# one code to bring them all

# saving pcodes and names to ensure unique names at end

df_pcodes <- df_ocha_clusters_raw %>%
  select(
    adm2_name = `ADM2_Manti (EN)`,
    adm3_name = Balad_ADM3,
    ends_with("Pcode")
  ) %>%
  rename_with(tolower) %>%
  distinct()

# intersectoral pins

df_ocha_is <- df_ocha_is_raw %>%
  clean_names() %>%
  slice(-1) %>%
  type_convert() %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  left_join(
    df_pcodes,
    by = c("mantika" = "adm2_name", "baladiya" = "adm3_name")
  ) %>%
  select(-key, -mantika, -baladiya) %>%
  mutate(
    source = "ocha",
    sector = "intersectoral",
    .before = 1
  )

# ocha provided cluster pins

df_ocha_clusters <- df_ocha_clusters_raw %>%
  clean_names() %>%
  select(
    sector,
    severity,
    ends_with("pcode"),
    population_group,
    matches("male")
  ) %>%
  pivot_longer(
    matches("male"),
    names_to = c("sex", "age"),
    names_sep = "(?<=e)_",
    values_to = "pin"
  ) %>%
  mutate(
    source = "ocha",
    .before = 1
  )

############################
#### GENERATE FULL DATA ####
############################
# and in the darkness bind them

df_organized <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  left_join(
    df_pcodes,
    by = c("adm2_pcode", "adm3_pcode")
  ) %>%
  select(
    source:adm2_pcode,
    adm2_name,
    adm3_pcode,
    adm3_name,
    population_group:pin
  ) %>%
  mutate(
    adm0_pcode = "LBY",
    adm0_name = "Libya",
    adm1_pcode = substr(adm2_pcode, 1, 4),
    .before = adm2_pcode
  ) %>%
  mutate(
    sector_general = ifelse(
      sector == "intersectoral",
      "intersectoral",
      "sectoral"
    )
  ) %>%
  left_join(
    df_adm1,
    by = c("adm1_pcode")
  ) %>%
  relocate(
    adm1_name,
    .before = adm2_pcode
  )

# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm3_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

# deleting those age-sex groups that don't have any PiN for a specific sector
df_summarized_age_sex <- df_organized %>%
  group_by(sector, age_sex = paste0(age, sex)) %>%
  summarize(tot_pin = sum(pin, na.rm = TRUE)) %>%
  filter(tot_pin != 0)

df_lby <- df_organized %>%
  filter(
    paste0(adm3_name, population_group) %in% paste0(
      df_summarized_pops$adm3_name,
      df_summarized_pops$population_group
    ),
    paste0(sector, age, sex) %in% paste0(
      df_summarized_age_sex$sector,
      df_summarized_age_sex$age_sex
    )
  ) %>%
  left_join(
    df_population %>% select(
      adm3_pcode = baladiya_p_code,
      population_group,
      sex,
      age,
      affected_population
    )
  ) %>%
  mutate(
    pin = round(pin),
    affected_population = round(affected_population)
  ) %>%
  # the population figures were not totally accurate
  # decided to use the max pin for those with zero population or lower than pin
  group_by(
    adm3_pcode,
    population_group,
    sex,
    age
  ) %>%
  mutate(
    affected_population =
      ifelse(affected_population < pin, max(pin), affected_population),
    affected_population = max(affected_population)
  ) %>%
  ungroup() %>%
  select(-severity)

temp <- df_lby %>%
  group_by(adm3_name, adm3_pcode) %>%
  summarize(
    pin = sum(pin),
    affected_population = sum(affected_population)
  )

df_is_sev <- df_is_sev %>%
  left_join(temp)

temp <- df_lby %>%
  group_by(
    adm3_name,
    adm3_pcode,
    population_group
  ) %>%
  summarize(
    affected_population = sum(affected_population),
    .groups = "drop"
  )

df_lby_sev <- df_cluster_sev %>%
  left_join(temp) %>%
  bind_rows(df_is_sev)

temp <- df_lby %>%
  select(
    adm0_pcode,
    adm0_name,
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    adm3_pcode
  ) %>%
  unique()

df_lby_sev <- df_lby_sev %>%
  left_join(temp) %>%
  mutate(
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral",
      "intersectoral",
      "sectoral"
    ),
    population_group = ifelse(sector == "intersectoral",
      "overall",
      population_group
    )
  ) %>%
  filter(severity > 0)

df_lby_indicator <- df_indicators %>%
  separate(
    col = key,
    into = c("adm3_name", "population_group"),
    sep = "-", extra = "merge"
  ) %>%
  left_join(
    df_lby %>%
      select(
        starts_with("adm")
      ) %>%
      unique(),
    by = c("pcode" = "adm3_pcode", "adm3_name")
  ) %>%
  # indicator 19 is all blank caused by an error from the file
  filter(indicator_number != 19) %>%
  transmute(
    adm0_name = "Libya",
    adm0_pcode = "LBY",
    adm1_name,
    adm1_pcode,
    adm2_name,
    adm2_pcode,
    adm3_name,
    adm3_pcode = ifelse(
      adm3_name == "Misrata", # fix error in one of the rows
      "LY021405",
      pcode
    ),
    population_group,
    indicator_number = ifelse(
      indicator_number > 19,
      indicator_number - 1,
      indicator_number
    ),
    critical = critical_status == "Yes",
    indicator_desc = indicator_text,
    pin = round(calculated_pi_n),
    severity = calculated_severity
  )

write_csv(
  df_lby,
  file_paths$save_path
)

write_csv(
  df_lby_sev,
  file_paths$save_path_sev
)

write_csv(
  df_lby_indicator,
  file_paths$save_path_indicator
)

write_csv(
  df_lby_indicator %>% filter(severity > 0),
  file_paths$save_path_indicator_sev
)
