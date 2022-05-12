library(tidyverse)
library(readxl)

###################
#### DATA DIRS ####
###################

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths("Sudan")

############################
#### OCHA PROVIDED DATA ####
############################

df_ocha_raw <- read_excel(
  file.path(
    file_paths$ocha_dir,
    "SDN HNO 2022 Baseline -JIAF exercise.xlsx"
  ),
  sheet = "PIN and Severity",
  skip = 6
) %>%
  rename_all(tolower) %>%
  rename(adm2_pcode = pcode) %>%
  drop_na(adm2_pcode)

df_ocha_clusters <- df_ocha_raw %>%
  select(-c(pin, sev)) %>%
  pivot_longer(
    cols = contains("_pin_") | contains("_sev_"),
    names_to = c("sector", ".value", "condition", "population_group"),
    names_sep = "_",
  ) %>%
  select(
    adm2_pcode, sector, population_group, condition, pin, sev
  ) %>%
  mutate(
    pin = replace_na(pin, 0),
    sev = replace_na(sev, 0)
  ) %>%
  group_by(adm2_pcode, sector, population_group) %>% # take max by condition
  summarize(
    pin = max(pin),
    severity = max(sev),
    .groups = "drop"
  )

df_ocha_is <- df_ocha_raw %>%
  select(
    adm2_pcode,
    matches("^(pin|sev)_(LT|LS)_[a-z]{3}$")
  ) %>%
  pivot_longer(
    -adm2_pcode,
    names_to = c(".value", "condition", "population_group"),
    names_sep = "_"
  ) %>%  
  mutate(
    pin = replace_na(pin, 0),
    sev = replace_na(sev, 0)
  ) %>%
  group_by(adm2_pcode, population_group) %>% # take max by condition
  summarize(
    pin = max(pin),
    severity = max(sev),
    .groups = "drop"
  ) %>%
  mutate(
    sector = "intersectoral"
  )

df_ocha <- bind_rows(
  df_ocha_clusters,
  df_ocha_is
) %>%
  mutate(source = "ocha", .before = 1,
         pin = ifelse(pin == -Inf, 0, round(pin)),
         severity = ifelse(pin == 0, 1, severity))

######################
#### CLUSTER DATA ####
######################

# Education file used for PCODES
df_edu_raw <- read_excel(
  file.path(
    file_paths$cluster_dir,
    "Sudan - Education PiN calculation - 2022 HNO.xlsx"
  ),
  sheet = "EDU", skip = 15
) %>%
  rename_all(tolower)

###############
### PCODES ####
###############

# OCHA data missing admin1 pcodes, use the edu data instead
# There is an error is most pcodes where Abyei PCA area is
# referred to as SD19101 but in the HDX pcodes, it is
# actually code SD19001 (GBV data is correct)

df_pcodes <- df_edu_raw %>%
  transmute(
    adm1_en = state,
    adm1_pcode = `p-code admin1`,
    adm2_en = locality,
    adm2_pcode = ifelse(
      `pcode admin2` == "SD19101",
      "SD19001",
      `pcode admin2`
    )
  ) %>%
  unique()

############################
#### GENERATE FULL DATA ####
############################

df_organized <- df_ocha %>%
  mutate(adm2_pcode = ifelse(
    adm2_pcode == "SD19101",
    "SD19001",
    adm2_pcode
  )) %>%
  left_join(df_pcodes,
            by = "adm2_pcode",
  ) %>%
  transmute(
    adm0_name = "Sudan",
    adm0_pcode = "SDN",
    adm1_name = adm1_en,
    adm1_pcode,    
    adm2_name = adm2_en,
    adm2_pcode,
    population_group,
    sector, 
    pin = round(pin),
    severity,
    source = "ocha",
    sector_general = ifelse(sector == "intersectoral",
                            "intersectoral",
                            "sectoral")
  )
 
# deleting those areas that don't have any PiN for a specific group
df_summarized_pops <- df_organized %>%
  group_by(adm2_name, population_group) %>%
  summarise(tot_pin = sum(pin, na.rm = T)) %>%
  filter(tot_pin != 0)

df_sdn <- df_organized %>% 
  filter(
    paste0(adm2_name, population_group) %in% paste0(
      df_summarized_pops$adm2_name,
      df_summarized_pops$population_group
    )
  )

write_csv(
  df_sdn,
  file_paths$save_path
)
