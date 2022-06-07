library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### WRANGLING ####
###################

df <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_sectoral_pins.csv"
  )
)

# for analysis, create max df that has
# a row for each lowest admin level and
# non-missing pop group. Separate it by
# intersectoral, sectoral (OCHA), and
# sectoral (cluster provided).

max_df <- df %>%
  pivot_longer(
    matches("adm[1-4]+"),
    names_to = c(NA, "adm_level", NA),
    values_to = "adm_pcode",
    names_pattern = "(adm)([0-9]+)(_pcode)",
    names_transform = list(adm_level = as.numeric)
  ) %>%
  drop_na(adm_pcode) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    sector_group,
    sector
  ) %>%
  slice_max(adm_level) %>%
  unite(
    pop_group,
    any_of(
      c(
        "population_group",
        "sex",
        "age"
      )
    ),
    sep = ", ",
    na.rm = TRUE
  ) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    adm_pcode,
    pop_group,
    sector_group
  ) %>%
  summarize(
    lowest_adm_level = unique(adm_level),
    max_pin = max(pin),
    max_sector = paste(sector[pin == max_pin], collapse = ", "),
    .groups = "drop"
  )

# calculate contributions to PiN
# by number of times/% that a
# combination of sectors has
# been the max and the total
# and percent of PiN attributed
# to it
pct_df <- max_df %>%
  filter(
    sector_group != "intersectoral"
  ) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    sector_group,
    max_sector
  ) %>%
  summarize(
    pin = sum(max_pin),
    max_count = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    max_pct = scales::percent(max_count / sum(max_count), accuracy = 1),
    pin_pct = scales::percent(pin / sum(pin), accuracy = 1)
  ) %>%
  arrange(desc(pin), .by_group = TRUE) %>%
  ungroup()


# calculate final pin for each
# method and country
pin_df <- max_df %>%
  group_by(
    adm0_name,
    adm0_pcode,
    sector_group,
  ) %>%
  summarize(
    lowest_adm_level = unique(lowest_adm_level),
    number_disagg = grepl(
      "[^\\s]",
      pop_group[1]
    ) + str_count(pop_group[1], ","),
    pin = sum(max_pin),
    .groups = "drop"
  )

# Make a file with cleaned up cluster names
cluster_df <-
  df %>%
  mutate(sector = tolower(sector)) %>% # clean up cluster names
  mutate(
    sector = case_when(
      sector %in% c("cc", "cccm", "gsat") ~ "CCCM",
      # gestion des Sites d’Accueil Temporaire
      sector %in% c(
        "early recovery",
        "early recovery & livelihoods",
        "rt",
        "el",
        "mpca"
      ) ~ "ERL",
      # rt is early recovery, el is emergency livelihoods
      # and mpca is Multi-Purpose Cash Assistance
      sector %in% c(
        "education",
        "educacion",
        "educ",
        "edu",
        "ed"
      ) ~ "Education",
      sector %in% c(
        "fs",
        "fsa",
        "fss",
        "fsl",
        "fsc",
        "food",
        "food_sec",
        "fslc",
        "food security",
        "food_security",
        "seguridad_alimentaria",
        "san_seguridad_alimentaria",
        "seg_alimentaria",
        "secal"
      ) ~ "FS/FSL",
      # securité alimentaire
      sector %in% c(
        "sante",
        "he",
        "health",
        "salud",
        "san",
        "hlt",
        "heat"
      ) ~ "Health",
      sector %in% c(
        "health & nutrition",
        "nutrition",
        "nutricion",
        "san_nutrition",
        "san_nutricion",
        "nut"
      ) ~ "Nutrition",
      sector %in% c(
        "protection",
        "proteccion",
        "prot",
        "pro",
        "protection_general",
        "protection_aor",
        "prt"
      ) ~ "Protection",
      sector %in% c(
        "protection_cp",
        "ninez",
        "cp",
        "child_protection"
      ) ~ "Protection (CP)",
      sector %in% c(
        "vbg",
        "protection_gbv",
        "pro-gen pro",
        "gbv",
        "gb",
        "gp"
      ) ~ "Protection (GBV)",
      sector %in% c("hlp", "ltb", "protection_hlp") ~ "Protection (HLP)",
      # Droit au Logement, à la Terre et aux Biens
      sector %in% c("ma", "minas", "lam") ~ "Protection (MA)",
      # lutte anti-mine
      sector %in% c(
        "abris",
        "alojamiento_energia_y_enseres",
        "alojamientos",
        "abris_ame",
        "shl",
        "snfi",
        "shelter",
        "shelter & nfis",
        "shleter&nfis",
        "shelter and nfi",
        "sn",
        "nfi"
      ) ~ "shl",
      sector %in% c("wash", "wa", "wsh", "eha") ~ "WASH",
      sector %in% c(
        "refugees",
        "refugee response",
        "migrants"
      ) ~ "Displaced pop.",
      sector %in% c(
        "inter_sectoral",
        "intersectorial",
        "intersectoral",
        "itc"
      ) ~ "intersectoral"
    )
  ) %>%
  filter(sector != "intersectoral")


# Clean up output file for actual output
write_csv(
  pct_df,
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_contributions.csv"
  )
)

write_csv(
  pin_df,
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
)

write_csv(
  cluster_df,
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
)
