library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### WRANGLING ####
###################

df <- map_dfr(list.files(file_paths$input_dir, full.names = TRUE), read_csv) %>%
  select(
    -matches("adm[0-9]{1}_[a-z]{2}$")
  ) %>%
  mutate(
    sector_group = ifelse(
      source == "cluster",
      "sectoral_cluster",
      sector_general
    ),
    .before = pin
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
  df %>% mutate(sector = tolower(sector)) %>% #clean up cluster names -- maybe there's a better way
  mutate(
    sector = case_when(
      sector %in% c("cc", "cccm", "gsat") ~ "cccm",
      # gestion des Sites d’Accueil Temporaire
      sector %in% c(
        "early recovery",
        "early recovery & livelihoods",
        "rt",
        "el",
        "mpca"
      ) ~ "erl",
      #rt is early recovery, el is emergency livelihoods and mpca is Multi-Purpose Cash Assistance
      sector %in% c("education", "educacion", "educ", "ed") ~ "edu",
      sector %in% c(
        "fs",
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
      ) ~ "fsa",
      # securité alimentaire
      sector %in% c("sante" , "he", "health", "salud", "san", "hlt") ~ "hea",
      sector %in% c(
        "health & nutrition",
        "nutrition",
        "nutricion",
        "san_nutrition",
        "san_nutricion",
        "nut"
      ) ~ "nut",
      sector %in% c(
        "protection",
        "proteccion",
        "prot",
        "protection_general",
        "protection_aor",
        "prt"
      ) ~ "pro",
      sector %in% c("protection_cp", "ninez", "cp", "child_protection") ~ "pro-cp",
      sector %in% c("vbg", "protection_gbv", "pro-gen pro", "gbv", "gb", "gp") ~ "pro-gbv",
      sector %in% c("hlp", "ltb", "protection_hlp") ~ "pro-hlp",
      # Droit au Logement, à la Terre et aux Biens
      sector %in% c("ma", "minas", "lam") ~ "pro-ma",
      # lutte anti-mine
      sector %in% c(
        "abris",
        "alojamiento_energia_y_enseres",
        "alojamientos",
        "abris_ame",
        "snfi",
        "shelter",
        "shelter & nfis",
        "shleter&nfis",
        "shelter and nfi",
        "sn" ,
        "nfi"
      ) ~ "shl",
      sector == "el" ~ "livelihoods",
      sector %in% c("wash", "wa", "eha") ~ "wsh",
      sector %in% c("refugees", "refugee response", "migrants") ~ "displacement_status",
      sector %in% c("inter_sectoral", "intersectorial", "itc") ~ "intersectoral"
    )
  ) %>% filter(sector != "intersectoral")


# Clean up output file for actual output
write_csv(pct_df,
          file.path(file_paths$output_dir,
                    "2022_hno_pin_contributions.csv"))

write_csv(
  pin_df,
  file.path(
    file_paths$output_dir,
    "2022_hno_pin_totals.csv"
  )
)

write_csv(
  cluster_df,
  file.path(
    file_paths$output_dir,
    "2022_hno_pin_cluster_totals.csv"
  )
)
