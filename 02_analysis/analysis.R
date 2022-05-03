library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

# Sample analysis to test our understanding
# of what needs to be done

jiaf_dir <- Sys.getenv("JIAF_DATA_DIR")
file_dir <- file.path(jiaf_dir, "Data aggregated for analysis")
save_path <- file.path(jiaf_dir, "Data analyzed")

###################
#### WRANGLING ####
###################

df <- map_dfr(list.files(file_dir, full.names = TRUE), read_csv) %>%
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

# Clean up output file for actual output
write_csv(
  pct_df,
  file.path(
    save_path,
    "2022_hno_pin_contributions.csv"
  )
)

write_csv(
  pin_df,
  file.path(
    save_path,
    "2022_hno_pin_totals.csv"
  )
)
