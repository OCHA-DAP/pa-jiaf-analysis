library(tidyverse)

# Sample analysis to test our understanding
# of what needs to be done

df <- read_csv(
  file.path(
    Sys.getenv("JIAF_DATA_DIR"),
    "Data aggregated for analysis",
    "lby_pins_2022.csv"
  )
)

max_df <- df %>%
  mutate(group = case_when(
    sector == "intersectoral" ~ "intersectoral",
    source == "ocha" ~ "sectoral",
    source == "cluster" ~ "sectoral_cluster"
  )) %>%
  group_by(
    group,
    adm3_pcode,
    population_group,
    sex,
    age
  ) %>%
  summarize(
    max_pin = max(pin),
    max_sector = paste(sector[pin == max_pin], collapse = ", "),
    .groups = "drop"
  ) 

# analysis example

df_sample <- max_df %>%
  group_by(group) %>%
  summarize(
    pin = sum(max_pin),
    .groups = "drop"
  )

df_sample

# driving sectors

df_drivers <- max_df %>%
  filter(group != "intersectoral") %>%
  group_by(group, max_sector) %>%
  summarize(
    max_count = n(),
    pin = sum(max_pin),
    .groups = "drop_last"
  ) %>%
  arrange(desc(pin), .by_group = TRUE) %>%
  mutate(
    max_count_pct = scales::percent(max_count / sum(max_count), accuracy = 1),
    pin_pct = scales::percent(pin / sum(pin), accuracy = 1)
  )

# df_drivers %>%
#   write_csv(
#     file.path(
#       Sys.getenv("JIAF_DATA_DIR"),
#       "Data analyzed",
#       "lby_sample_drivers.csv"
#     )
#   )
