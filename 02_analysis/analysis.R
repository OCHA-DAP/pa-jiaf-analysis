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

df %>%
  mutate(group = ifelse(
    sector == "intersectoral",
    "intersectoral",
    "sectoral"
  )) %>%
  group_by(
    group,
    adm3_pcode,
    population_group,
    sex,
    age
  ) %>%
  summarize(
    pin = max(pin),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  summarize(
    pin = sum(pin)
  )
