library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

# Sample analysis to test our understanding
# of what needs to be done
analyze_pins <- function(country_name, iso3) {
  
file_paths <- get_paths(country_name)
df <- read_csv(file_paths$save_path)

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
  filter(pin == max(pin))

df_sample <- max_df %>%
  group_by(group, sector) %>%
  summarize(
    pin = sum(pin),
    .groups = "drop"
  ) %>%
  add_row(group = "sectoral", sector = "intersectoral", 
          pin = df_sample %>% filter(group == "sectoral") %>%
            summarize(sum(pin)) %>% pull ) %>%
  arrange(group, desc(pin))

df_sample %>%
   write_csv(
     file.path(
       Sys.getenv("JIAF_DATA_DIR"),
       "Data analyzed",
       paste0(iso3, "_sample_drivers.csv")
     )
   )
}

analyze_pins("Libya", "lby")
