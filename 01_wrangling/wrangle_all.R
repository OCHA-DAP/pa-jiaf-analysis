library(tidyverse)
library(renv)
source(here::here("99_helpers", "helpers.R"))

# get all filepaths
file_paths <- get_paths_analysis()

# Run all file wrangling for countries
wrangle_files <- list.files("01_wrangling")
wrangle_files <- wrangle_files[wrangle_files != "wrangle_all.R"]

walk(
  wrangle_files,
  \(x) {
    print(x)
    run(
      file.path(
        "01_wrangling",
        x
      )
    )
  }
)

# Sectoral PiNs

sectoral_df <- map_dfr(
  list.files(
    file_paths$input_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
) %>%
  mutate(
    sector_group = ifelse(
      source == "cluster",
      "sectoral_cluster",
      sector_general
    ),
    .before = pin
  )

sectoral_df %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_sectoral_pins.csv"
    )
  )

# Indicator PiNs

df_indicators <- map_dfr(
  list.files(
    file_paths$input_indicator_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  ~ read_csv(.) %>% mutate(indicator_number = as.character(indicator_number))
)

df_indicators %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_indicator_pins.csv"
    )
  )

# HH Data

df_hh_data <- map_dfr(
  list.files(
    file_paths$input_hh_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
)

df_hh_data %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_hh_data.csv"
    )
  )
