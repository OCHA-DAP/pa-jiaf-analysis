library(tidyverse)
library(scales)

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths_analysis()

###################
#### WRANGLING ####
###################

# Plot the PINs
df_pins <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
) %>%
  mutate(
    scenario = case_when(
      adm0_pcode == "IRQ" ~ "A",
      adm0_pcode == "NGA" ~ "A",
      adm0_pcode == "PSE" ~ "A",
      TRUE ~ "B"
    ),
  ) %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    percent_diff = (pin - pin[sector_general == "intersectoral"])
    / pin[sector_general == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_general == "intersectoral" ~ "JIAF 1.1",
      sector_general == "sectoral" ~ "HPC 2023",
    ),
    change_direction = ifelse(percent_diff > 0, "increase", "decrease")
  )

##################
#### PLOTTING ####
##################

# difference with intersectoral
df_pins %>%
  filter(sector_group == "HPC 2023") %>%
  ggplot(
    aes(
      y = fct_reorder(adm0_pcode, percent_diff),
      x = percent_diff,
      fill = change_direction
    )
  ) +
  geom_bar(stat = "identity") +
  theme_light() +
  labs(
    x = "% difference",
    y = "Country",
    title = "% difference, 2023 HPC and JIAF 1.1",
    subtitle = "Intersectoral PiN calculations",
    fill = "Change\nw.r.t.\nJIAF 1.1"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "m-twg_2022_hno_pct_difference.png"
  ),
  width = 3840, height = 2018, units = "px"
)
