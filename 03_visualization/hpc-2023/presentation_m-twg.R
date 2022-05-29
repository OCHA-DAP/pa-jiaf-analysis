library("ggplot2")
library("tidyverse")
library("stringr")
library(scales)

# TODO: refactor filepaths to helpers
jiaf_dir <- Sys.getenv("JIAF_DATA_DIR")
save_path <- file.path(jiaf_dir, "Data analyzed/HPC-2023")

###################
#### WRANGLING ####
###################

# Plot the PINs
df_pins <- read.csv(
  file.path(
    save_path,
    "2022_hno_pin_totals.csv"
  )
) %>%
  filter(
    sector_group != "sectoral_cluster",
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
    adm0_name,
    adm0_pcode
  ) %>%
  mutate(
    percent_diff = (pin - pin[sector_group == "intersectoral"])
    / pin[sector_group == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_group == "intersectoral" ~ "JIAF 1.1",
      sector_group == "sectoral" ~ "HPC 2023",
    ),
    change_direction = ifelse(percent_diff > 0, "increase", "decrease")
  )

##################
#### PLOTTING ####
##################

ggplot(df_pins, aes(
  fill = fct_rev(sector_group), y = pin, x = adm0_pcode,
  label = ifelse(percent_diff == 0, "",
    paste0(round(percent_diff, digits = 0), "%")
  )
)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(vjust = -0.5, position = position_dodge(width = 1), size = 3) +
  labs(
    fill = "Methodology",
    x = "Country",
    y = "PIN"
  ) +
  scale_y_continuous(label = comma) +
  theme_light() +
  scale_fill_manual(values = c("#009988", "#EE7733"))

ggsave(file.path(save_path, "m-twg_2022_hno_pin_totals.png"),
  width = 3840, height = 2018, units = "px"
)

# difference with intersectoral
df_pins %>%
  filter(sector_group == "HPC 2023") %>%
  ggplot(
    aes(
      y = fct_reorder(adm0_name, percent_diff),
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

ggsave(file.path(save_path, "m-twg_2022_hno_pct_difference.png"),
  width = 3840, height = 2018, units = "px"
)
