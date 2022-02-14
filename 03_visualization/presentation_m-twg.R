library("ggplot2")
library("tidyverse")

# TODO: refactor filepaths to helpers
jiaf_dir <- Sys.getenv("JIAF_DATA_DIR")
save_path <- file.path(jiaf_dir, "Data analyzed")

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
    adm0_pcode != "COL 2+"
  ) %>%
  mutate(adm0_pcode = gsub("COL 3\\+", "COL", adm0_pcode)) %>%
  group_by(adm0_pcode) %>%
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
    fill = "Group",
    x = "Country ISO3",
    y = "PIN"
  ) +
  scale_y_continuous(label = comma) +
  theme_light()

ggsave(file.path(save_path, "m-twg_2022_hno_pin_totals.png"),
  width = 7, height = 7
)

# difference with intersectoral
df_pins %>%
  filter(sector_group == "HPC 2023") %>%
  ggplot(
    aes(
      y = fct_reorder(adm0_pcode, percent_diff),
      x = percent_diff,
      fill = number_disagg
    )
  ) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_fill_gradient(
    low = "#F6BDC0",
    high = "#EA4C46"
  ) +
  labs(
    x = "% difference",
    y = "Country",
    title = "% difference, 2023 HPC and JIAF 1.1",
    subtitle = "Intersectoral PiN calculations",
    fill = "Number of\ndisaggregations"
  )

ggsave(file.path(save_path, "m-twg_2022_hno_pct_difference.png"),
  width = 7, height = 7
)
