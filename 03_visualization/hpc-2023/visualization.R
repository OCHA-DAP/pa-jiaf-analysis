library(ggplot2)
library(scales)
library(tidyverse)

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
  group_by(adm0_pcode) %>%
  mutate(
    percent_diff = (pin - pin[sector_group == "intersectoral"])
    / pin[sector_group == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_group == "intersectoral" ~ "JIAF 1.1",
      sector_group == "sectoral" ~ "JIAF 2.0",
      sector_group == "sectoral_cluster" ~ "Cluster totals"
    ),
  )

##################
#### PLOTTING ####
##################

ggplot(df_pins, aes(
  fill = sector_group, y = pin, x = adm0_pcode,
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
  scale_y_continuous(labels = comma) +
  theme_light()

ggsave(file.path(save_path, "2022_hno_pin_totals.png"),
       width = 3840, height = 2018, units = "px"
)

# Plot the contribs

df_contrib <- read.csv(
  file.path(
    save_path,
    "2022_hno_pin_contributions.csv"
  )
) %>%
  mutate(
    max_pct = as.numeric(sub("%", "", max_pct)),
    pin_pct = as.numeric(sub("%", "", pin_pct))
  ) %>%
  arrange(adm0_pcode, desc(pin_pct))

ggplot(
  df_contrib %>% filter(sector_group == "sectoral", pin_pct > 0),
  aes(x = fct_rev(fct_inorder(max_sector)), y = pin_pct, label = max_sector)
) +
  geom_bar(
    position = "dodge", stat = "identity", show.legend = FALSE,
    fill = "#FF6666"
  ) +
  coord_flip() +
  facet_grid(adm0_pcode ~ ., scales = "free", space = "free") +
  labs(
    x = "sector",
    y = "% contribution to intersectoral PIN"
  ) +
  theme(strip.text = element_text(
    size = 5),
    axis.text = element_text(
      size = 5))

ggsave(file.path(save_path, "2022_hno_pin_contributions.png"),
       width = 3840, height = 2018, units = "px", scale = 0.7
)

# difference with intersectoral
df_pins %>%
  filter(sector_group == "JIAF 2.0") %>%
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
    title = "% difference, 2023 HPC and 2022 actual",
    subtitle = "Intersectoral PiN calculations",
    fill = "Number of\ndisaggregations"
  ) +
  geom_text(
    y = "COL 2+",
    x = 30,
    label = paste0(
      "Colombia PiN calculated for all sectors\n",
      "with severity 2 and above."
    ),
    size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    y = "COL 3+",
    x = 30,
    label = paste0(
      "Colombia PiN calculated for all sectors\n",
      "with severity 3 and above."
    ),
    size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    y = "IRQ",
    x = 30,
    label = "Reported PiN for each admin\narea is higher than sector max.",
    size = 3,
    check_overlap = TRUE
  ) +
  geom_text(
    y = "NGA",
    x = 30,
    label = "Reported PiN for each admin\narea is higher than sector max.",
    size = 3,
    check_overlap = TRUE
  )

ggsave(file.path(save_path, "2022_hno_pct_difference.png"),
  width = 3840, height = 2018, units = "px"
)
