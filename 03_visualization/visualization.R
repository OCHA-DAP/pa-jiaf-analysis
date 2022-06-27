library(scales)
library(tidyverse)

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths_analysis()

####################
#### TOTAL PINS ####
####################

df_pins <- read.csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
) %>%
  group_by(adm0_pcode) %>%
  mutate(
    percent_diff = (pin - pin[sector_general == "intersectoral"])
    / pin[sector_general == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_general == "intersectoral" ~ "JIAF 1.1",
      sector_general == "sectoral" ~ "Option 1 (no adjustment)"
    )
  )

##################
#### PLOTTING ####
##################

ggplot(
  df_pins,
  aes(
    fill = sector_group,
    y = pin,
    x = adm0_pcode,
    label = ifelse(
      sector_group == "JIAF 1.1",
      "",
      paste0(
        round(percent_diff, digits = 0),
        "%"
      )
    )
  )
) +
  geom_bar(
    position = "dodge",
    stat = "identity"
  ) +
  geom_text(
    vjust = -0.5,
    position = position_dodge(width = 1),
    size = 3
  ) +
  labs(
    title = "JIAF 1.1 PiN vs. sectoral max PiN",
    fill = "Group",
    x = "",
    y = "PIN"
  ) +
  scale_y_continuous(
    labels = comma
  ) +
  theme_light() +
  scale_fill_manual(
    values = c("#007CE0", "#1EBFB3")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_hno_pin_totals.png"
  ),
  width = 3840,
  height = 2018,
  units = "px"
)

write_csv(
  df_pins,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_pin_totals.csv"
  )
)

# difference with intersectoral
df_pins %>%
  filter(sector_group != "JIAF 1.1") %>%
  mutate(
    number_disagg = factor(
      number_disagg,
      levels = 0:3
    )
  ) %>%
  ggplot(
    aes(
      y = fct_reorder(adm0_pcode, percent_diff),
      x = percent_diff,
      fill = number_disagg
    )
  ) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  scale_fill_manual(
    values = c("#D2F2F0", "#78D9D1", "#1EBFB3", "#18998F")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  labs(
    title = "% difference, option 1 vs. JIAF 1.1",
    y = "",
    x = "% difference",
    fill = "Additional disaggregations"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_hno_pct_difference.png"
  ),
  width = 6,
  height = 5
)

write_csv(
  df_pins %>%
    filter(sector_group != "JIAF 1.1") %>%
    mutate(number_disagg = factor(number_disagg,
      levels = 0:3
    )),
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_pct_difference.csv"
  )
)
