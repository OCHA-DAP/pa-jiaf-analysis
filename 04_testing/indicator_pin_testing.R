library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### WRANGLING ####
###################

df_indicators <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_indicator_pins.csv"
  )
)

df_sectors <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
) %>%
  filter(adm0_pcode %in% df_indicators$adm0_pcode) %>%
  rename(pin_calculation = sector_group)

max_df <- df_indicators %>%
  pivot_longer(
    matches("adm[1-3]+"),
    names_to = c(NA, "adm_level", NA),
    values_to = "adm_pcode",
    names_pattern = "(adm)([0-9]+)(_pcode)",
    names_transform = list(adm_level = as.numeric)
  ) %>%
  drop_na(adm_pcode) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    indicator_number
  ) %>%
  slice_max(adm_level) %>%
  unite(pop_group,
    any_of(c(
      "population_group",
      "administration"
    )),
    sep = ", ",
    na.rm = TRUE
  ) %>%
  group_by(
    adm0_name,
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  summarize(
    lowest_adm_level = unique(adm_level),
    max_pin = max(pin, na.rm = TRUE),
    max_indicator = paste(indicator_number[pin == max_pin], collapse = ", "),
    .groups = "drop"
  )

df_indicator_pin <- max_df %>%
  group_by(
    adm0_name,
    adm0_pcode
  ) %>%
  summarize(
    lowest_adm_level = unique(lowest_adm_level),
    number_disagg = grepl(
      "[^\\s]",
      pop_group[1]
    ) + str_count(pop_group[1], ","),
    pin = sum(max_pin),
    .groups = "drop"
  ) %>%
  mutate(pin_calculation = "Max of Indicator PiN")

df_compr_sector_indicator <- df_indicator_pin %>%
  bind_rows(df_sectors) %>%
  filter(pin_calculation != "intersectoral")

df_compr_sector_indicator %>%
  mutate(
    pin_calculation = case_when(
      # pin_calculation == "intersectoral" ~ "HNO2022 PiN",
      pin_calculation == "sectoral" ~ "Max Sectoral PiN",
      TRUE ~ pin_calculation
    ),
    pin = round(pin / 1000000, 2)
  ) %>%
  ggplot(aes(
    x = pin_calculation,
    y = pin,
    fill = pin_calculation,
    label = paste0(pin, "M")
  )) +
  facet_wrap(~adm0_name,
    nrow = 1,
    strip.position = "bottom"
  ) +
  geom_text(
    position = position_identity(),
    vjust = -.2,
    size = 4
  ) +
  geom_col() +
  labs(
    fill = "",
    x = "",
    y = "PIN",
    title = paste(
      "Option 3: Comparison between Max of",
      "Indicator PiN and Max of Sectoral"
    )
  ) +
  theme_minimal() +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, width = 35)
    }
  ) +
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
    panel.grid.major = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 3",
    "2022_indicator_pin_sectoral_pin.png"
  ),
  height = 6,
  width = 12
)

write_csv(
  df_compr_sector_indicator,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 3",
    "datasets",
    "2022_indicator_pin_sectoral_pin.csv"
  )
)

df_compr_jiaf_indicator <- df_sectors %>%
  filter(pin_calculation == "intersectoral") %>%
  left_join(df_indicator_pin %>%
    transmute(
      adm0_name,
      ind_pin = pin
    ))

df_compr_jiaf_indicator %>%
  mutate(pin_diff = round((ind_pin - pin) / 1000000, 2)) %>%
  ggplot(aes(
    y = reorder(adm0_pcode, pin_diff),
    x = pin_diff,
    label = paste0(pin_diff, "M")
  ), ) +
  scale_fill_distiller(type = "seq", direction = 1) +
  geom_col(
    fill = "#1EBFB3"
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "M")) +
  theme_minimal() +
  labs(
    y = "",
    x = "Difference in PiN, max of indicator PiN - JIAF 1.1 PiN",
    title = paste(
      "Difference in country PiN using max",
      "of indicator PiN compared to JIAF 1.1 PiN"
    ),
    caption = "Positive value means indicator PiN is higher than JIAF PiN"
  ) +
  geom_text(
    position = position_identity(),
    hjust = -.5,
    size = 4
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
    "Option 3",
    "2022_absolute_compr_indicator_jiaf.png"
  ),
  height = 6,
  width = 16
)

write_csv(
  df_compr_jiaf_indicator,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 3",
    "datasets",
    "2022_absolute_compr_indicator_jiaf.csv"
  )
)

# absolute comparison between indicator PiN and sectoral PiN
df_diff_indicator_sector <- df_sectors %>%
  filter(pin_calculation != "intersectoral") %>%
  left_join(df_indicator_pin %>%
    transmute(adm0_name,
      ind_pin = pin
    ))

df_diff_indicator_sector %>%
  mutate(pin_diff = ind_pin - pin) %>%
  ggplot(aes(
    y = reorder(adm0_pcode, pin_diff),
    x = pin_diff
  ), ) +
  scale_fill_continuous() +
  geom_col(fill = "#1EBFB3") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  labs(
    y = "",
    x = "Difference in PiN, max of indicator PiN - max of sectoral PiN",
    title = paste(
      "Difference in country PiN using max of",
      "indicator PiN compared to max of sectoral PiN"
    ),
    caption = "Positive value means indicator PiN is higher than sectoral PiN"
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
    "Option 3",
    "2022_absolute_compr_indicator_sectoral.png"
  ),
  height = 6,
  width = 14
)

write_csv(
  df_diff_indicator_sector,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 3",
    "datasets",
    "2022_absolute_compr_indicator_sectoral.csv"
  )
)

# percent comparison between indicator PiN and sectoral PiN
df_perc_indicator_sector <- df_sectors %>%
  filter(pin_calculation != "intersectoral") %>%
  left_join(df_indicator_pin %>%
    transmute(adm0_name,
      ind_pin = pin
    ))

df_perc_indicator_sector %>%
  mutate(
    pin_diff = round((ind_pin - pin) / pin, 2) * 100,
    pin_diff_label = pmax(pin_diff, -3)
  ) %>%
  ggplot(aes(
    y = reorder(adm0_pcode, pin_diff),
    x = pin_diff,
    label = paste0(pin_diff, "%")
  ), ) +
  scale_fill_distiller(type = "seq", direction = 1) +
  geom_col(fill = "#1EBFB3") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  ) +
  labs(
    y = "",
    x = paste(
      "Percent of increase or decrease of",
      "indicator PiN compared to sectoral PiN"
    ),
    title = paste(
      "Increase or decrease in country PiN resulted",
      "from max of indicator PiN compared to sectoral PiN"
    )
  ) +
  geom_text(
    aes(
      x = pin_diff_label
    ),
    position = position_identity(),
    hjust = -.3,
    size = 4
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
    "Option 3",
    "2022_percent_compr_indicator_sectoral.png"
  ),
  height = 6,
  width = 12
)

write_csv(
  df_perc_indicator_sector,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 3",
    "datasets",
    "2022_percent_compr_indicator_sectoral.csv"
  )
)
