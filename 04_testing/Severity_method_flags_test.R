library(tidyverse)

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

df <- read_csv(file.path(
  file_paths$agg_dir,
  "2022_sectoral_sev.csv"
)) %>%
  filter(sector_general != "intersectoral") %>%
  mutate(disag = paste0(
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
    administration
  )) %>%
  group_by(
    adm0_name,
    disag
  ) %>%
  mutate(n_sectors = n()) %>%
  group_by(adm0_name) %>%
  mutate(
    country_sectors = max(n_sectors),
    adm0_name = paste0(adm0_name, " (", country_sectors, ")"),
  ) %>%
  ungroup() %>%
  filter(n_sectors > 3)

df_calc <- df %>%
  group_by(
    adm0_name,
    disag
  ) %>%
  summarize(
    s_5 = sum(severity == 5, na.rm = TRUE),
    s_4 = sum(severity == 4, na.rm = TRUE),
    s_3 = sum(severity == 3, na.rm = TRUE),
    s_2 = sum(severity == 2, na.rm = TRUE),
    s_1 = sum(severity == 1, na.rm = TRUE),
    n_sectors = n(),
    mean_sectors = mean(severity),
    max_sectors = max(severity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    preliminary = case_when(
      s_5 >= 2 & s_5 + s_4 >= 4 ~ 5,
      s_4 + s_5 >= 4 ~ 4,
      s_3 + s_4 + s_5 >= 4 ~ 3,
      s_3 + s_4 + s_5 + s_2 >= 4 ~ 2,
      s_2 <= 3 ~ 1
    ),
    flag1 = case_when(s_5 > 0 ~ "Flag 1: 1 or more sectors in phase 5"),
    flag2 = case_when(
      s_4 >= 5 | s_4 / n_sectors >= 0.75 ~
        "Flag 2: 5 or more, or at least 75% of sectors in phase 4"
    ),
    flag3 = case_when(
      preliminary == 3 &
        (s_3 + s_4 + s_5 >= 5 | (s_3 + s_4 + s_5) / n_sectors >= 0.75) ~ paste(
        "Flag 3: 5 or more, or at least 75% of sectors in phase 3 or worse",
        "and preliminary in phase 3"
      )
    ),
    flag4a = case_when(
      mean_sectors <= preliminary - 1 ~
        "Flag 4: Preliminary is at least 1 phases away from average."
    ),
    flag4b = case_when(
      mean_sectors <= preliminary - 1.5 ~
        "Flag 4: Preliminary is at least 1.5 phases away from average."
    ),
    flag4c = case_when(
      mean_sectors <= preliminary - 2 ~
        "Flag 4: Preliminary is at least 2 phases away from average."
    )
  ) %>%
  pivot_longer(
    cols = matches("flag"),
    values_to = "flags"
  ) %>%
  select(
    adm0_name,
    disag,
    preliminary,
    flags
  ) %>%
  unique()

df_calc %>%
  group_by(
    adm0_name,
    preliminary,
    flags
  ) %>%
  summarize(n_units_flagged = n()) %>%
  filter(!is.na(flags)) %>%
  ggplot(aes(
    x = preliminary,
    y = n_units_flagged,
    label = n_units_flagged,
    fill = flags,
    color = flags
  )) +
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.9
  ) +
  geom_text(
    aes(color = flags),
    size = 3,
    hjust = .5,
    vjust = -0.5,
    position = position_dodge(0.9),
    inherit.aes = TRUE
  ) +
  scale_fill_manual(values = c(
    "#66B0EC",
    "#F2645A",
    "#85ab67",
    "#FFBF00",
    "#ba6594",
    "#626c9e"
  )) +
  scale_color_manual(values = c(
    "#66B0EC",
    "#F2645A",
    "#85ab67",
    "#FFBF00",
    "#ba6594",
    "#626c9e"
  )) +
  facet_wrap(~adm0_name,
    scales = "free_y"
  ) +
  labs(
    x = "Severity",
    title = paste(
      "Distribution of Automated Flags per Preliminary Severity Classification",
      "per country."
    ),
    y = "# of unit of analysis",
    fill = ""
  ) +
  scale_y_continuous(
    labels = scales::number,
    expand = c(0.20, 0)
  ) +
  guides(fill = guide_legend(nrow = 3), color = "none") +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(
      size = 10,
      family = "Roboto",
      margin = margin(r = 10, unit = "pt")
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 14,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 14,
      family = "Roboto",
      margin = margin(t = 20)
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
    file_paths$output_dir_sev,
    "graphs",
    "2022_severity_method_flags_test.png"
  ),
  height = 8,
  width = 14
)
