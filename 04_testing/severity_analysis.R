library(tidyverse)
library(tidytext)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

###################
#### Reading Data ####
###################

df <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_sectoral_sev.csv"
  )
) %>%
  unite(
    disaggregation,
    any_of(
      c(
        "adm1_pcode",
        "adm2_pcode",
        "adm3_pcode",
        "population_group",
        "administration"
      )
    ),
    sep = ", ",
    na.rm = TRUE
  ) %>%
  mutate(
    sector = ifelse(sector == "intersectoral", "Intersectoral", sector)
  )

# frequency of sectors being 3 or above
temp <- df %>%
  filter( # filtering out those that are not of interest
    !is.na(severity) &
      sector_general != "intersectoral" &
      severity > 0
  ) %>%
  mutate(
    above_2 = ifelse(severity > 2, 1, 0)
  ) %>%
  group_by(
    adm0_pcode,
    disaggregation
  ) %>%
  summarize(
    sum_above_2 = sum(above_2, na.rm = TRUE),
    .groups = "drop"
  )

temp %>%
  ggplot() +
  geom_density(
    aes(x = sum_above_2),
    fill = "#1EBFB3",
    na.rm = TRUE
  ) +
  facet_wrap(~adm0_pcode, scales = "free_y") +
  labs(
    y = "Density of number of sectors",
    title = "Distribution of number of sectors that are 3 or above by geographic area", # nolint
    x = "Number of sectors"
  ) +
  scale_fill_manual(
    values = "#FFE0B2"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks()
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = .5,
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text.x = element_text(
      face = "bold",
      margin = margin(20, 0, 20, 0, "pt"),
      size = 10,
      family = "Roboto"
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto"
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_number_sectors_3_density.png"
  ),
  height = 10,
  width = 14
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_number_sectors_3_density.csv"
  )
)

temp <- df %>%
  filter(
    !is.na(severity) & severity > 0
  ) %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarize(
    sum_severity_3 = sum(severity > 3) / n(),
    .groups = "drop"
  )

temp %>%
  ggplot() +
  geom_tile(
    aes(
      y = sector,
      x = adm0_pcode,
      fill = sum_severity_3
    )
  ) +
  theme_minimal() +
  scale_fill_gradient(
    labels = scales::percent_format(1),
    low = "#ebfffd",
    high = "#1EBFB3"
  ) +
  labs(
    fill = "% of areas with score 3 or above",
    y = "Sector",
    x = "",
    title = "Sectoral severity as % of geographical areas with score 3 or above"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    axis.text = element_text(
      face = "bold",
      size = 12,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 14,
      family = "Roboto",
      margin = margin(r = 10)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.title = element_text(
      size = 12,
      family = "Roboto",
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_sector_severity_percent.png"
  ),
  width = 15,
  height = 10
)

write_csv(
  fig_heatmap,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_sector_severity_percent.csv"
  )
)
