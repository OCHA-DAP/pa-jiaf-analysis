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
  filter( # filtering out those that are not of interest
    !is.na(severity) &
      sector_general != "intersectoral" &
      severity > 0
  ) %>%
  mutate(
    severity = as.numeric(severity)
  ) %>%
  group_by(
    adm0_pcode,
    disaggregation
  ) %>%
  summarize(
    mean = mean(severity),
    min = min(severity),
    max = max(severity),
    n_sectors = n(),
    depth = mean - ((max - min) / n_sectors),
    overlap = (sum(severity >= 3) / n_sectors) * 5,
    .groups = "drop"
  ) %>%
  expand(
    nesting(
      adm0_pcode,
      disaggregation,
      mean,
      min,
      max,
      n_sectors,
      depth,
      overlap
    ),
    life_threat = c("Yes", "No"),
    irreversible = c("Yes", "No"),
    vulnerable = c("Yes", "No")
  ) %>%
  mutate(
    average = (depth + overlap) / 2,
    qual_score = 5 * (
      ifelse(life_threat == "Yes", 4, 0) +
        ifelse(irreversible == "Yes", 2, 0) +
        ifelse(vulnerable == "Yes", 1, 0)
    ) / 7,
    prelim_sev = ifelse(qual_score > average,
      (qual_score + average) / 2,
      average
    )
  )

df %>%
  mutate(
    avg_cut = cut(
      average,
      breaks = seq(0, 5, 0.5)
    ),
    qual = paste(life_threat, irreversible, vulnerable, sep = ", ")
  ) %>%
  group_by(
    adm0_pcode,
    avg_cut,
    qual
  ) %>%
  summarize(
    prelim_sev = mean(prelim_sev, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(
      x = avg_cut,
      y = qual,
      fill = prelim_sev
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  geom_text(
    aes(
      label = round(prelim_sev, 1)
    ),
    family = "Roboto"
  ) +
  facet_wrap(~adm0_pcode) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
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
      family = "Roboto",
      angle = -90
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto",
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_discrete(
    labels = paste(
      seq(0, 4.5, 0.5),
      seq(0.5, 5, 0.5),
      sep = " - "
    )
  ) +
  labs(
    y = "Cases of the components",
    x = "Quantitative Score",
    fill = "Average final Score",
    title = paste(
      "Final severity scores from quantitative sectoral severities adjusted by",
      "all possible cases of the 3 qualitative components."
    ),
    subtitle = paste(
      "Qualitative components: Life threatening conditions,",
      "Irreversible negative consequences,",
      "Vulnerability"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_severity_scores_from_components.png"
  ),
  width = 18,
  height = 12,
  units = "in"
)

# difference in scores
df %>%
  mutate(
    avg_cut = cut(
      average,
      breaks = seq(0, 5, 0.5)
    ),
    qual = paste(life_threat, irreversible, vulnerable, sep = ", ")
  ) %>%
  group_by(
    adm0_pcode,
    avg_cut,
    qual
  ) %>%
  summarize(
    prelim_sev = mean(prelim_sev - average, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(
      x = avg_cut,
      y = qual,
      fill = prelim_sev
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  geom_text(
    aes(
      label = round(prelim_sev, 1)
    ),
    family = "Roboto"
  ) +
  facet_wrap(~adm0_pcode) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.subtitle = element_text(
      size = 12,
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    text = element_text(
      size = 8,
      family = "Roboto",
    ),
    axis.title = element_text(
      size = 12,
      family = "Roboto",
    ),
    axis.text.x = element_text(
      face = "bold",
      margin = margin(20, 0, 20, 0, "pt"),
      size = 10,
      family = "Roboto",
      angle = -90
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto",
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_discrete(
    labels = paste(
      seq(0, 4.5, 0.5),
      seq(0.5, 5, 0.5),
      sep = " - "
    )
  ) +
  labs(
    y = "Cases of the components",
    x = "Quantitative Score",
    fill = "Average increase\nof scores",
    title = paste(
      "Average score increase by the 3 qualitative components to the",
      "quantitative sectoral severities."
    ),
    subtitle = paste(
      "Qualitative components: Life threatening conditions,",
      "Irreversible negative consequences,",
      "Vulnerability"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_severity_increases_from_components.png"
  ),
  width = 15,
  height = 10,
  units = "in"
)


write_csv(
  df,
  file.path(
    file_paths$output_dir_sev,
    "datasets",
    "2022_severity_components_aggregated.csv"
  )
)

abc <- data.frame(s1 = 1:5) %>%
  expand(
    s1,
    s2 = 1:5,
    s3 = 1:5,
    s4 = 1:5,
    s5 = 1:5,
    s6 = 1:5,
    s7 = 1:5,
    s8 = 1:5
  ) %>%
  rowwise() %>%
  transmute(
    mean = mean(c(s1, s2, s3, s4, s5, s6, s7, s8)),
    min = min(c(s1, s2, s3, s4, s5, s6, s7, s8)),
    max = max(c(s1, s2, s3, s4, s5, s6, s7, s8)),
    depth = mean - ((max - min) / 8),
    overlap = (sum(c(s1, s2, s3, s4, s5, s6, s7, s8) >= 3) / 8) * 5,
    average = (depth + overlap) / 2
  ) %>%
  unique() %>%
  expand(
    nesting(
      mean,
      min,
      max,
      depth,
      overlap,
      average
    ),
    life_threat = c("Yes", "No"),
    irreversible = c("Yes", "No"),
    vulnerable = c("Yes", "No")
  ) %>%
  mutate(
    qual_score = 5 * (
      ifelse(life_threat == "Yes", 4, 0) +
        ifelse(irreversible == "Yes", 2, 0) +
        ifelse(vulnerable == "Yes", 1, 0)
    ) / 7,
    prelim_sev = ifelse(qual_score > average,
      (qual_score + average) / 2,
      average
    )
  )

abc %>%
  mutate(
    avg_cut = cut(
      average,
      breaks = seq(0, 5, 0.5)
    ),
    qual = paste(life_threat, irreversible, vulnerable, sep = ", ")
  ) %>%
  group_by(
    avg_cut,
    qual
  ) %>%
  summarize(
    prelim_sev = mean(prelim_sev - average, na.rm = TRUE)
  ) %>%
  ggplot(
    aes(
      x = avg_cut,
      y = qual,
      fill = prelim_sev
    )
  ) +
  geom_tile(
    color = "white"
  ) +
  geom_text(
    aes(
      label = paste0(
        ifelse(round(prelim_sev, 2) > 0, "+", ""),
        round(prelim_sev, 2)
      )
    ),
    family = "Roboto"
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.subtitle = element_text(
      size = 14,
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
      family = "Roboto",
      angle = -90
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto",
      hjust = 0
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_discrete(
    labels = paste(
      seq(0, 4.5, 0.5),
      seq(0.5, 5, 0.5),
      sep = " - "
    )
  ) +
  labs(
    y = "Components",
    x = "Quantitative Score",
    fill = "Average increase\nof severity",
    title = paste(
      "All possible increases of quantitative severity by the 3 qualitative",
      "components,\nconsidering 8 sectoral severities."
    ),
    subtitle = paste(
      "Qualitative components: Life threatening conditions,",
      "Irreversible negative consequences,",
      "Vulnerability"
    )
  )


ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_severity_increases_all_cases.png"
  ),
  width = 15,
  height = 10,
  units = "in"
)

write_csv(
  abc,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_severity_increases_all_cases.csv"
  )
)
