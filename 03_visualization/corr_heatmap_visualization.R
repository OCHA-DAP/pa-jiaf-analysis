library(tidyverse)
library(scales)
library(ggcorrplot)
library(GGally)

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths_analysis()

####################
#### TOTAL PINS ####
####################

df_pins <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
)

ggplot(
  df_pins %>% filter(sector_general == "sectoral"),
  aes(
    x = fct_reorder(adm0_pcode, pin),
    y = pin
  )
) +
  geom_col(
    fill = "#1EBFB3"
  ) +
  coord_flip() +
  labs(
    y = "Intersectoral PIN",
    x = "",
    title = "Total intersectoral PiN by country"
  ) +
  theme_light() +
  scale_y_continuous(labels = scales::comma) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(10, 10, 10, 10, "pt"),
      hjust = .5,
      family = "Roboto"
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
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_blank()
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "pin_totals",
    "2022_hno_pin_totals.png"
  ),
  width = 10, height = 5
)

write_csv(
  df_pins %>% filter(sector_general == "sectoral"),
  file.path(
    file_paths$output_dir,
    "graphs",
    "pin_totals",
    "datasets",
    "2022_hno_pin_totals.csv"
  )
)

######################
#### CORRELATIONS ####
######################

# Get the correlation data
df_corr <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
)

# First plot the correlations for the full data sample
df_corr_all <- df_corr %>%
  filter(
    sector != "Displaced pop."
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = pin,
    values_fn = list
  ) %>%
  unnest(
    cols = everything()
  ) %>%
  select(Education:ERL)

cluster_corr_all <- cor(
  as.matrix(df_corr_all),
  use = "pairwise.complete.obs"
) %>%
  ggcorrplot(
    type = "lower",
    lab = TRUE,
    lab_size = 2,
    colors = c("#F2645A", "white", "#1EBFB3"),
    title = "Sector correlations, disaggregated PiN"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(10, 0, 10, 0, "pt"),
      family = "Roboto",
      hjust = 1
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
      size = 8,
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
    "correlation",
    "2022_hno_pin_cluster_corr_all.png"
  ),
  width = 6,
  height = 6,
  plot = cluster_corr_all
)

write.csv(
  cor(as.matrix(df_corr_all),
    use = "pairwise.complete.obs"
  ),
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "datasets",
    "2022_hno_pin_cluster_corr_all.csv"
  ),
  row.names = TRUE
)

cluster_corr_all_pairs <- ggpairs(df_corr_all)

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "2022_hno_pin_cluster_corr_all_pairs.png"
  ),
  width = 20,
  height = 20,
  plot = cluster_corr_all_pairs
)

# You can see that the distributions are skewed towards lower numbers.
# Try with logging to weight the smaller numbers more.
# TODO: fix deprecated
df_corr_all_log <- df_corr_all %>%
  mutate(
    across(
      .fns = ~ log10(1 + .x)
    )
  )

cluster_corr_all_log <- cor(
  as.matrix(df_corr_all_log),
  use = "pairwise.complete.obs"
) %>%
  ggcorrplot(
    type = "lower",
    lab = TRUE,
    lab_size = 2
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "2022_hno_pin_cluster_corr_all_log.png"
  ),
  width = 5,
  height = 5,
  plot = cluster_corr_all_log
)

write.csv(
  cor(as.matrix(df_corr_all_log),
    use = "pairwise.complete.obs"
  ),
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "datasets",
    "2022_hno_pin_cluster_corr_all_log.csv"
  ),
  row.names = TRUE
)

cluster_corr_all_pairs_log <- ggpairs(df_corr_all_log)

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "2022_hno_pin_cluster_corr_all_pairs_log.png"
  ),
  width = 20,
  height = 20,
  plot = cluster_corr_all_pairs_log
)

# Finally try just the total country sums
df_corr_countries <- df_corr %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarise(
    pin = sum(pin),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = pin,
    values_fn = list
  ) %>%
  unnest(
    cols = everything()
  ) %>%
  select(Education:ERL)

cluster_corr_countries <- cor(
  as.matrix(df_corr_countries),
  use = "pairwise.complete.obs"
) %>%
  ggcorrplot(
    type = "lower",
    lab = TRUE,
    lab_size = 2,
    colors = c("#F2645A", "white", "#1EBFB3"),
    title = "Sector correlations, country summed PiN"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = 1
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
      size = 8,
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
    "correlation",
    "2022_hno_pin_cluster_corr_countries.png"
  ),
  width = 6,
  height = 6,
  plot = cluster_corr_countries
)

write.csv(
  cor(
    as.matrix(df_corr_countries),
    use = "pairwise.complete.obs"
  ),
  file.path(
    file_paths$output_dir,
    "graphs",
    "correlation",
    "datasets",
    "2022_hno_pin_cluster_corr_countries.csv"
  ),
  row.names = TRUE
)
# ggpairs doens't work because it uses cor.test which can't do
# pairwise, I've checked the significance with psych::corr and some
# are significant, some not. Have trouble plotting it though because
# the vectors are different size or something.

#######################
#### PERCENT TOTAL ####
#######################

df_sectoral <- df_corr %>%
  group_by(
    adm0_pcode,
    adm0_name,
    sector
  ) %>%
  summarize(
    pin = sum(pin),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = pin
  ) %>%
  left_join(
    filter(
      df_pins,
      sector_general == "intersectoral"
    ) %>%
      select(
        adm0_pcode,
        intersectoral_pin = pin
      ),
    by = c("adm0_pcode")
  ) %>%
  mutate(
    across(
      Education:ERL,
      ~ .x / intersectoral_pin
    )
  ) %>%
  select(
    -intersectoral_pin
  ) %>%
  pivot_longer(
    -starts_with("adm0"),
    names_to = "sector",
    values_to = "pin_percent"
  ) %>%
  filter(
    !is.na(pin_percent)
  )

# plot these as min to max

df_violin <- df_sectoral %>%
  group_by(sector) %>%
  summarize(
    min_perc = min(pin_percent),
    max_perc = max(pin_percent),
    n_country = n()
  ) %>%
  arrange(
    desc(max_perc),
    min_perc
  ) %>%
  mutate(
    sector = factor(sector, levels = rev(sector))
  )

fig_pin_all <- df_violin %>%
  ggplot() +
  geom_segment(
    aes(
      y = sector,
      yend = sector,
      x = min_perc,
      xend = max_perc
    ),
    lwd = 2,
    color = "#1EBFB3"
  ) +
  geom_text(
    aes(
      y = sector,
      label = n_country
    ),
    x = 1.28,
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "% of JIAF 1.1 intersectoral PiN",
    y = "",
    title = "Sectoral PiN as % of JIAF 1.1 intersectoral PiN",
    subtitle = paste0(
      "Minimum to maximum % across ",
      "n countries (specified to the right)"
    )
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
    "pin_totals",
    "2022_hno_pin_percent_all.png"
  ),
  width = 10, height = 7, plot = fig_pin_all
)

write_csv(
  df_violin,
  file.path(
    file_paths$output_dir,
    "graphs",
    "pin_totals",
    "datasets",
    "2022_hno_pin_percent_all.csv"
  )
)

# heat map version

fig_heatmap <- df_sectoral %>%
  mutate(
    sector = factor(sector, levels = levels(df_violin$sector))
  )

fig_heatmap %>%
  ggplot() +
  geom_tile(
    aes(
      x = sector,
      y = adm0_name,
      fill = pin_percent
    )
  ) +
  theme_minimal() +
  scale_fill_gradient(
    labels = scales::percent_format(1),
    low = "white",
    high = "#1EBFB3"
  ) +
  labs(
    fill = "% of JIAF 1.1 intersectoral PiN",
    y = "",
    x = "Sector",
    title = "Sectoral PiN as % of JIAF 1.1 intersectoral PiN"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.text.x = element_text(angle = -90),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
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
    file_paths$output_dir,
    "graphs",
    "pin_totals",
    "2022_hno_pin_percent_countries.png"
  ),
  width = 10,
  height = 7
)

write_csv(
  fig_heatmap,
  file.path(
    file_paths$output_dir,
    "graphs",
    "pin_totals",
    "datasets",
    "2022_hno_pin_percent_countries.csv"
  )
)
