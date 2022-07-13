library(tidyverse)
library(tidytext)

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

df <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
) %>%
  filter(!is.na(affected_population) & affected_population > 0) %>%
  mutate(adm_pcode = case_when(
    !is.na(adm3_pcode) ~ adm3_pcode,
    !is.na(adm2_pcode) ~ adm2_pcode,
    TRUE ~ adm1_pcode
  )) %>%
  unite(pop_group,
    any_of(c(
      "population_group",
      "administration",
      "sex",
      "age"
    )),
    sep = ", ",
    na.rm = TRUE,
    remove = TRUE
  )

########################
#### DATA WRANGLING ####
########################

df_max <- df %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  slice_max(pin, n = 2, with_ties = FALSE) %>%
  summarize(
    max_pin = pin[1],
    max_sector = sector[1],
    second_max_pin = pin[2],
    second_max_sector = sector[2],
    affected_population = affected_population[1]
  )

df_min <- df %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  slice_min(pin, n = 2, with_ties = FALSE) %>%
  summarize(
    min_pin = pin[1],
    min_sector = sector[1],
    second_min_pin = pin[2],
    second_min_sector = sector[2]
  )

df_max_min <-
  left_join(
    df_max,
    df_min
  ) %>%
  filter(!is.na(second_max_pin) & max_pin > 0)

df_small_clusters <- df %>%
  select(
    adm0_pcode,
    adm_pcode,
    pop_group,
    sector,
    pin,
    affected_population
  ) %>%
  filter(sector %in% c(
    "Nutrition",
    "Education",
    "Protection (CP)",
    "Protection (GBV)"
  )) %>%
  mutate(perc_pin = pin / affected_population)

df_outliers <- df %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  mutate(
    mean = mean(pin),
    stdv = sd(pin, na.rm = TRUE)
  ) %>%
  filter(!is.na(stdv)) %>%
  mutate(
    is_upper_outlier = ifelse(pin > (2 * stdv) + mean, 1, 0),
    is_lower_outlier = ifelse(pin < mean - (2 * stdv), 1, 0),
    is_any_outlier = pmin(is_upper_outlier + is_lower_outlier, 1)
  )

write_csv(
  df_outliers,
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_sectors_being_outlier.csv"
  )
)

df_perc_upp_low <- df_outliers %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarize(
    `upper outlier` = sum(is_upper_outlier, na.rm = TRUE) / n(),
    `lower outlier` = sum(is_lower_outlier, na.rm = TRUE) / n()
  ) %>%
  pivot_longer(
    cols = matches("(upper|lower) outlier"),
    names_to = "type",
    values_to = "value"
  )

df_perc_upp_low %>%
  ggplot(
    aes(
      x = reorder_within(
        sector,
        value,
        adm0_pcode
      ),
      y = value,
      fill = type
    )
  ) +
  geom_col() +
  facet_wrap(
    ~adm0_pcode,
    scales = "free_y"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "% of sectors identified as outliers",
    subtitle = "Sectoral PiN < or > 2 std. dev. from mean sectoral PiN",
    y = "% of lowest unit of analysis with outliers",
    x = "",
    caption = paste0(
      "Lowest unit of analysis is the most disaggregated PiN available, ",
      "such as PiN per population group at the admin 2 level."
    ),
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#007CE0", "#1EBFB3")
  ) +
  theme_minimal() +
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
  scale_x_reordered(
    labels = function(x) str_remove(x, "__(.*)")
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_percentage_of_outlier_frequency_per_sector.png"
  ),
  height = 10,
  width = 11
)

write_csv(
  df_perc_upp_low,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_percentage_of_outlier_frequency_per_sector.csv"
  )
)

# overall percentage of outlier per country
df_overall_outlier <- df_outliers %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  summarize(
    is_upper_outlier = sum(is_upper_outlier, na.rm = TRUE),
    is_lower_outlier = sum(is_lower_outlier, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(adm0_pcode) %>%
  summarize(
    `upper outlier` = sum(is_upper_outlier, na.rm = TRUE) / n(),
    `lower outlier` = sum(is_lower_outlier, na.rm = TRUE) / n(),
    .groups =
    ) %>%
  pivot_longer(
    cols = matches("(upper|lower) outlier"),
    names_to = "type",
    values_to = "value"
  )

df_overall_outlier %>%
  ggplot(aes(x = reorder(adm0_pcode, +value), y = value, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "% of lowest unit of analysis having at least one outlier",
    subtitle = "Sectoral PiN < or > 2 std. dev. from mean sectoral PiN",
    y = "% of lowest unit of analysis with outliers",
    x = "",
    caption = paste0(
      "Lowest unit of analysis is the most disaggregated PiN available, ",
      "such as PiN per population group at the admin 2 level."
    ),
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#007CE0", "#1EBFB3")
  ) +
  theme_minimal() +
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
    "2022_percentage_of_times_sectors_being_outlier.png"
  ),
  height = 8,
  width = 12
)

write_csv(
  df_overall_outlier,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_percentage_of_times_sectors_being_outlier.csv"
  )
)

# percentage density of max pin over affected population
df_max_density <- df_max_min %>%
  pivot_longer(
    cols = matches("pin$"),
    names_to = "summary_mode",
    values_to = "value"
  ) %>%
  filter(!is.na(value), affected_population > 0, summary_mode == "max_pin") %>%
  mutate(
    perc_of_pop = value / affected_population
  )

df_max_density %>%
  ggplot() +
  geom_density(
    aes(
      x = perc_of_pop,
      y = ..scaled..,
      fill = summary_mode
    ),
    fill = "#1EBFB3",
    na.rm = TRUE
  ) +
  geom_text(
    data = data.frame(
      x = c(0.05, 0.1, 0.05),
      y = c(0.7, 0.7, 0.85),
      adm0_pcode = c("MLI", "MOZ", "SDN"),
      label = c(
        "Many units have near 100%\nof population as PiN",
        "Many units have near 0%\nof population as PiN",
        "Mix of units with PiN near\n25% or 100% of population"
      )
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    family = "Roboto",
    hjust = 0
  ) +
  facet_wrap(~adm0_pcode, scales = "fixed") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    y = "Density of Max PiN",
    title = "Distribution of max PiN as % of the affected population",
    x = "Max PiN % of the affected population"
  ) +
  scale_fill_manual(
    values = "#FFE0B2"
  ) +
  theme_minimal() +
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
    "2022_max_pin_density.png"
  ),
  height = 10,
  width = 14
)

write_csv(
  df_max_density,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_max_pin_density.csv"
  )
)

# difference between second max pin and min pin to max pin as
# percentage of the total affected population
df_diff_max_min <- df_max_min %>%
  mutate(
    diff_second_max = max_pin - second_max_pin,
    diff_min_max = max_pin - min_pin
  ) %>%
  pivot_longer(
    cols = matches("^diff|^max_pin"),
    names_to = "summary_mode",
    values_to = "value"
  ) %>%
  mutate(
    summary_mode = case_when(
      summary_mode == "max_pin" ~ "Max of Sectoral PiN",
      summary_mode == "diff_second_max" ~
        "% difference between max and second max PiN",
      TRUE ~
        "% difference between max and min PiN"
    )
  ) %>%
  group_by(
    adm0_pcode,
    summary_mode
  ) %>%
  summarize(
    value = sum(value) / sum(affected_population)
  )

df_diff_max_min %>%
  ggplot(aes(y = value, x = summary_mode, fill = "")) +
  geom_col() +
  coord_flip() +
  facet_wrap(~adm0_pcode, scales = "fixed") +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, width = 25)
    }
  ) +
  labs(
    x = "",
    y = "% of the total affected population",
    title = paste0(
      "% difference between max PiN and second max PiN ",
      "as well as min PiN per country"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_manual(
    values = c("#f9b993")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      colour = "#134373",
      margin = margin(10, 10, 30, 10, "pt"),
      hjust = 0.5
    ),
    legend.position = "none",
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      colour = "#134373",
      margin = margin(20, 10, 10, 10, "pt")
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      colour = "#134373",
      margin = margin(10, 20, 10, 10, "pt")
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_perc_diff_max_min_pin.png"
  ),
  height = 13,
  width = 20
)

write_csv(
  df_diff_max_min,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_perc_diff_max_min_pin.csv"
  )
)

## density of percentage of PiN for the samll sectors
df_small_clusters %>%
  ggplot(
    aes(
      x = perc_pin,
      y = ..scaled..,
      fill = sector
    )
  ) +
  geom_density(
    alpha = 0.5
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  labs(
    y = "Density",
    title = paste0(
      "PiN distribution of Nutrition, Education, Child Protection and GBV ",
      "as % of the affected population"
    ),
    x = "PiN (% of the affected population)",
    fill = ""
  ) +
  scale_x_continuous(
    labels = scales::percent_format(),
    breaks = scales::pretty_breaks()
  ) +
  scale_fill_manual(
    values = c(
      "#007CE0",
      "#1EBFB3",
      "#F2645A",
      "#888888"
    )
  ) +
  theme_minimal() +
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
    "2022_percentage_pin_density_subpop_sectors.png"
  ),
  height = 13,
  width = 20
)

write_csv(
  df_small_clusters,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_percentage_pin_density_subpop_sectors.csv"
  )
)

# frequency of sectors being max pin
df_max_sector_freq <- df_max_min %>%
  pivot_longer(
    cols = matches("max_sector"),
    values_to = "sectors",
    names_to = "modes"
  ) %>%
  mutate(
    modes = ifelse(modes == "max_sector",
      "Max sector",
      "Second max sector"
    )
  )

df_max_sector_freq %>%
  ggplot(
    aes(
      x = sectors,
      fill = modes
    )
  ) +
  geom_histogram(
    stat = "count",
    position = "dodge"
  ) +
  facet_wrap(
    ~adm0_pcode,
    scales = "free"
  ) +
  labs(
    y = "Frequency of lowest unit of analysis",
    title = paste0(
      "Frequency of sectors being max or second max value at the ",
      "lowest unit of analysis per country"
    ),
    x = "Sectors",
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#1EBFB3", "#007CE0")
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
  coord_flip()

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_frequency_of_sectors_being_max.png"
  ),
  height = 13,
  width = 20
)

write_csv(
  df_max_sector_freq,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_frequency_of_sectors_being_max.csv"
  )
)
################################
#### SECTORS FREQUENTLY MAX ####
################################

df_max_frequency <- df %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  mutate(
    max_pin = pin == max(pin) & pin != 0
  ) %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarize(
    max_times = sum(max_pin),
    .groups = "drop"
  )

df_max_frequency %>%
  ggplot(
    aes(
      y = reorder_within(
        sector,
        max_times,
        adm0_pcode
      ),
      x = max_times
    )
  ) +
  geom_col(
    fill = "#1EBFB3"
  ) +
  facet_wrap(
    ~adm0_pcode,
    scales = "free"
  ) +
  scale_y_reordered(
    labels = function(x) str_remove(x, "__(.*)")
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks()
  ) +
  labs(
    title = "Frequency of sectors being the max PiN",
    y = "",
    x = "# of times sectoral PiN was the max PiN"
  ) +
  theme_minimal() +
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
    "2022_freq_sector_max.png"
  ),
  height = 12,
  width = 15
)

write_csv(
  df_max_frequency,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_freq_sector_max.csv"
  )
)

#####################################
#### DISTRIBUTION OF MAX AND MIN ####
#####################################

df_max_min_distribution <- df_max_min %>%
  ungroup() %>%
  mutate(
    `2nd max PiN` = second_max_pin / max_pin,
    `Min PiN` = min_pin / max_pin
  ) %>%
  select(adm0_pcode, ends_with(" PiN")) %>%
  pivot_longer(-adm0_pcode)

df_max_min_distribution %>%
  ggplot() +
  geom_density(
    aes(
      x = value,
      group = name,
      fill = name,
      y = ..scaled..
    ),
    alpha = 0.6,
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_fill_manual(
    values = c("#1EBFB3", "#007CE0")
  ) +
  theme_minimal() +
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
    y = "Density",
    x = "% of max PiN",
    fill = "",
    title = "Distribution of 2nd max PiN and min PiN as % of max PiN",
    subtitle = "Density measured across lowest units of analysis",
    caption = paste0(
      "Lowest unit of analysis is the most disaggregated PiN available, ",
      "such as PiN per population group at the admin 2 level."
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_distribution_min_2nd_max_pins.png"
  ),
  height = 8,
  width = 12
)

write_csv(
  df_max_min_distribution,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_distribution_min_2nd_max_pins.csv"
  )
)

###############################################
#### RELATIONSHIP WITH DISAGG AND OUTLIERS ####
###############################################

# get labels for cutting bins
log_labs <- sapply(
  0:15,
  function(x) {
    paste(
      scales::comma(exp(x)),
      scales::comma(exp(x + 1)),
      sep = " - "
    )
  }
)

df_outlier_pop <- df_outliers %>%
  mutate(
    log_pop = log(affected_population),
    log_pop_bins = cut(
      log_pop,
      breaks = 0:16,
      labels = log_labs,
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    log_pop_bins
  ) %>%
  summarize(
    is_any_outlier = sum(is_any_outlier) / n(),
    .groups = "drop"
  )

df_outlier_pop %>%
  ggplot() +
  geom_bar(
    aes(
      x = fct_rev(log_pop_bins),
      y = is_any_outlier
    ),
    stat = "identity",
    fill = "#1EBFB3"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  labs(
    x = "Population size, disaggregated area",
    y = "% of outliers",
    title = "Outliers by size of unit of analysis"
  ) +
  theme_minimal() +
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
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_percentage_of_outlier_frequency_by_pop.png"
  ),
  height = 6,
  width = 8
)

write_csv(
  df_outlier_pop,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_percentage_of_outlier_frequency_by_pop.csv"
  )
)

## Simply plotting distribution of areas to show why using log

df_pop_hist <- df_outliers %>%
  mutate(
    log_pop = log(affected_population),
    log_pop_bins = cut(
      log_pop,
      breaks = 0:16,
      labels = log_labs,
      include.lowest = TRUE
    )
  ) %>%
  group_by(
    log_pop_bins
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    perc_areas = n / sum(n)
  )

df_pop_hist %>%
  ggplot() +
  geom_bar(
    aes(
      x = fct_rev(log_pop_bins),
      y = perc_areas
    ),
    stat = "identity",
    fill = "#1EBFB3"
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(
      accuracy = 1
    )
  ) +
  labs(
    x = "Population size, disaggregated area",
    y = "% of areas",
    title = "% of units of analysis by population size"
  ) +
  theme_minimal() +
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
    "2022_population_histogram.png"
  ),
  height = 6,
  width = 8
)

write_csv(
  df_pop_hist,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_population_histogram.csv"
  )
)

## Look at number of units of analysis and outliers

df_outliers_num <- df_outliers %>%
  group_by(
    adm0_pcode
  ) %>%
  summarize(
    units = n(),
    num_sectors = length(unique(sector)),
    pct_outliers = sum(is_any_outlier) / units,
    .groups = "drop"
  )

df_outliers_num %>%
  ggplot() +
  geom_point(
    aes(
      x = units,
      y = pct_outliers
    ),
    size = 3,
    color = "#1EBFB3"
  ) +
  scale_x_log10() +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  theme_minimal() +
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
    x = "# of units of analysis",
    y = "% of outliers",
    title = "Outliers by # of units of analysis"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_outliers_number_units_analysis.png"
  ),
  height = 4,
  width = 7
)

write_csv(
  df_outliers_num,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_outliers_number_units_analysis.csv"
  )
)

# also plotting against # of sectors

df_outliers_num %>%
  ggplot(
    aes(
      x = num_sectors,
      y = pct_outliers
    )
  ) +
  geom_point(
    size = 3,
    color = "#1EBFB3"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  theme_minimal() +
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
    x = "# of sectors included in calculation",
    y = "% of outliers",
    title = "Outliers by # of sectors"
  )


ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_outliers_number_sectors.png"
  ),
  height = 4,
  width = 7
)

write_csv(
  df_outliers_num,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_outliers_number_sectors.csv"
  )
)

df_outliers_num %>%
  ggplot(
    aes(
      x = units,
      y = num_sectors,
      color = pct_outliers
    )
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_log10() +
  scale_color_gradient(
    labels = scales::percent_format(1),
    low = "#D2F2F0",
    high = "#18998F"
  ) +
  theme_minimal() +
  labs(
    x = "# of sectors",
    y = "# of units of analysis",
    color = "% of outliers",
    title = "Outliers by # sectors and # of units of analysis"
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
    "2022_outliers_num_sectors_and_units.png"
  ),
  height = 4,
  width = 8
)

###########################################
#### % DIFF BETWEEN MAX AND SECOND MAX ####
###########################################

df_diff_max_min <- df_max_min %>%
  mutate(
    perc_diff_max = (max_pin - second_max_pin) / max_pin
  ) %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    mean_diff_max = mean(perc_diff_max),
    std_diff_max = sd(perc_diff_max)
  ) %>%
  mutate(
    is_outlier =
      ifelse(perc_diff_max > mean_diff_max + (2 * std_diff_max), 1, 0),
    perc_max = max_pin / affected_population,
    perc_max = ifelse(perc_max > 1, 1, perc_max),
    perc_2max = second_max_pin / max_pin,
    perc_2max = ifelse(perc_2max > 1, 1, perc_2max)
  )

diff_max_min_summarized <- df_diff_max_min %>%
  group_by(
    adm0_pcode
  ) %>%
  summarize(
    perc_outlier = sum(is_outlier, na.rm = TRUE) / n()
  )

diff_max_min_summarized %>%
  ggplot(
    aes(
      x = reorder(adm0_pcode, +perc_outlier),
      y = perc_outlier,
      fill = ""
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = paste(
      "% diff between highest and second highest PiN",
      "being an outlier within each country"
    ),
    y = "% of lowest unit of analysis with outliers",
    x = "",
    caption = paste0(
      "Lowest unit of analysis is the most disaggregated PiN available, ",
      "such as PiN per population group at the admin 2 level."
    ),
    fill = ""
  ) +
  scale_fill_manual(
    values = "#1EBFB3"
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
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_perc_diff_max_2nd_max.png"
  ),
  height = 10,
  width = 11
)

write_csv(
  diff_max_min_summarized,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_perc_diff_max_2nd_max.csv"
  )
)

# plotting outliers relation to % of PiN and % diff

df_diff_max_min %>%
  mutate(
    is_outlier = ifelse(is_outlier == 1, "Outliers", "Non outliers")
  ) %>%
  ggplot() +
  geom_point(aes(
    x = perc_max,
    y = perc_2max
  ),
  size = 3,
  color = "#1EBFB3",
  alpha = 0.3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  facet_wrap(~is_outlier) +
  theme_minimal() +
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
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    axis.title = element_text(
      face = "bold",
      size = 14,
      family = "Roboto"
    )
  ) +
  labs(
    x = "Max PiN (as % of affected population)",
    y = "Second max PiN (as % of max PiN)",
    title = "Outleirs and non-outliers by max and second max PiN"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_outliers_max_2nd_max.png"
  ),
  height = 10,
  width = 14
)

write_csv(
  df_diff_max_min,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_outliers_max_2nd_max.csv"
  )
)

df_diff_max_min %>%
  mutate(
    is_outlier = ifelse(is_outlier == 1, "Outliers", "Non outliers")
  ) %>%
  ggplot() +
  geom_point(aes(
    x = perc_max,
    y = perc_diff_max
  ),
  size = 3,
  color = "#1EBFB3",
  alpha = 0.3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  facet_wrap(~is_outlier) +
  theme_minimal() +
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
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    axis.title = element_text(
      face = "bold",
      size = 14,
      family = "Roboto"
    )
  ) +
  labs(
    x = "Max PiN (as % of affected population)",
    y = "% diff between max and second max PiN (as % of max PiN)",
    title = "Outleirs and non-outliers by max and its % difference with second max PiN" # nolint
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_outliers_max_diff.png"
  ),
  height = 10,
  width = 14
)

write_csv(
  df_diff_max_min,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_outliers_max_diff.csv"
  )
)

##############################
#### OUTLIERS AT ADMIN 1 ####
############################

df_outlier_adm1 <- df %>%
  mutate(
    perc_pin = pin / affected_population
  ) %>%
  group_by(
    adm0_pcode,
    adm1_pcode
  ) %>%
  mutate(
    mean_pin = mean(perc_pin),
    std_pin = sd(perc_pin),
  ) %>%
  mutate(
    is_upper_outlier =
      ifelse(perc_pin > mean_pin + (2 * std_pin), 1, 0),
    is_lower_outlier =
      ifelse(perc_pin < mean_pin - (2 * std_pin), 1, 0)
  )

df_outlier_adm1_summ <- df_outlier_adm1 %>%
  group_by(
    adm0_pcode
  ) %>%
  summarize(
    `upper outlier` = sum(is_upper_outlier, na.rm = TRUE) / n(),
    `lower outlier` = sum(is_lower_outlier, na.rm = TRUE) / n()
  ) %>%
  pivot_longer(
    cols = matches("(upper|lower) outlier"),
    names_to = "type",
    values_to = "value"
  ) %>%
  filter(value > 0)

df_outlier_adm1 %>%
  ggplot(
    aes(
      x = reorder(adm0_pcode, +value),
      y = value,
      fill = type
    )
  ) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = paste(
      "Outliers of lowest unit of disaggregation's sectoral PiN",
      "from each admin 1 for each country"
    ),
    y = "% of lowest unit of analysis with outliers",
    x = "",
    caption = paste0(
      "Lowest unit of analysis is the most disaggregated PiN available, ",
      "such as PiN per population group at the admin 2 level."
    ),
    fill = ""
  ) +
  scale_fill_manual(
    values = c("#007CE0", "#1EBFB3")
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
    "2022_outlier_admin1.png"
  ),
  height = 10,
  width = 11
)

write_csv(
  df_outlier_adm1,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_outlier_admin1.csv"
  )
)
