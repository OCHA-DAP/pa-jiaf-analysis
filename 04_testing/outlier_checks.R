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
  filter(!is.na(second_max_pin))

df_small_clusters <- df %>%
  select(
    adm0_pcode,
    adm_pcode,
    pop_group,
    sector,
    pin,
    affected_population
  ) %>%
  filter(sector %in% c("nut", "edu", "pro-cp", "pro-gbv")) %>%
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
    is_outlier = ifelse(is_upper_outlier == 1 | is_lower_outlier == 1, 1, 0)
  )

df_outliers %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarize(
    `upper outlier` = sum(is_upper_outlier, na.rm = TRUE) / n(),
    `lower outlier` = sum(is_lower_outlier, na.rm = TRUE) / n(),
    `any outlier` = `upper outlier` + `lower outlier`
  ) %>%
  pivot_longer(
    cols = matches("(upper|lower) outlier"),
    names_to = "type",
    values_to = "value"
  ) %>%
  ggplot(
    aes(
      x = reorder_within(
        sector,
        `any outlier`,
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
    title = "Percentage of sectors identified as outliers",
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
    "sectoral_pins",
    "2022_percentage_of_outlier_frequency_per_sector.png"
  ),
  height = 10,
  width = 11
)

df_outliers %>%
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
  ) %>%
  ggplot(aes(x = reorder(adm0_pcode, +value), y = value, fill = type)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  labs(
    title = "Percentage of lowest unit of analysis having at least one outlier",
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
    "sectoral_pins",
    "2022_percentage_of_times_sectors_being_outlier.png"
  ),
  height = 8,
  width = 12
)

df_max_min %>%
  pivot_longer(
    cols = matches("pin$"),
    names_to = "summary_mode",
    values_to = "value"
  ) %>%
  filter(!is.na(value), affected_population > 0, summary_mode == "max_pin") %>%
  mutate(
    perc_of_pop = value / affected_population
  ) %>%
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
    "sectoral_pins",
    "2022_max_pin_density.png"
  ),
  height = 10,
  width = 14
)

# difference between second max pin and min pin to max pin as
# percentage of the total affected population
df_max_min %>%
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
  ) %>%
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
    y = "Percentage of the total affected population",
    title = paste0(
      "Percentage difference between max PiN and second max PiN ",
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
      size = 22,
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
    "2022_perc_diff_max_min_pin.png"
  ),
  height = 13,
  width = 20
)

## density of percentage of PiN for the samll sectors
df_small_clusters %>%
  mutate(
    sector = case_when(
      sector == "nut" ~ "Nutrition",
      sector == "edu" ~ "Education",
      sector == "pro-cp" ~ "Child Protection",
      sector == "pro-gbv" ~ "Protection GBV"
    )
  ) %>%
  ggplot(aes(x = perc_pin, fill = sector)) +
  geom_density() +
  facet_wrap(~adm0_pcode, scales = "free") +
  labs(
    y = "Density of percentage of sectoral PiN",
    title = paste0(
      "PiN distribution of Nutrition, Education, Child Protection and GBV ",
      "as percentage of the affected population"
    ),
    x = "Percentage of PiN of the affected population",
    fill = ""
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      colour = "#134373",
      margin = margin(10, 10, 30, 10, "pt"),
      hjust = 0.5
    ),
    legend.position = "bottom",
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
    "2022_percentage_pin_density_subpop_sectors.png"
  ),
  height = 13,
  width = 20
)

# frequency of sectors being max pin
df_max_min %>%
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
  ) %>%
  ggplot(
    aes(
      x = reorder_within(
        sectors,
      ),
      fill = modes
    )
  ) +
  geom_histogram(
    stat = "count",
    position = "stack"
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
    values = c("#4d64ce", "#bfc7f9")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      colour = "#134373",
      margin = margin(10, 10, 30, 10, "pt"),
      hjust = 0.5
    ),
    # legend.position = "bottom",
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
    "2022_frequency_of_sectors_being_max.png"
  ),
  height = 13,
  width = 20
)

################################
#### SECTORS FREQUENTLY MAX ####
################################

df %>%
  group_by(
    adm0_pcode,
    adm_pcode,
    pop_group
  ) %>%
  mutate(
    max_pin = pin == max(pin)
  ) %>%
  group_by(
    adm0_pcode,
    sector
  ) %>%
  summarize(
    max_times = sum(max_pin),
    .groups = "drop"
  ) %>%
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
    "sectoral_pins",
    "2022_freq_sector_max.png"
  ),
  height = 8,
  width = 12
)

#####################################
#### DISTRIBUTION OF MAX AND MIN ####
#####################################

df_max_min %>%
  ungroup() %>%
  mutate(
    `2nd max PiN` = second_max_pin / max_pin,
    `Min PiN` = min_pin / max_pin
  ) %>%
  select(adm0_pcode, ends_with(" PiN")) %>%
  pivot_longer(-adm0_pcode) %>%
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
    "sectoral_pins",
    "2022_distribution_min_2nd_max_pins.png"
  ),
  height = 8,
  width = 12
)
