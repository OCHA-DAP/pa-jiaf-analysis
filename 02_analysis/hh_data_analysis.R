rm(list = ls(all = TRUE))
library(tidyverse)
library(readxl)
library(janitor)
library(expss)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### DATA DIRS ####
###################

file_paths <- get_paths_analysis()

###############################
#### HH AGGREGATION METHOD ####
###############################

df <- read_csv(file.path(file_paths$agg_dir, "2022_hh_data.csv"))

hh_scoring_method_1 <- df %>%
  group_by(hh_id) %>%
  slice_max(
    order_by = severity,
    prop = 0.25,
    with_ties = FALSE
  ) %>%
  summarize(
    mean_max_25 = mean(severity),
    median_max_25 = median(severity)
  )

hh_scoring_method_2 <- df %>%
  group_by(hh_id) %>%
  slice_max(
    order_by = severity,
    prop = 0.5,
    with_ties = FALSE
  ) %>%
  summarize(
    mean_max_50 = mean(severity),
    median_max_50 = median(severity)
  )

hh_scoring_method_3 <- df %>%
  group_by(hh_id) %>%
  slice_max(
    order_by = severity,
    prop = 0.75,
    with_ties = FALSE
  ) %>%
  summarize(
    mean_max_75 = mean(severity),
    median_max_75 = median(severity)
  )

hh_scoring_method_4 <- df %>%
  group_by(hh_id) %>%
  summarize(
    mean_all = mean(severity),
    median_all = median(severity)
  )

hh_scoring_method_5 <- df %>%
  group_by(
    hh_id,
    sector
  ) %>%
  summarize(hh_sectoral_mean = mean(severity)) %>%
  group_by(hh_id) %>%
  summarize(hh_sectoral_max_mean = max(hh_sectoral_mean))

hh_scoring_method_6 <- df %>%
  group_by(hh_id) %>%
  summarize(any_indicator_inneed = sum(severity >= 3, na.rm = TRUE)) %>%
  mutate(
    two_indicator_inneed = ifelse(any_indicator_inneed > 1, 3, 0),
    any_indicator_inneed = ifelse(any_indicator_inneed > 0, 3, 0)
  )

hh_scoring_method <-
  left_join(
    hh_scoring_method_1,
    hh_scoring_method_2
  ) %>%
  left_join(hh_scoring_method_3) %>%
  left_join(hh_scoring_method_4) %>%
  left_join(hh_scoring_method_5) %>%
  left_join(hh_scoring_method_6) %>%
  pivot_longer(
    cols = -hh_id,
    values_to = "severity",
    names_to = "aggregation_method"
  ) %>%
  left_join(unique(select(df, -c(
    severity, sector, indicator
  ))), by = "hh_id") %>%
  mutate(severity = ifelse(severity >= 3, 1, 0))

# freeing up space of ram
remove(
  hh_scoring_method_1,
  hh_scoring_method_2,
  hh_scoring_method_3,
  hh_scoring_method_4,
  hh_scoring_method_5,
  hh_scoring_method_6
)

hh_summarized <- hh_scoring_method %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
    target_population,
    aggregation_method
  ) %>%
  summarize(
    percentage = weighted.mean(severity, w = weight),
    .groups = "drop"
  ) %>%
  mutate(
    value = round(percentage * target_population),
    calculation_level = "HH indicators' severity scores"
  ) %>%
  select(-c(target_population, percentage))

####################################
##### AREA AGGREGATION METHODS #####
####################################

area_summarized <- df %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
    target_population,
    sector,
    indicator
  ) %>%
  mutate(severity = ifelse(severity >= 3, 1, 0)) %>%
  summarize(percentage = weighted.mean(severity, w = weight)) %>%
  mutate(value = round(percentage * target_population))

area_pin_from_sector <- area_summarized %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
    sector
  ) %>%
  summarize(area_pin_sector_mean = round(mean(value, na.rm = TRUE))) %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name, population_group
  ) %>%
  summarize(area_pin_sector_mean = max(area_pin_sector_mean, na.rm = TRUE))

area_pin_from_indicator <- area_summarized %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group
  ) %>%
  summarize(
    area_pin_indicator_max = max(value, na.rm = TRUE),
    area_pin_indicator_mean = round(mean(value, na.rm = TRUE))
  )

area_pin <-
  left_join(
    area_pin_from_indicator,
    area_pin_from_sector,
    by = c(
      "adm0_name",
      "adm1_name",
      "adm2_name",
      "adm3_name",
      "population_group"
    )
  ) %>%
  pivot_longer(
    cols = matches("^area_pin"),
    values_to = "value",
    names_to = "aggregation_method"
  ) %>%
  filter(!is.infinite(value) & !is.nan(value)) %>%
  mutate(calculation_level = "Area level PiN")

df_ocha_pin <- read_csv(
  file.path(
    file_paths$agg_dir,
    "2022_sectoral_pins.csv"
  )
) %>%
  filter(
    adm0_pcode %in% c("IRQ", "SOM", "SYR")
  )

df_hno_pin <- df_ocha_pin %>%
  filter(
    sector_group == "intersectoral"
  ) %>%
  group_by(
    adm0_name
  ) %>%
  summarize(
    value = sum(pin, na.rm = TRUE)
  ) %>%
  mutate(
    aggregation_method = "HNO 2022 intersectoral PiN",
    calculation_level = "HNO 2022 intersectoral PiN"
  )

df_cluster_pin <- df_ocha_pin %>%
  filter(
    sector_group == "sectoral"
  ) %>%
  group_by(
    adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
  ) %>%
  summarize(
    value = max(pin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_name
  ) %>%
  summarize(
    value = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    aggregation_method = "Option 1 (no adjustment)",
    calculation_level = "Option 1 (no adjustment)"
  )

# filtering the population to only area/population groups that have PiNs
df_pops <- df %>%
  group_by(adm0_name,
    adm1_name,
    adm2_name,
    adm3_name,
    population_group,
    drop = TRUE
  ) %>%
  summarize(value = max(target_population, na.rm = TRUE)) %>%
  mutate(
    aggregation_method = "Targeted population",
    calculation_level = "Targeted population"
  )

pin_all <-
  rbind(
    df_pops,
    area_pin,
    hh_summarized
  ) %>%
  group_by(adm0_name,
    calculation_level,
    aggregation_method,
    .drop = TRUE
  ) %>%
  summarize(value = sum(value, na.rm = TRUE)) %>%
  rbind(
    df_hno_pin,
    df_cluster_pin
  ) %>%
  mutate(
    aggregation_method = case_when(
      aggregation_method == "any_indicator_inneed" ~ "In need any indicator",
      aggregation_method == "two_indicator_inneed" ~ "In need 2+ indicators",
      aggregation_method == "mean_max_25" ~ "Mean (top 25%)",
      aggregation_method == "median_max_25" ~ "Median (top 25%)",
      aggregation_method == "mean_max_50" ~ "Mean (top 50%)",
      aggregation_method == "median_max_50" ~ "Median (top 50%)",
      aggregation_method == "mean_max_75" ~ "Mean (top 75%)",
      aggregation_method == "median_max_75" ~ "Median (top 75%)",
      aggregation_method == "mean_all" ~ "Mean (100%)",
      aggregation_method == "median_all" ~ "Median (100%)",
      aggregation_method == "area_pin_indicator_max" ~ "Max (indicator PiNs)",
      aggregation_method == "area_pin_indicator_mean" ~ "Mean (indicator PiNs)",
      aggregation_method == "area_pin_sector_mean" ~ "Mean (sector PiNs)",
      TRUE ~ aggregation_method
    ),
    value = round(value / 1000000, 2)
  ) %>%
  arrange(adm0_name, desc(value)) %>%
  filter(aggregation_method != "hh_sectoral_max_mean")

bar_chart <-
  ggplot(
    pin_all,
    aes(
      fill = calculation_level,
      y = value,
      x = reorder(aggregation_method, +value),
      label = paste0(value, "M")
    )
  ) +
  facet_wrap(
    ~adm0_name,
    strip.position = "bottom",
    scales = "free_x"
  ) +
  geom_col() +
  geom_text(
    position = position_identity(),
    hjust = -.2,
    size = 4
  ) +
  coord_flip() +
  labs(
    fill = "",
    x = "",
    y = "PIN",
    title = paste0(
      "Option 3: Comparison of HH severity aggregation, indicator PiN, ",
      "and sectoral PiN"
    )
  ) +
  theme_light() +
  scale_y_continuous(
    labels = function(x) paste0(x, "M"),
    expand = expansion(c(0, .2))
  ) +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, width = 35)
    }
  ) +
  scale_fill_manual(
    values = c("#18998F", "#78D9D1", "#0063B3", "#66B0EC", "#000000")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 30, 10, "pt"),
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
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent"),
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
    "2022_hh_data_aggregations.png"
  ),
  bg = "transparent",
  height = 13,
  width = 25,
  plot = bar_chart
)

write_csv(
  pin_all,
  file.path(file_paths$output_dir, "/2022_hh_data_aggregated_pin.csv")
)
