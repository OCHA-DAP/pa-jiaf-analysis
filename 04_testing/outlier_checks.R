library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

df <- read_csv(
  file.path(
    file_paths$output_dir,
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
  mutate(perc_pin = (pin / affected_population) * 100)

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
    `lower outlier` = sum(is_lower_outlier, na.rm = TRUE) / n()
  ) %>%
  filter(`upper outlier` + `lower outlier` != 0) %>%
  pivot_longer(
    cols = matches("(upper|lower) outlier"),
    names_to = "type",
    values_to = "value"
  ) %>%
  ggplot(aes(x = sector, y = value * 100, fill = type)) +
  geom_col() +
  facet_wrap(~adm0_pcode, scales = "free")

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_percentage_of_times_sectors_being_outlier.png"
  ),
  height = 13,
  width = 20
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
  ggplot(aes(x = perc_of_pop, fill = summary_mode)) +
  geom_density(na.rm = TRUE) +
  facet_wrap(~adm0_pcode, scales = "free_y") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    y = "Density of Max PiN",
    title = "Distribution of Max PiN as percentage of the affected population",
    x = "Percentage of Max PiN of the affected population"
  ) +
  scale_fill_manual(
    values = "#FFE0B2"
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
    "2022_max_pin_density.png"
  ),
  height = 13,
  width = 20
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
  group_by(
    adm0_pcode,
    summary_mode
  ) %>%
  summarize(
    value = sum(value) / sum(affected_population)
  ) %>%
  ggplot(aes(y = value, x = summary_mode)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~adm0_pcode, scales = "free")

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_max_pin_density.png"
  ),
  height = 13,
  width = 20
)

## density of percentage of PiN for the samll sectors
df_small_clusters %>%
  ggplot(aes(x = perc_pin, color = sector)) +
  geom_density() +
  facet_wrap(~adm0_pcode, scales = "free_y") +
  labs(
    y = "Density of Max PiN",
    title = paste0(
      "Distribution of Percentage pf PiN as a percentage of the",
      "affected population for Nutrition, Education,",
      "Child Protection and GBV"
    ),
    x = "Percentage of PiN of the affected population"
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
  ggplot(aes(x = sectors, fill = modes)) +
  geom_histogram(stat = "count", position = "dodge") +
  facet_wrap(~adm0_pcode, scales = "free")

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_frequency_of_sectors_being_max.png"
  ),
  height = 13,
  width = 20
)
