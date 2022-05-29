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

df_prepped <- df_indicators %>%
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
  )

################################
#### FUNCTION FOR SAMPLING #####
################################

ind_max <- function(df, indicators) {
  df %>%
    filter(
      indicator_number %in% !!indicators
    ) %>%
    summarize(
      pin = max(pin),
      .groups = "drop"
    ) %>%
    group_by(
      adm0_name
    ) %>%
    summarize(
      pin = sum(pin),
      n_indicators = !!length(indicators),
      .groups = "drop"
    )
}

sampler <- function(df) {
  # get number of unique indicators
  n <- max(df[["indicator_number"]])
  # group data frame for taking max
  df <- group_by(
    df,
    adm0_name,
    adm0_pcode,
    adm_pcode,
    pop_group
  )

  map_dfr(
    1:n,
    function(x) {
      map_dfr(
        1:200,
        function(y) {
          inds <- sample(1:n, x)
          ind_max(df, inds)
        }
      )
    }
  )
}

set.seed(123)

df_results <- df_prepped %>%
  group_by(
    adm0_pcode
  ) %>%
  group_split() %>%
  map_dfr(sampler)

# calculate PiN if just using critical indicators

df_crit_max <- df_prepped %>%
  filter(
    critical
  ) %>%
  summarize(
    pin = max(pin),
    n_indicators = length(unique(indicator_number)),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_name
  ) %>%
  summarize(
    pin = sum(pin),
    n_indicators = unique(n_indicators),
    .groups = "drop"
  )

###############
#### GRAPH ####
###############

df_results %>%
  ggplot(
    aes(
      x = pin,
      y = n_indicators,
      group = n_indicators
    )
  ) +
  geom_boxplot() +
  geom_point(
    data = df_crit_max,
    size = 4,
    color = "#ef6666"
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::comma
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  geom_label(
    data = data.frame(
      adm0_name = "Cameroon",
      pin = 3000000,
      n_indicators = 2
    ),
    label = "Critical\nindicators PiN",
    hjust = 0,
    size = 3,
    fill = "#ef6666",
    color = "white"
  ) +
  geom_segment(
    data = data.frame(
      adm0_name = "Cameroon",
      pin = 2780000,
      n_indicators = 2.8
    ),
    xend = 3100000,
    yend = 1.8,
    lwd = 1,
    color = "#ef6666"
  ) +
  labs(
    title = "Distribution of PiNs based on number of indicators",
    subtitle = "PiNs calculated from max of sectoral indicator PiNs",
    y = "Number of indicators",
    x = "PiN",
    caption = "Critical indicators PiN calculated from all critical indicators"
  ) +
  theme(
    plot.background = element_rect(fill = "white")
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_sectoral_pin_sampling_plot.png"
  ),
  height = 10,
  width = 12
)

# plotting number of times an indicator is max

df_prepped %>%
  slice_max(
    pin
  ) %>%
  group_by(
    adm0_name,
    indicator_desc
  ) %>%
  summarize(
    times_max = n(),
    .groups = "drop_last"
  ) %>%
  arrange(
    desc(times_max),
    .by_group = TRUE
  ) %>%
  mutate(
    indicator_desc_label = case_when( # nolint start
      indicator_desc == "IPC" ~ "IPC",
      indicator_desc == "Prévalence en % de l’allaitement exclusif chez les enfants de 0-5 mois" ~ "% breastfeeding children",
      indicator_desc == "%  d'épisodes  de paludisme" ~ "% malaria",
      indicator_desc == "% of girls / women /boys /men  without access to GBV-related services." ~ "% access to GBV",
      indicator_desc == "Number of cases or incidence rates for selected diseases relevant to the local context (malaria, COVID, others outbreak prone diseases)" ~ "Disease incidence rate",
      indicator_desc == "Cadre Harmonisé" ~ "IPC (CH)",
      indicator_desc == "Afectación por conflicto armado" ~ "Affected by armed conflict",
      indicator_desc == "Pobreza multidimensional" ~ "Multidimensional poverty",
      indicator_desc == "Acceso a agua" ~ "Access to water",
      indicator_desc == "ipc" ~ "IPC",
      indicator_desc == "clean_water_access" ~ "Access to water",
      indicator_desc == "disaster_house_damage" ~ "Damage to house",
      indicator_desc == "Food Security Index (CARI)" ~ "Food security (CARI)",
      indicator_desc == "Indicator 1: [% of HHs withouth access to an improved and accessible sufficent drinking water source (bottle water, public network, water tracking, protective well and tap accessible to the public)] + Indicator 3: [% of HHs without access to functional and accessible" ~ "Access to water",
      indicator_desc == "% of girls / boys without access to core CP services" ~ "% access to CP services",
      indicator_desc == "Health Indicator" ~ "Health",
      indicator_desc == "Indicator 1+2+3" ~ "Protection",
      indicator_desc == "# of Individuals currently living in unsustainable shelter situations" ~ "Poor shelter",
      indicator_desc == "# of people in IPC phases" ~ "IPC",
      indicator_desc == "% of school-aged children dropping out of school in the previous school year" ~ "School drop outs",
      indicator_desc == "Average time needed by school-enrolled children to access the nearest education facility" ~ "Time to access school",
      indicator_desc == "Number of HF with Basic Emergency Obstetric Care/ 500,000 population, by administrative unit" ~ "Health facilities with obstetrics",
      indicator_desc == "% of persons living in areas with mine/UXO contamination" ~ "Mine/UXO exposure",
      indicator_desc == "Proportion of oxygen beds occupied with COVID-19 patients" ~ "% oxygen beds with COVID-19 patients",
      indicator_desc == "% of IDPs who have to limit expenses even for food or have funds only for food" ~ "% IDPs limited money for food",
      TRUE ~ " "
    ),
    x_pct = 2 / n()
  ) %>% # nolint end
  ggplot(
    aes(
      x = times_max,
      y = reorder(indicator_desc, times_max)
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#ef6666"
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free"
  ) +
  geom_text(
    aes(
      label = indicator_desc_label,
      x = x_pct
    ),
    hjust = 0
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    y = "",
    x = "# of times indicator PiN was maximum",
    title = "Times an indicator PiN was maximum PiN in calculation"
  )


ggsave(
  file.path(
    file_paths$output_dir,
    "2022_indicator_max_times.png"
  ),
  height = 10,
  width = 12
)

# Percent of indicators used for max

df_max_pct <- df_prepped %>%
  mutate(
    num_pins = max(indicator_number)
  ) %>%
  slice_max(
    pin
  ) %>%
  group_by(
    adm0_name
  ) %>%
  summarize(
    max_pins = length(unique(indicator_number)),
    num_pins = unique(num_pins),
    .groups = "drop"
  ) %>%
  mutate(
    pct_max_pins = max_pins / num_pins
  )

df_max_pct %>%
  ggplot(
    aes(
      x = pct_max_pins,
      y = reorder(adm0_name, pct_max_pins)
    )
  ) +
  geom_bar(stat = "identity") +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    y = "",
    x = "% of indicator PiNs",
    title = "% of indicator PiNs ever used in max calculation"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "2022_indicator_max_pct_pins.png"
  ),
  height = 5,
  width = 8
)

df_max_pct %>%
  write_csv(
    file.path(
      file_paths$output_dir,
      "2022_indicator_max_pct_pins.csv"
    )
  )
