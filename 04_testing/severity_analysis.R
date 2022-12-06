library(tidyverse)
library(tidytext)
library(ggcorrplot)
library(gghdx)

gghdx()

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

######################
#### Reading Data ####
######################

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
  ) %>%
  group_by(
    adm0_pcode,
    sum_above_2
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    perc = n / sum(n)
  )

temp %>%
  ggplot() +
  geom_bar(
    aes(
      x = sum_above_2,
      y = perc
    ),
    fill = "#1EBFB3",
    na.rm = TRUE,
    stat = "identity"
  ) +
  facet_wrap(~adm0_pcode, scales = "fixed") +
  labs(
    y = "% of areas",
    title = "Distribution of # of sectors that are 3 or above",
    x = "# of sectors with severity 3 or above"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks()
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
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
    "2022_number_sectors_3_above.png"
  ),
  height = 6,
  width = 10,
  units = "in"
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_number_sectors_3_above.csv"
  )
)

##########################
#### AREAS 3 OR ABOVE ####
##########################

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
    y = "",
    x = "",
    title = "% of geographical areas with severity 3 or above"
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
  width = 13,
  height = 8,
  units = "in"
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_sector_severity_percent.csv"
  )
)

# percentage overlap between sectors across all countries
percent_overlap <- function(df, severity_cutoff = 3) {
  severity_cutoff <- case_when(
    severity_cutoff > 5 ~ 5,
    severity_cutoff < 1 ~ 1,
    TRUE ~ severity_cutoff
  )
  df_sev <- df %>%
    mutate(severity = ifelse(severity >= severity_cutoff, 1, 0)) %>%
    arrange(sector)

  df_corr_all <- df_sev %>%
    pivot_wider(
      names_from = sector,
      values_from = severity,
      values_fn = list
    ) %>%
    unnest(cols = everything()) %>%
    right_join(df_sev) %>%
    filter(severity == 1)

  df_corr_summarize <- df_corr_all %>%
    select(
      sector,
      sort(unique(df_corr_all$sector))
    ) %>%
    group_by(sector) %>%
    summarise(across(
      .cols = everything(),
      ~ ifelse(
        sum(is.na(.x), na.rm = TRUE) == n(),
        NA_real_,
        sum(.x, na.rm = TRUE) / n()
      )
    ))

  temp <- df_corr_summarize %>%
    pivot_longer(
      cols = -sector
    ) %>%
    mutate(
      value = round(value * 100)
    )

  temp %>%
    ggplot(
      aes(
        y = sector,
        x = name,
        fill = value,
        label = ifelse(is.na(value), "", paste0(value, "%"))
      )
    ) +
    geom_tile() +
    geom_text(
      position = "identity",
      na.rm = TRUE
    ) +
    labs(
      title = paste(
        "% of areas with sectoral severity",
        ifelse(severity_cutoff == 5, "5", paste(severity_cutoff, "or above")),
        "overlapping with other sectors in the same range of severity"
      ),
      x = "Numerator",
      y = "Denominator",
      fill = "% of areas overlapping"
    ) +
    scale_fill_gradient(
      low = "#ebfffd",
      high = "#1EBFB3",
      na.value = "white",
      labels = scales::percent_format(scale = 1)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 18,
        margin = margin(10, 10, 10, 10, "pt"),
        family = "Roboto",
        hjust = .5
      ),
      axis.text.y = element_text(
        face = "bold",
        size = 12,
        family = "Roboto"
      ),
      axis.text.x = element_text(
        face = "bold",
        size = 12,
        family = "Roboto",
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.title.y = element_text(
        face = "bold",
        size = 16,
        family = "Roboto",
        margin = margin(r = 10)
      ),
      axis.title.x = element_text(
        face = "bold",
        size = 16,
        family = "Roboto",
        margin = margin(t = 10)
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
      paste0(
        "2022_hno_perc_sector_severity_",
        ifelse(severity_cutoff == 5, "5", paste0(severity_cutoff, "_above")),
        "_overlap.png"
      )
    ),
    width = 14,
    height = 11,
    units = "in"
  )

  write_csv(
    df_corr_summarize,
    file.path(
      file_paths$output_dir_sev,
      "graphs",
      "datasets",
      paste0(
        "2022_hno_perc_sector_severity_",
        ifelse(severity_cutoff == 5, "5", paste0(severity_cutoff, "_above")),
        "_overlap.csv"
      )
    )
  )
}

##########################
#### SEVERITY OVERLAP ####
##########################

df_sev <- df %>%
  filter(
    !is.na(severity),
    sector != "Intersectoral (raw)",
    severity > 0
  ) %>%
  select(
    adm0_pcode,
    disaggregation,
    sector,
    severity
  )

percent_overlap(df_sev, 3)
percent_overlap(df_sev, 4)
percent_overlap(df_sev, 5)

#########################################
#### DISTRIBUTION OF SEVERITY SCORES ####
#########################################

# percent of severity scores for each sector across all geographical areas
temp <- df_sev %>%
  group_by(
    sector,
    severity
  ) %>%
  summarise(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(perc = n / sum(n))

temp %>%
  mutate(
    sector = str_replace(
      sector,
      " ",
      "\n"
    )
  ) %>%
  ggplot(
    aes(
      y = perc,
      x = severity,
      fill = severity,
      label = paste0(round(perc * 100), "%")
    )
  ) +
  geom_bar(stat = "identity") +
  geom_text(
    vjust = -.9,
    size = 2.8,
    hjust = .4
  ) +
  facet_grid(
    cols = vars(sector),
    scales = "fixed",
    switch = "x"
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "",
    y = "",
    fill = "Severity Scores",
    title = paste0(
      "% of geographical areas with severity scores of 1 to 5, ",
      "all sectors across all countries"
    )
  ) +
  scale_fill_gradient(
    low = "#D2F2F0",
    high = "#18998F"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 11,
      family = "Roboto",
      face = "bold",
      vjust = 1
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_perc_sector_severities.png"
  ),
  width = 15,
  height = 8,
  units = "in"
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_perc_sector_severities.csv"
  )
)

# PiN and Severity relationship

df_relation <- df %>%
  filter(
    !is.na(severity) &
      !is.na(pin) &
      severity > 0 &
      !is.na(affected_population) &
      sector != "Intersectoral (raw)" &
      !(adm0_pcode %in% c("BDI", "NGA", "TCD")) # they only have intersectoral
  ) %>%
  mutate(
    affected_population = ifelse(pin > affected_population,
      pin,
      affected_population
    )
  ) %>%
  transmute(
    adm0_pcode,
    disaggregation,
    sector,
    perc_pin = pin / affected_population,
    severity
  )

df_relation %>%
  ggplot(
    aes(
      x = severity,
      y = perc_pin,
      group = severity
    )
  ) +
  geom_boxplot(
    color = "#1EBFB3"
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  labs(
    y = "PiN (% of affected population)",
    title = paste(
      "% of population in need relative to area level severity,",
      "all sectors"
    ),
    x = "Severity score"
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
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
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
    "2022_hno_severity_pin_distribution_country.png"
  ),
  width = 12,
  height = 8,
  units = "in"
)

##################################
#### SEVERITY - PIN BY SECTOR ####
##################################

df_relation %>%
  ggplot(
    aes(
      x = severity,
      y = perc_pin,
      group = severity
    )
  ) +
  geom_boxplot(
    color = "#1EBFB3"
  ) +
  facet_wrap(
    ~sector
  ) +
  labs(
    y = "PiN (% of affected population)",
    title = paste(
      "% of population in need relative to area level severity,",
      "all countries"
    ),
    x = "Severity score"
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
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
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
    "2022_hno_severity_pin_distribution_sector.png"
  ),
  width = 12,
  height = 8,
  units = "in"
)

write_csv(
  df_relation,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_severity_pin_distribution.csv"
  )
)

# comparison of JIAF 1.1 and adjusted Intersectoral severity

df_jiaf_is <- df %>%
  filter(sector_general == "intersectoral" &
    !is.na(severity) &
    disaggregation %in% df$disaggregation[sector == "Intersectoral (raw)"])

df_is_compare <- df_jiaf_is %>%
  pivot_wider(
    names_from = sector,
    values_from = severity
  ) %>%
  group_by(
    adm0_pcode,
    Intersectoral,
    `Intersectoral (raw)`
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  complete(
    adm0_pcode,
    Intersectoral = 1:5,
    `Intersectoral (raw)` = 1:5
  )

df_is_compare %>%
  mutate(
    perc_lab = ifelse(
      perc > 0,
      paste0(round(100 * perc), "%"),
      ""
    )
  ) %>%
  ggplot(
    aes(
      x = `Intersectoral (raw)`,
      y = Intersectoral
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "#888888"
  ) +
  geom_text(
    aes(
      fill = perc,
      label = perc_lab
    ),
    family = "Roboto"
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3",
    na.value = "white",
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "Intersectoral (raw)",
    title = "Comparison of raw and adjusted severity",
    y = "Intersectoral (adjusted)",
    fill = "% of areas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  geom_segment(
    data = data.frame(
      x = c(1:4 + 0.5, 1:4 + 0.5),
      xend = c(2:5 + 0.5, 1:4 + 0.5),
      y = c(1:4 + 0.5, 0:3 + 0.5),
      yend = c(1:4 + 0.5, 1:4 + 0.5)
    ),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    color = "#007CE0"
  ) +
  geom_segment(
    data = data.frame(
      x = c(0:3 + 0.5, 1:4 + 0.5),
      xend = c(1:4 + 0.5, 1:4 + 0.5),
      y = c(1:4 + 0.5, 1:4 + 0.5),
      yend = c(1:4 + 0.5, 2:5 + 0.5)
    ),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    color = "#18998F"
  ) +
  geom_text(
    data = tibble(
      Intersectoral = c(1.5, 4.5),
      "Intersectoral (raw)" = c(4.5, 1.5),
      label = c(
        "Adjusted\ndownward",
        "Adjusted\nupward"
      ),
      adm0_pcode = "BDI"
    ),
    aes(
      label = label
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_compr_jiaf_intersectoral.png"
  ),
  width = 10,
  height = 6,
  units = "in"
)

write_csv(
  df_is_compare,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_jiaf_intersectoral.csv"
  )
)

####################################
#### ADJUSTMENT AND PIN PERCENT ####
####################################

df_adjust_pop <- df_jiaf_is %>%
  pivot_wider(
    names_from = sector,
    values_from = severity
  ) %>%
  mutate(
    pin_perc = pin / affected_population,
    adjustment = Intersectoral - `Intersectoral (raw)`
  ) %>%
  filter(
    !is.na(affected_population)
  )

df_adjust_pop %>%
  ggplot() +
  geom_boxplot(
    aes(
      group = adjustment,
      x = adjustment,
      y = pin_perc
    ),
    color = "#1EBFB3"
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_continuous(
    breaks = c(-1, 0, 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "Severity adjustment",
    y = "PiN (% of affected population)",
    title = "Relationship between adjusted severity and PiN %"
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_compr_adjustment_pop.png"
  ),
  width = 10,
  height = 6,
  units = "in"
)

write_csv(
  df_adjust_pop,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_adjustment_pop.csv"
  )
)

#########################################
#### RELATIONSHIP PIN POP AND ADJUST ####
#########################################

df_adjust_same_sev <- df_adjust_pop %>%
  group_by(
    adm0_pcode,
    `Intersectoral (raw)`,
    Intersectoral
  ) %>%
  summarize(
    pin_perc = mean(pin_perc),
    .groups = "drop_last"
  ) %>%
  filter(
    n() > 1
  ) %>%
  mutate(
    adjustment = factor(
      case_when(
        Intersectoral == `Intersectoral (raw)` ~ "none",
        Intersectoral > `Intersectoral (raw)` ~ "increase",
        TRUE ~ "decrease"
      ),
      levels = c("decrease", "none", "increase")
    )
  )

df_adjust_same_sev %>%
  ggplot(
    aes(
      x = pin_perc,
      y = adjustment
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#1EBFB3",
    width = 0.75
  ) +
  facet_wrap(
    ~adm0_pcode
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  scale_y_discrete(
    labels = c("Decrease", "None", "Increase")
  ) +
  labs(
    x = "Mean PiN (% of affected population)",
    y = "Severity adjustment, with same raw severity",
    title = "Relationship between adjusted severity and PiN %"
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_compr_adjustment_pop_same_sev.png"
  ),
  width = 8,
  height = 4,
  units = "in"
)

write_csv(
  df_adjust_same_sev,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_adjustment_pop_same_sev.csv"
  )
)


###################################################
#### RELATIONSHIP SECTORAL SUM & INTERSECTORAL ####
###################################################

df_sum_sev <- df %>%
  filter(
    sector == "Intersectoral" | sector_general == "sectoral",
    !(adm0_pcode %in% c("NGA", "BDI", "TCD", "BFA", "MLI")), # no sector data,
    !(adm0_pcode %in% c("LBY", "VEN")), # insufficient intersector data
  ) %>%
  group_by(
    adm0_pcode,
    disaggregation,
    sector_general
  ) %>%
  summarize(
    severity = mean(severity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sector_general,
    values_from = severity
  ) %>%
  filter(
    intersectoral > 0
  )

df_sum_sev %>%
  ggplot(
    aes(
      x = intersectoral,
      y = sectoral,
      group = intersectoral
    )
  ) +
  geom_boxplot(
    color = "#1EBFB3"
  ) +
  facet_wrap(
    ~adm0_pcode,
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
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
    x = "Intersectoral severity",
    y = "Mean of sectoral severity",
    title = "Intersectoral severity relative to sectoral severity"
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_compr_mean_sector_inter.png"
  ),
  width = 10,
  height = 6,
  units = "in"
)

write_csv(
  df_sum_sev,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_mean_sector_inter.csv"
  )
)

##############################################
#### SECTORAL TO INTERSECTORAL COMPARISON ####
##############################################

df_sect_is_comp <- df %>%
  filter(
    sector_general != "intersectoral",
    !(adm0_pcode %in% c("NGA", "BDI", "TCD", "BFA", "MLI")), # no sector data
    !(adm0_pcode %in% c("LBY", "VEN")), # insufficient intersector data
    !is.na(severity),
    severity > 0
  ) %>%
  left_join(
    df %>%
      filter(
        sector == "Intersectoral"
      ) %>%
      select(
        disaggregation,
        intersectoral = severity
      ),
    by = "disaggregation"
  ) %>%
  group_by(
    sector,
    severity,
    intersectoral
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(
    sector
  ) %>%
  mutate(
    perc = n / sum(n)
  ) %>%
  complete(
    sector,
    severity = 1:5,
    intersectoral = 1:5
  )

df_sect_is_comp %>%
  mutate(
    perc_lab = ifelse(
      perc > 0,
      paste0(round(100 * perc), "%"),
      ""
    )
  ) %>%
  ggplot(
    aes(
      x = intersectoral,
      y = severity
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    ),
    color = "#888888"
  ) +
  geom_text(
    aes(
      label = perc_lab
    ),
    family = "Roboto"
  ) +
  geom_segment(
    data = data.frame(
      x = c(1:4 + 0.5, 1:4 + 0.5),
      xend = c(2:5 + 0.5, 1:4 + 0.5),
      y = c(1:4 + 0.5, 0:3 + 0.5),
      yend = c(1:4 + 0.5, 1:4 + 0.5)
    ),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    color = "#18998F"
  ) +
  geom_segment(
    data = data.frame(
      x = c(0:3 + 0.5, 1:4 + 0.5),
      xend = c(1:4 + 0.5, 1:4 + 0.5),
      y = c(1:4 + 0.5, 1:4 + 0.5),
      yend = c(1:4 + 0.5, 2:5 + 0.5)
    ),
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend
    ),
    color = "#007CE0"
  ) +
  geom_text(
    data = data.frame(
      sector = "Protection (HLP)",
      severity = c(4.5, 2),
      intersectoral = c(2, 4.5),
      label = c(
        "Sectoral severity higher\nthan intersectoral",
        "Sectoral severity\nlower than\nintersectoral"
      )
    ),
    aes(
      label = label
    ),
    family = "Roboto",
    size = 2.5
  ) +
  facet_wrap(
    ~sector
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3",
    na.value = "white",
    labels = scales::percent_format(1)
  ) +
  labs(
    y = "Sectoral severity",
    title = "Comparison of sectoral and intersectoral severity",
    x = "Intersectoral severity",
    fill = "% of areas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
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
    "2022_hno_compr_sector_is_tiles.png"
  ),
  width = 10,
  height = 8,
  units = "in"
)

write_csv(
  df_sect_is_comp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_sector_is_tiles.csv"
  )
)

#########################################################
#### JUST LOOK AT FREQUENCY OF BEING ABOVE AND BELOW ####
#########################################################

df_sect_is_comp2 <- df %>%
  filter(
    sector_general != "intersectoral",
    !(adm0_pcode %in% c("NGA", "BDI", "TCD", "BFA", "MLI")), # no sector data
    !(adm0_pcode %in% c("LBY", "VEN")), # insufficient intersector data
    !is.na(severity),
    severity > 0
  ) %>%
  left_join(
    df %>%
      filter(
        sector == "Intersectoral"
      ) %>%
      select(
        disaggregation,
        intersectoral = severity
      ),
    by = "disaggregation"
  ) %>%
  mutate(
    is_status = case_when(
      intersectoral < severity ~ "lower",
      intersectoral == severity ~ "same",
      intersectoral > severity ~ "higher"
    )
  ) %>%
  filter(
    !is.na(is_status)
  ) %>%
  group_by(
    sector,
    severity,
    is_status
  ) %>%
  summarize(
    n = n(),
    .groups = "drop_last"
  ) %>%
  mutate(
    perc = n / sum(n)
  )


df_sect_is_comp2 %>%
  complete(
    severity = 1:5,
    is_status = c(
      "higher", "lower", "same"
    ),
    fill = list(
      n = 0,
      perc = 0
    )
  ) %>%
  mutate(
    is_status = factor(is_status, levels = c("lower", "same", "higher"))
  ) %>%
  ggplot(
    aes(
      y = severity,
      x = is_status
    )
  ) +
  geom_tile(
    aes(
      fill = perc
    )
  ) +
  facet_wrap(
    ~sector
  ) +
  labs(
    y = "Sectoral severity",
    title = "Comparison of sectoral and intersectoral severity",
    x = "Intersectoral severity (relative to sectoral)",
    fill = "% of areas"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = .5
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.y = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(r = 20)
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      family = "Roboto",
      margin = margin(t = 20)
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.key.width = unit(1, "cm"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  scale_fill_gradient(
    low = "white",
    high = "#1EBFB3",
    labels = scales::percent_format()
  )


ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_compr_sector_is_tiles_high_low.png"
  ),
  width = 10,
  height = 8,
  units = "in"
)

write_csv(
  df_sect_is_comp2,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_compr_sector_is_tiles_high_low.csv"
  )
)

##################################
#### CORRELATIONS BETWEEN ALL ####
##################################

df_corr <- df %>%
  select(
    adm0_name,
    disaggregation,
    sector,
    severity
  ) %>%
  filter(
    sector != "Intersectoral (raw)"
  ) %>%
  pivot_wider(
    names_from = sector,
    values_from = severity
  ) %>%
  select(
    -c(adm0_name, disaggregation)
  ) %>%
  as.matrix()

df_correlations <- cor(
  df_corr,
  use = "pairwise.complete.obs"
)

df_correlations %>%
  ggcorrplot(
    type = "lower",
    lab = TRUE,
    lab_size = 2,
    colors = c("#F2645A", "white", "#1EBFB3"),
    title = "Severity correlations, sectoral and intersectoral"
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
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_severity_corr.png"
  ),
  width = 10,
  height = 8,
  units = "in"
)

write_csv(
  df_sect_is_comp,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_severity_corr.csv"
  )
)

#########################################################
#### AVERAGE SECTORAL CORRELATIONS VS IS CORRELATION ####
#########################################################

avg_sector_corr <- df_correlations %>%
  as.data.frame() %>%
  slice(-1) %>%
  summarize(
    across(
      ERL:`Protection (HLP)`,
      function(x) {
        x <- x[x != 1]
        mean(x, na.rm = TRUE)
      }
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = "sector",
    values_to = "sector_corr"
  )

is_corr <- df_correlations %>%
  as.data.frame() %>%
  select(
    -Intersectoral
  ) %>%
  slice(1) %>%
  pivot_longer(
    everything(),
    names_to = "sector",
    values_to = "is_corr"
  )

df_corr_avg <- full_join(
  is_corr,
  avg_sector_corr,
  by = "sector"
)

df_corr_avg %>%
  ggplot(
    aes(
      x = sector_corr,
      y = is_corr
    )
  ) +
  geom_point(
    size = 3,
    color = "#1EBFB3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto",
      hjust = 0
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
  ) +
  labs(
    y = "Correlation with intersectoral severity",
    x = "Average correlations with other sectors",
    title = paste(
      "Sectoral correlations and correlation with",
      "final intersectoral severity"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_hno_severity_corr_avg.png"
  ),
  width = 8,
  height = 4,
  units = "in"
)

write_csv(
  df_corr_avg,
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "datasets",
    "2022_hno_severity_corr_avg.csv"
  )
)

########################
#### CHECK ANALYSIS ####
########################

# original proposal

df_sev_test <- df %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  filter(
    sector_general != "intersectoral"
  ) %>%
  summarize(
    is_severity = case_when(
      sum(severity >= 5) / n() >= 0.2 ~ 5,
      sum(severity >= 4) / n() >= 0.7 ~ 5,
      sum(severity >= 3) / n() >= 0.7 ~ 4,
      sum(severity >= 4) / n() >= 0.4 ~ 4,
      sum(severity >= 3) / n() >= 0.4 ~ 3,
      sum(severity >= 4) / n() >= 0.2 ~ 3,
      sum(severity >= 2) / n() >= 0.4 ~ 2,
      sum(severity >= 3) / n() >= 0.1 ~ 2,
      TRUE ~ 1
    )
  )

df_sev_test %>%
  ggplot() +
  geom_bar(
    aes(
      x = is_severity
    )
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free_y"
  ) +
  labs(
    x = "Intersectoral severity",
    y = "# of areas",
    title = "Test of intersectoral severity"
  )

# original intersectoral severity

df %>%
  filter(
    sector_general == "intersectoral"
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = severity
    )
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free_y"
  ) +
  labs(
    x = "Intersectoral severity",
    y = "# of areas",
    title = "Original intersectoral severity"
  )


df_sev_test %>%
  left_join(
    df %>%
      filter(
        sector_general == "intersectoral"
      ) %>%
      select(
        adm0_name,
        disaggregation,
        orig_severity = severity
      )
  ) %>%
  group_by(
    adm0_name,
    is_severity,
    orig_severity
  ) %>%
  summarize(
    n = n(),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_name
  ) %>%
  mutate(
    pct = n / sum(n)
  ) %>%
  filter(
    !is.na(orig_severity),
    adm0_name != "Somalia"
  ) %>%
  ggplot() +
  geom_tile(
    aes(
      x = is_severity,
      y = orig_severity,
      fill = pct
    )
  ) +
  facet_wrap(
    ~adm0_name
  ) +
  labs(
    y = "Original severity",
    x = "Testing severity",
    fill = "% of units",
    title = "Relationship between original and tested intersectoral severity"
  ) +
  geom_text(
    aes(
      x = is_severity,
      y = orig_severity,
      label = scales::percent(pct, accuracy = 1)
    )
  ) +
  scale_fill_continuous(
    labels = scales::percent_format()
  )


#######################
#### ANALYSIS IDEA ####
#######################

pcts <- seq(0.4, 0.8, 0.01)

df_sev5_test <- map_dfr(
  .x = pcts,
  .f = ~ df %>%
    group_by(
      adm0_name,
      disaggregation
    ) %>%
    filter(
      sector_general != "intersectoral"
    ) %>%
    summarize(
      threshold = .x,
      severity_5 = sum(severity >= 5) / n() >= .x,
      .groups = "drop_last"
    ) %>%
    summarize(
      threshold = unique(threshold),
      pct_severity_5 = sum(severity_5) / n()
    )
)

df_sev5_test %>%
  ggplot(
    aes(
      x = threshold,
      y = pct_severity_5
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_wrap(
    ~adm0_name,
    scales = "free_y"
  ) +
  labs(
    x = "Threshold: % of sectors in 4+",
    y = "% of units in phase 5",
    title = "Impact of % threshold on phase 5 classifications"
  )

##############################
#### % sectors in phase 5 ####
##############################

df %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  summarize(
    n_5 = sum(severity == 5),
    n_4 = sum(severity == 4),
    .groups = "drop"
  ) %>%
  group_by(
    adm0_name,
    n_5,
    n_4
  ) %>%
  summarize(
    n = n()
  ) %>%
  group_by(
    adm0_name
  ) %>%
  mutate(
    pct = n / sum(n)
  ) %>%
  ggplot(
    aes(
      x = n_4,
      y = n_5
    )
  ) +
  geom_tile(
    aes(
      fill = pct
    )
  ) +
  facet_wrap(
    ~adm0_name
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks()
  ) +
  scale_fill_continuous(
    labels = scales::percent_format()
  ) +
  geom_text(
    aes(
      label = scales::percent(pct, accuracy = 1)
    ),
    size = 3
  ) +
  geom_vline(
    xintercept = 2.5,
    color = hdx_hex("tomato-hdx")
  ) +
  geom_hline(
    yintercept = 0.5,
    color = hdx_hex("tomato-hdx")
  ) +
  labs(
    x = "# of sectors in phase 4",
    y = "# of sectors in phase 5",
    fill = "% of units of analysis",
    title = "Instances of sectors in phase 4 and phase 5"
  ) +
  geom_label_hdx(
    data = data.frame(
      adm0_name = "Burkina Faso",
      n_4 = 8,
      n_5 = 5
    ),
    label = "3 or more sectors in phase 4\nand at least 1 in phase 5"
  )

######################
#### NEW ANALYSIS ####
######################

df_test_flags <- df %>%
  filter(
    sector_general != "intersectoral"
  ) %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  summarize(
    test_severity = case_when(
      any(severity == 5) & (sum(severity >= 4) / n() >= 0.5) ~ 5,
      sum(severity >= 4) / n() >= 0.5 ~ 4,
      sum(severity >= 3) / n() >= 0.5 ~ 3,
      sum(severity >= 2) / n() >= 0.5 ~ 2,
      TRUE ~ 1
    ),
    test_flag = case_when(
      sum(severity >= 4) / n() >= 0.625 ~ 5,
      sum(severity >= 3) / n() >= 0.625 ~ 4,
      sum(severity >= 2) / n() >= 0.625 ~ 3,
      any(severity >= 2) ~ 2,
      TRUE ~ 1
    ),
    .groups = "drop_last"
  ) %>%
  mutate(
    flagged = ifelse(
      test_flag > test_severity,
      "Flagged",
      "Not flagged"
    )
  )

df_test_flags %>%
  ggplot(
    aes(
      x = test_severity,
      fill = flagged
    )
  ) +
  geom_bar() +
  facet_wrap(
    ~adm0_name,
    scales = "free_y"
  ) +
  scale_fill_manual(
    values = unname(hdx_hex(c("tomato-hdx", "mint-hdx")))
  ) +
  labs(
    x = "# of units",
    y = "Severity",
    fill = "",
    title = "Test severity and flags"
  )

df %>%
  filter(
    sector_general != "intersectoral"
  ) %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  summarize(
    max_pin = max(pin),
    max_sev = max(severity),
    max_pin_sev = max(severity[pin == max(pin)]),
    .groups = "drop_last"
  ) %>%
  filter(
    !is.na(max_pin)
  ) %>%
  summarize(
    pct = sum(max_pin_sev == max_sev) / n()
  )

df %>%
  filter(
    sector_general != "intersectoral"
  ) %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  summarize(
    max_pin = max(pin),
    max_sev = max(severity),
    max_sev_pin = max(pin[severity == max(severity)]),
    max_sev_pin_pct = max_sev_pin / max_pin,
    .groups = "drop"
  ) %>%
  filter(
    !is.na(max_pin)
  ) %>%
  ggplot(
    aes(
      x = max_sev_pin_pct
    )
  ) +
  geom_histogram() +
  facet_wrap(
    ~max_sev,
    scales = "free_y"
  ) +
  labs(
    title = "Max PiN for sector with max severity, as % of overall max PiN",
    x = "Max sev PiN (% of max PiN)",
    y = ""
  )

df %>%
  filter(
    sector_general != "intersectoral"
  ) %>%
  group_by(
    adm0_name,
    disaggregation
  ) %>%
  summarize(
    max_pin = max(pin),
    max_sev = max(severity),
    min_sev_pin = min(pin[severity == max(severity)]),
    min_sev_pin_pct = min_sev_pin / max_pin,
    .groups = "drop"
  ) %>%
  filter(
    !is.na(max_pin)
  ) %>%
  ggplot(
    aes(
      x = min_sev_pin_pct
    )
  ) +
  geom_histogram() +
  facet_wrap(
    ~max_sev,
    scales = "free_y"
  ) +
  labs(
    title = "Min PiN for sector with max severity, as % of overall max PiN",
    x = "Min sev PiN (% of max PiN)",
    y = ""
  )
