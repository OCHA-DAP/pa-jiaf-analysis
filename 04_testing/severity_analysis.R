library(tidyverse)
library(tidytext)
library(ggcorrplot)

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
      y = Intersectoral,
      fill = perc
    )
  ) +
  geom_tile(
    color = "#888888"
  ) +
  geom_text(
    aes(
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
    y = "Intersectoral (raw)",
    title = "Comparison of raw and adjusted severity",
    x = "Intersectoral (adjusted)",
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
    title = "Relationship of between adjusted severity and PiN %"
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

cor(
  df_corr,
  use = "pairwise.complete.obs"
) %>%
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
    "2022_hno_compr_sector_is_tiles.csv"
  )
)
