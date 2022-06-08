library(tidyverse)
library(janitor)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

#################
#### LOADING ####
#################

# load data with only aggregate columns + sector + pin
df <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
) %>%
  select(
    ends_with("pcode"),
    administration,
    population_group,
    sex,
    age,
    sector,
    pin
  )

df_is <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_totals.csv"
  )
) %>%
  filter(
    sector_group == "intersectoral"
  ) %>%
  select(
    adm0_pcode,
    pin_hrp = pin
  )

###################
#### FUNCTIONS ####
###################

# calculates the max pin
# based on available data
max_pin <- function(df) {
  grouped_df <- df %>%
    group_by(
      across(-c(sector, pin))
    ) %>%
    summarize(
      pin = max(pin),
      .groups = "drop_last"
    )

  # only if we have enough groups
  if (ncol(grouped_df) > 2) {
    grouped_df <- grouped_df %>%
      summarize(
        pin = sum(pin),
        agg_groups_n = n(),
        agg_groups_name = names(.)[ncol(.) - 1],
        .groups = "drop"
      ) %>%
      group_by(
        adm0_pcode
      ) %>%
      summarize(
        agg_cols = paste(
          c(names(.)[1:(ncol(.) - 3)], unique(agg_groups_name)),
          collapse = ", "
        ),
        agg_cols_n = ncol(.) - 3,
        agg_groups_n = mean(agg_groups_n),
        agg_groups_name = unique(agg_groups_name),
        pin = sum(pin),
        .groups = "drop"
      )
  } else {
    grouped_df <- grouped_df %>%
      group_by(
        adm0_pcode
      ) %>%
      summarize(
        agg_cols = paste(
          names(.)[1],
          collapse = ", "
        ),
        agg_cols_n = ncol(.) - 2,
        pin = sum(pin),
        .groups = "drop"
      )
  }
  grouped_df
}

# tests all combinations for specific adm0
tester <- function(df) {
  # remove aggregation columns with no data
  df <- remove_empty(df, which = "cols")

  # for all agg columns, calculate max pin for those columns
  # then drop last column and calculate for next column
  n <- ncol(df)
  map_dfr(
    (n - 2):1,
    function(x) {
      cols <- c(names(df)[1:x], "sector")
      df %>%
        group_by(
          across(
            all_of(cols)
          )
        ) %>%
        summarize(
          pin = sum(pin),
          .groups = "drop"
        ) %>%
        max_pin()
    }
  )
}

#################
#### TESTING ####
#################

results_df <- df %>%
  group_by(adm0_pcode) %>%
  group_split() %>%
  map_dfr(tester)

results_df %>%
  write_csv(
    file.path(
      file_paths$output_dir,
      "datasets",
      "2022_hno_max_test.csv"
    )
  )

################
#### VISUAL ####
################

# check PiN for level of disaggregation

df_aggr_results <- results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    perc_pin_to_lowest = pin / pin[1]
  ) %>%
  filter(agg_cols_n != 0)

df_aggr_results %>%
  ggplot(
    aes(
      y = perc_pin_to_lowest,
      x = agg_cols_n,
      fill = agg_cols_n
    )
  ) +
  geom_bar(stat = "identity") +
  facet_grid(
    adm0_pcode ~ "",
    scales = "free_y"
  ) +
  coord_flip() +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    y = "PiN (% of max PiN for most disaggregated data)",
    fill = "Levels of disaggregation",
    title = "Comparison of PiNs by level of disaggregation",
    caption = paste0(
      "Disaggregations go 1 to 6 from administrative ",
      "boundaries to population groups to sex and age"
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
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
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
    "2022_hno_max_test.png"
  ),
  height = 13,
  width = 8
)

write_csv(
  df_aggr_results,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_max_test.csv"
  )
)

# difference between highest a lowest aggregation

df_diff_highest_lowest <- results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  filter(
    agg_cols_n %in% c(max(agg_cols_n), 1),
    adm0_pcode != "VEN"
  ) %>%
  summarize(
    pin = pin[1] - pin[2],
    agg_diff = agg_cols_n[1] - agg_cols_n[2],
    .groups = "drop"
  )

df_diff_highest_lowest %>%
  ggplot(
    aes(
      x = pin,
      y = reorder(adm0_pcode, pin),
      fill = agg_diff
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  scale_x_continuous(
    labels = scales::comma
  ) +
  scale_fill_gradient(
    low = "#D2F2F0",
    high = "#18998F"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    y = "",
    x = "Increase in PiN",
    fill = "Additional levels\nof disaggregation",
    title = paste(
      "Increase of country PiN",
      "if calculated using multiple disaggregations"
    ),
    subtitle = "Comparing calculations from most disaggregated with admin 1"
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
    "2022_hno_max_test_absolute.png"
  ),
  height = 6.5,
  width = 10
)

write_csv(
  df_diff_highest_lowest,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_max_test_absolute.csv"
  )
)

# difference between highest and lowest aggregation, as a percent

df_perc_highest_lowest <- results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  filter(
    agg_cols_n %in% c(max(agg_cols_n), 1)
  ) %>%
  summarize(
    pin = (pin[1] - pin[2]) / pin[2],
    agg_diff = agg_cols_n[1] - agg_cols_n[2],
    .groups = "drop"
  ) %>%
  filter(
    adm0_pcode != "VEN"
  )

df_perc_highest_lowest %>%
  ggplot(
    aes(
      x = pin,
      y = reorder(adm0_pcode, pin),
      fill = agg_diff
    )
  ) +
  geom_bar(stat = "identity") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_gradient(
    low = "#D2F2F0",
    high = "#18998F"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    y = "",
    x = "Increase in PiN (% change)",
    fill = "Additional levels\nof disaggregation",
    title = paste(
      "% increase of country PiN if",
      "calculated using multiple disaggregations"
    ),
    subtitle = "Comparing calculations from most disaggregated with admin 1"
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
    "2022_hno_max_test_percent_change.png"
  ),
  height = 6.5,
  width = 11
)

write_csv(
  df_perc_highest_lowest,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_max_test_percent_change.csv"
  )
)

# see drop in % based on # of agg groups
df_perc_pin_drop <- results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  mutate(
    pin_change = (pin - lag(pin)) / lag(pin),
    agg_groups_n = lag(agg_groups_n),
    agg_groups_name = lag(agg_groups_name),
    agg_groups_type = case_when(
      str_starts(agg_groups_name, "adm") ~ "Admin",
      agg_groups_name == "population_group" ~ "Population group",
      TRUE ~ str_to_title(agg_groups_name)
    )
  ) %>%
  filter(
    !is.na(agg_groups_n)
  )

df_perc_pin_drop %>%
  ggplot(
    aes(
      y = pin_change,
      x = agg_groups_n,
      color = agg_groups_type
    )
  ) +
  geom_point() +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_color_manual(
    values = c(
      "#18998F",
      "#78D9D1",
      "#F2645A",
      "#007CE0"
    )
  ) +
  theme_minimal() +
  labs(
    y = "% reduction in PiN when aggregation removed",
    x = "Average # of unique groups in aggregation",
    color = "Type of aggregation",
    title = "Impact of aggregation on max PiN"
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
    "2022_hno_max_test_pct.png"
  ),
  height = 6,
  width = 7
)

write_csv(
  df_perc_pin_drop,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_max_test_pct.csv"
  )
)

# max agg PiN with HRP
df_compr_max_hrp23 <- results_df %>%
  group_by(
    adm0_pcode
  ) %>%
  filter(
    agg_cols_n == max(agg_cols_n)
  ) %>%
  left_join(
    df_is,
    by = "adm0_pcode"
  ) %>%
  mutate(
    pin_diff = pin - pin_hrp
  )

df_compr_max_hrp23 %>%
  ggplot() +
  geom_segment(
    aes(
      y = reorder(adm0_pcode, pin_diff),
      yend = reorder(adm0_pcode, pin_diff),
      x = 0,
      xend = pin_diff
    ),
    lwd = 3,
    color = "#1EBFB3"
  ) +
  scale_x_continuous(
    labels = scales::comma
  ) +
  theme_minimal() +
  labs(
    y = "",
    x = "Difference in PiN, disaggregated max - JIAF 1.1",
    title = paste(
      "Difference in country PiN using disaggregated max compared to",
      "JIAF 1.1 reported PiN"
    ),
    caption = "Positive value means disaggregated max higher than JIAF 1.1 PiN"
  ) +
  geom_text(
    x = 0,
    y = "VEN",
    label = paste0(
      "5 countries' reported JIAF 1.1\n",
      "PiNs match the disaggregated max"
    ),
    check_overlap = TRUE
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
    "2022_hno_max_vs_jiaf.png"
  ),
  height = 6,
  width = 8
)

write_csv(
  df_compr_max_hrp23,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_hno_max_vs_jiaf.csv"
  )
)
