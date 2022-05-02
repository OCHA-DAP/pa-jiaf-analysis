library(ggplot2)
library(scales)
library(tidyverse)

source(here::here("99_helpers", "helpers.R"))
file_paths <- get_paths_analysis()

####################
#### TOTAL PINS ####
####################

df_pins <- read.csv(
  file.path(
    file_paths$output_dir,
    "2022_hno_pin_totals.csv"
  )
) %>%
  group_by(adm0_pcode) %>%
  mutate(
    percent_diff = (pin - pin[sector_group == "intersectoral"])
    / pin[sector_group == "intersectoral"] * 100
  ) %>%
  ungroup() %>%
  mutate(
    sector_group = case_when(
      sector_group == "intersectoral" ~ "JIAF 1.1",
      sector_group == "sectoral" ~ "JIAF 2.0",
      sector_group == "sectoral_cluster" ~ "Cluster totals"
    ),
  )

##################
#### PLOTTING ####
##################

ggplot(df_pins, aes(
  fill = sector_group, y = pin, x = adm0_pcode,
  label = ifelse(percent_diff == 0, "",
    paste0(round(percent_diff, digits = 0), "%")
  )
)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(vjust = -0.5, position = position_dodge(width = 1), size = 3) +
  labs(
    fill = "Group",
    x = "Country ISO3",
    y = "PIN"
  ) +
  scale_y_continuous(labels = comma) +
  theme_light()

ggsave(file.path(save_path, "2022_hno_pin_totals.png"),
       width = 3840, height = 2018, units = "px"
)

# Plot the contribs

df_contrib <- read.csv(
  file.path(
    save_path,
    "2022_hno_pin_contributions.csv"
  )
) %>%
  mutate(
    max_pct = as.numeric(sub("%", "", max_pct)),
    pin_pct = as.numeric(sub("%", "", pin_pct))
  ) %>%
  arrange(adm0_pcode, desc(pin_pct))

ggplot(
  df_pins %>% filter(sector_group == "sectoral"),
  aes(
    x = fct_reorder(adm0_pcode, pin),
    y = pin
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    y = "Intersectoral PIN",
    x = "Country ISO3"
  ) +
  theme(strip.text = element_text(
    size = 5),
    axis.text = element_text(
      size = 5))

ggsave(file.path(save_path, "2022_hno_pin_contributions.png"),
       width = 3840, height = 2018, units = "px", scale = 0.7
)

# difference with intersectoral
df_pins %>%
  filter(sector_group == "JIAF 2.0") %>%
  ggplot(
    aes(
      y = fct_reorder(adm0_pcode, percent_diff),
      x = percent_diff,
      fill = number_disagg
    )
  ) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_y_continuous(labels = scales::comma)

ggsave(file.path(file_paths$output_dir, "2022_hno_pin_totals.png"),
  width = 5, height = 5
)


######################
#### CORRELATIONS ####
######################

# Get the correlation data
df_corr <- read.csv(
  file.path(
    file_paths$output_dir,
    "2022_hno_pin_cluster_totals.csv"
  )
)

# First plot the correlations for the full data sample
df_corr_all <- df_corr %>%
  pivot_wider(names_from = sector, values_from = pin, values_fn = list) %>%
  unnest(cols = everything()) %>%
  select(edu:erl)

fig1 <- cor(as.matrix(df_corr_all), use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "lower", lab = TRUE, lab_size = 2)
ggsave(file.path(file_paths$output_dir, "2022_hno_pin_cluster_corr_all.png"),
  width = 5, height = 5, plot = fig1
)

fig2 <- ggpairs(df_corr_all)
ggsave(file.path(
  file_paths$output_dir,
  "2022_hno_pin_cluster_corr_all_pairs.png"
),
width = 20, height = 20, plot = fig2
)

# You can see that the distributions are skewed towards lower numbers.
# Try with logging to weight the smaller numbers more.
# TODO: fix deprecated
df_corr_all_log <- df_corr_all %>% mutate_each(funs(log10(1 + .)))
fig1 <- cor(as.matrix(df_corr_all_log), use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "lower", lab = TRUE, lab_size = 2)
ggsave(file.path(
  file_paths$output_dir,
  "2022_hno_pin_cluster_corr_all_log.png"
),
width = 5, height = 5, plot = fig1
)

fig2 <- ggpairs(df_corr_all_log)
ggsave(file.path(
  file_paths$output_dir,
  "2022_hno_pin_cluster_corr_all_pairs_log.png"
),
width = 20, height = 20, plot = fig2
)

# Finally try just the total country sums
df_corr_countries <- df_corr %>%
  group_by(adm0_pcode, sector) %>%
  summarise(pin = sum(pin)) %>%
  ungroup() %>%
  pivot_wider(names_from = sector, values_from = pin, values_fn = list) %>%
  unnest(cols = everything()) %>%
  select(edu:erl)

fig1 <- cor(as.matrix(df_corr_all_log), use = "pairwise.complete.obs") %>%
  ggcorrplot(type = "lower", lab = TRUE, lab_size = 2)
ggsave(file.path(
  file_paths$output_dir,
  "2022_hno_pin_cluster_corr_countries.png"
),
width = 5, height = 5, plot = fig1
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
      sector_group == "intersectoral"
    ) %>%
      select(
        adm0_pcode,
        intersectoral_pin = pin
      ),
    by = c("adm0_pcode")
  ) %>%
  mutate(
    across(
      edu:erl,
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

ggsave(file.path(save_path, "2022_hno_pct_difference.png"),
  width = 3840, height = 2018, units = "px"

# plot these as min to max
df_violin <- df_sectoral %>%
  filter(
    !(adm0_pcode %in% c("UKR", "COL")) # need to check data first
  ) %>%
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
    lwd = 1.5
  ) +
  geom_text(
    aes(
      y = sector,
      label = n_country
    ),
    x = 1.03,
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "% of JIAF 1.1 intersectoral PiN",
    y = "Sector",
    title = "Sectoral PiN as % of JIAF 1.1 intersectoral PiN",
    subtitle = paste0(
      "Minimum to maximum percent across ",
      "n countries (specified to the right)"
    )
  )

ggsave(file.path(file_paths$output_dir, "2022_hno_pin_percent_all.png"),
  width = 10, height = 7, plot = fig_pin_all
)

# heat map version

fig_heatmap <- df_sectoral %>%
  filter(
    !(adm0_pcode %in% c("UKR", "COL")) # need to check data first
  ) %>%
  mutate(
    sector = factor(sector, levels = levels(df_violin$sector))
  ) %>%
  ggplot() +
  geom_tile(
    aes(
      x = sector,
      y = adm0_name,
      fill = pin_percent
    )
  ) +
  theme_minimal() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_gradient(
    labels = scales::percent_format(1),
    low = "white",
    high = "#F2645A"
  ) +
  labs(
    fill = "% of JIAF 1.1 intersectoral PiN",
    y = "Country",
    x = "Sector",
    title = "Sectoral PiN as % of JIAF 1.1 intersectoral PiN"
  )


ggsave(file.path(file_paths$output_dir, "2022_hno_pin_percent_countries.png"),
  width = 10, height = 7, plot = fig_heatmap
)
