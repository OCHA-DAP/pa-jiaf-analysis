library("ggplot2")
library("tidyverse")
library("scales")
library("ggcorrplot")
library("GGally")


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
)

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
  pivot_wider(names_from = sector, values_from = pin, , values_fn = list) %>%
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
  pivot_wider(names_from = sector, values_from = pin, , values_fn = list) %>%
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
