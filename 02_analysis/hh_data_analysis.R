rm(list=ls(all=T))
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
#### HH Aggregation Method ####
###############################

df <- map_dfr(list.files(file.path(file_paths$input_dir, "hh_data"), full.names = TRUE), read_csv)

hh_scoring_method_1 <- df %>%
  group_by(
    hh_id
  ) %>%
  slice_max(order_by = severity, prop = 0.25, with_ties = FALSE) %>%
  summarize(
    mean_max_25 = mean(severity),
    median_max_25 = median(severity)
  )

hh_scoring_method_2 <- df %>%
  group_by(
    hh_id
  ) %>%
  slice_max(order_by = severity, prop = 0.5, with_ties = FALSE) %>%
  summarize(
    mean_max_50 = mean(severity),   
    median_max_50 = median(severity)
  )

hh_scoring_method_3 <- df %>%
  group_by(
    hh_id
  ) %>%
  slice_max(order_by = severity, prop = 0.75, with_ties = FALSE) %>%
  summarize(
    mean_max_75 = mean(severity),
    median_max_75 = median(severity)
  )

hh_scoring_method_4 <- df %>%
  group_by(
    hh_id
  ) %>%
  summarize(
    mean_all = mean(severity),
    median_all = median(severity)
  )

hh_scoring_method_5 <- df %>%
  group_by(
    hh_id,
    sector
  ) %>%
  summarize(
    hh_sectoral_mean = mean(severity)
  ) %>%
  group_by(
    hh_id
  ) %>%
  summarize(
    hh_sectoral_max_mean = max(hh_sectoral_mean)
  )

hh_scoring_method_6 <- df %>%
  group_by(
    hh_id
  ) %>%
  summarize(
    any_indicator_inneed = sum(severity >= 3, na.rm = T)
  ) %>%
  mutate(
    two_indicator_inneed = ifelse(any_indicator_inneed > 1, 3, 0),
    any_indicator_inneed = ifelse(any_indicator_inneed > 0, 3, 0)
  )

hh_scoring_method <-
  left_join(hh_scoring_method_1,
            hh_scoring_method_2) %>%
  left_join(hh_scoring_method_3) %>%
  left_join(hh_scoring_method_4) %>%
  left_join(hh_scoring_method_5) %>%
  left_join(hh_scoring_method_6) %>%
  pivot_longer(
    cols = -hh_id,
    values_to = "severity",
    names_to = "aggregation_method"
  ) %>%
  left_join(
    unique(select(
    df,-c(severity, sector, indicator_label, indicator))
    ), by = "hh_id") %>%
  mutate(
    severity = ifelse(severity >= 3, 1, 0)
  )

#freeing up space of ram
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
    adm2_name,
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
    calculation_level = "from household indicators' severity scores"
  ) %>%
  select(-c(target_population, percentage))

####################################
##### Area Aggregation Methods #####
####################################

area_summarized <- df %>%
  group_by(adm2_name,
           population_group,
           target_population,
           sector,
           indicator_label,
           indicator) %>%
  mutate(severity = ifelse(severity >= 3, 1, 0)) %>%
  summarize(percentage = weighted.mean(severity, w = weight)) %>%
  mutate(value = round(percentage * target_population)) 

area_pin_from_sector <- area_summarized %>%
  group_by(adm2_name,
           population_group,
           sector) %>%
  summarize(
    area_pin_sector_mean = round(mean(value, na.rm = TRUE))
  ) %>%
  group_by(
    adm2_name,
    population_group
  ) %>%
  summarize(
    area_pin_sector_mean = max(area_pin_sector_mean, na.rm = TRUE)
  )

area_pin_from_indicator <- area_summarized %>%
  group_by(adm2_name,
           population_group) %>%
  summarize(
    area_pin_indicator_max = max(value, na.rm = TRUE),
    area_pin_indicator_mean = round(mean(value, na.rm = TRUE))
  )

area_pin <-
  left_join(
    area_pin_from_indicator,
    area_pin_from_sector,
    by = c("adm2_name", "population_group")
  ) %>%
  pivot_longer(
    cols = matches("^area"),
    values_to = "value",
    names_to = "aggregation_method"
  ) %>%
  filter(
    !is.infinite(value) & !is.nan(value)
  ) %>%
  mutate(
    calculation_level = "indicators' or sectors' PiN at area level"
  )

df_hno_pin <- read.csv(paste0(file_paths$input_dir, "/irq_pins_2022.csv")) %>%
  filter(sector != "itc") %>%
  group_by(
    adm2_name,
    population_group
  ) %>%
  summarize(
    value = max(pin, na.rm = TRUE)
  ) %>% 
  mutate(
    aggregation_method = "HNO 2022 Intersectoral PiN (without expert judgement)",
    calculation_level = "HNO 2022 Intersectoral PiN"
  )

pin_all <- rbind(
  area_pin,
  hh_summarized,
  df_hno_pin
) 

#filtering the population to only area/population groups that have PiNs
df_irq_pops <- df %>%
  group_by(
    adm2_name,
    population_group
  ) %>%
  summarize(
    value = max(target_population, na.rm = TRUE)
  ) %>%
  mutate(aggregation_method = "Total Targetted Population",
         calculation_level = "Total Targetted Population")

pin_all <- pin_all %>%
  rbind(df_irq_pops) %>%
  group_by(
    adm0_name = "Iraq",
    calculation_level,
    aggregation_method
  ) %>%
  summarize(
    value = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    aggregation_method = case_when(
      aggregation_method == "any_indicator_inneed" ~ "households being in need for any indicator",
      aggregation_method == "two_indicator_inneed" ~ "households being in need for at least two indicators",
      aggregation_method == "hh_sectoral_max_mean" ~ "max of sectoral severity which is calculated from mean of their sectoral indicators",
      aggregation_method == "mean_max_25" ~ "mean of 25% of indicators with maximum severity",
      aggregation_method == "median_max_25" ~ "median of 25% of indicators with maximum severity",
      aggregation_method == "mean_max_50" ~ "mean of 50% of indicators with maximum severity",
      aggregation_method == "median_max_50" ~ "median of 50% of indicators with maximum severity",
      aggregation_method == "mean_max_75" ~ "mean of 75% of indicators with maximum severity",
      aggregation_method == "median_max_75" ~ "median of 75% of indicators with maximum severity",
      aggregation_method == "mean_all" ~ "mean of all indicators' severity",
      aggregation_method == "median_all" ~ "median of all indicators' severity",
      aggregation_method == "area_pin_indicator_max" ~ "max of all indicators' PiN",
      aggregation_method == "area_pin_indicator_mean" ~ "mean of all indicators' PiN",
      aggregation_method == "area_pin_sector_mean" ~ "mean of all sectors' PiN",
      TRUE ~ aggregation_method
    )
  )

#importing roboto fonts
hrbrthemes::import_roboto_condensed()

bar_chart <- ggplot(pin_all, aes(
  fill = fct_rev(calculation_level), y = value, x = reorder(aggregation_method, +value),
  label = paste0(round(value/1000000, 2), "M")
)) +
  geom_col() +
  geom_text(position = position_dodge(width = .3),hjust=-.3, size = 4)+
  coord_flip() +
  labs(
    fill = "PiN Calculation",
    x = "",
    y = "PIN",
    title = "Option3: Comparison of HH  severity aggregation vs indicator pin vs sectoral pin from Iraq MSNA dataset"
  ) +
  theme_light() +
  scale_y_continuous(label = scales::comma)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 30)) +
  scale_fill_manual(values = c("#B00020", "#D1C4E9", "#00E676", "#FFE0B2")) +
  theme(
    plot.title = element_text(face = "bold", size = 22, colour = "#134373", margin = margin(10, 10, 30, 10, "pt")),
    axis.text = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold"),
    legend.position = c(.85, .1),
    panel.background = element_rect(fill='transparent'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
    )


ggsave(file.path(
  file_paths$output_dir,
  "2022_hh_data_aggregation.png"
),
bg = "transparent",
height = 13,
width = 20, 
plot = bar_chart
)


write_csv(
  pin_all,
  file.path(file_paths$output_dir, "/2022_hh_data_aggregated_pin.csv")
)
