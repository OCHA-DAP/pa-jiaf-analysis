rm(list=ls(all=T))
library(tidyverse)
library(readxl)
library(janitor)
library(expss)

# helper functions to get paths
source(here::here("99_helpers", "helpers.R"))

###################
#### FUNCTIONS ####
###################

#' @importFrom dplyr %>%
read_in_disagg <- function(fp) {
  groups <- c("In-Camp-IDPs", "Out-of-Camp-IDPs", "Returnees")
  purrr::map_dfr(
    groups,
    ~ readxl::read_excel(
      fp,
      sheet = paste0(.x, "-District"),
      skip = 4
    ) %>%
      transmute(
        adm0_name = "Iraq",
        adm0_pcode = "IRQ",
        adm1_pcode = admin1Pcode,
        adm2_pcode = admin2Pcode,
        adm1_name = gov_name,
        adm2_name = dist_name,
        population_group = .x,
        pop_num
      )
  )
}

###################
#### DATA DIRS ####
###################

population_data_path <- get_paths("Iraq")

file_paths <- get_paths_analysis()

###############################
#### HH Aggregation Method ####
###############################

ocha_fp <- file.path(
  population_data_path$ocha_dir,
  paste(
    "Iraq 2022 HNO Final Intersectoral",
    "& Cluster PIN Estimates - Updated 20211129.xlsx"
  )
)

df_irq_pops <- read_in_disagg(ocha_fp) %>%
  mutate(pop_num = ifelse(population_group == "In-Camp-IDPs" & adm2_name == "Al-Falluja", 2500, pop_num)) %>%
  filter(pop_num != 0 & adm1_name != "Total")

df <- map_dfr(list.files(paste0(file_paths$input_dir, "/hh_data"), full.names = TRUE), read_csv)

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
    hh_sectoral_mean = mean(severity),
    hh_sectoral_max = max(severity)
  ) %>%
  group_by(
    hh_id
  ) %>%
  summarize(
    hh_sectoral_max_mean = max(hh_sectoral_mean),
    hh_sectoral_max_max = max(hh_sectoral_max)
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
    aggregation_method
  ) %>%
  summarize(
    percentage = weighted.mean(severity, w = weight)
  ) %>%
  left_join(
    df_irq_pops,
    by=c("adm2_name",
         "population_group")
  ) %>%
  mutate(
    value = round(percentage * pop_num),
    calculation_level = "household indicators' severity scores of indicators at household level"
  )

####################################
##### Area Aggregation Methods #####
####################################

area_summarized <- df %>%
  group_by(adm2_name,
           population_group,
           sector,
           indicator_label,
           indicator) %>%
  mutate(severity = ifelse(severity >= 3, 1, 0)) %>%
  summarize(percentage = weighted.mean(severity, w = weight)) %>%
  left_join(df_irq_pops,
            by = c("adm2_name",
                   "population_group")) %>%
  mutate(value = round(percentage * pop_num)) 

area_pin_from_sector <- area_summarized %>%
  group_by(adm2_name,
           population_group,
           sector) %>%
  summarize(
    area_pin_sector_max = max(value, na.rm = TRUE),
    area_pin_sector_mean = round(mean(value, na.rm = TRUE))
  ) %>%
  group_by(
    adm2_name,
    population_group
  ) %>%
  summarize(
    area_pin_sector_max = max(area_pin_sector_max, na.rm = TRUE),
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
    aggregation_method = "reported",
    calculation_level = "Max of sectoral PiNs (calculated by clusters) at area level"
  )

pin_all <- rbind(
  area_pin,
  select(hh_summarized, adm2_name, population_group, aggregation_method, value, calculation_level),
  df_hno_pin
) 

#filtering the population to only area/population groups that have PiNs
df_irq_pops <- df_irq_pops %>%
  mutate(value = pop_num) %>%
  filter(
    paste0(adm2_name, population_group) %in% paste0(pin_all$adm2_name, pin_all$population_group)
  ) %>%
  mutate(aggregation_method = "Total Targetted Population",
         calculation_level = "Total Targetted Population") %>%
  select(
    aggregation_method,
    calculation_level,
    adm2_name,
    population_group,
    value
  )

pin_all <- pin_all %>%
  rbind(df_irq_pops) %>%
  group_by(
    adm0_name = "Iraq",
    aggregation_method
  ) %>%
  summarize(
    value = sum(value, na.rm = T)
  )
# 
# bar_chart <- ggplot(pin_all, aes(
#   fill = fct_rev(aggregation_method), y = value, x = reorder(aggregation_method, +value),
#   label = paste0(round(value/1000000, 2), "M")
# )) +
#   geom_col() +
#   geom_text(position = position_dodge(width = .3),hjust=-.08, size = 1.8)+
#   coord_flip() +
#   labs(
#     fill = "Methodology",
#     x = "Country",
#     y = "PIN"
#   ) +
#   theme_light() +
#   scale_y_continuous(label = comma)+
#   scale_fill_discrete() +
#   theme(legend.position = "none")
#   
# ggsave(file.path(
#   file_paths$output_dir,
#   "2022_hh_data_aggregation.png"
# ),
# width = 1920, height = 1080, units = "px", plot = bar_chart
# )


write.csv(
  pin_all,
  paste0(file_paths$output_dir, "/2022_hh_data_aggregated_pin.csv"),
  row.names = FALSE
)
