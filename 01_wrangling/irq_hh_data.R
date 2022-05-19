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

file_paths <- get_paths("Iraq")

############################
#### MSNA Indicator DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
    "Iraq 2022 HNO MCNA Intersectoral Composites and Aggregation Template [Working].xlsx"
)

df <- read_excel(
  ocha_fp,
  sheet = "data",
  col_names = FALSE
)

population_fp <- file.path(
  file_paths$ocha_dir,
  paste(
    "Iraq 2022 HNO Final Intersectoral",
    "& Cluster PIN Estimates - Updated 20211129.xlsx"
  )
)

df_irq_pops <- read_in_disagg(population_fp) %>%
  mutate(pop_num = ifelse(population_group == "In-Camp-IDPs" & adm2_name == "Al-Falluja", 2500, pop_num)) %>%
  filter(pop_num != 0 & adm1_name != "Total") %>%
  select(adm2_name, population_group, pop_num)

#list of indicators
list_indicators <- df %>% 
  filter(row_number() < 3) %>% 
  t() %>%
  data.frame(row.names = NULL) %>%
  rename(indicator_name = X1, indicator_code = X2) %>%
  filter(grepl("^s[0-9]", indicator_code))

#renaming the dataset
names(df) <- df[2, ]

df_cleaned <- df %>% 
  filter(row_number() > 2) %>%
  clean_names() %>% 
  type_convert() %>%
  mutate(
    hh_id = paste0("IRQ", 1:nrow(.))
  ) %>%
  pivot_longer(cols = matches("^s[0-9]"),
               names_to = "indicator",
               values_to = "severity",
               values_drop_na = TRUE) %>%
  left_join(list_indicators, by=c("indicator" = "indicator_code")) %>%
  left_join(df_irq_pops, by = c("dist_cod" = "adm2_name", "population_group")) %>%
  transmute(
    hh_id,
    adm0_name = "IRAQ",
    adm0_pcode = "IRQ",
    adm1_name = gov_cod,
    adm1_pcode = gov_cod_pc,
    adm2_name = dist_cod,
    adm2_pcode = dist_cod_pc,
    population_group = case_when(
      population_group == "idp_in_camp" ~ "In-Camp-IDPs",
      population_group == "idp_out_camp" ~ "Out-of-Camp-IDPs",
      population_group == "returnee" ~ "Returnees"
    ),
    target_population = df_irq_pops$pop_num[match(
      paste0(adm2_name, population_group),
      paste0(df_irq_pops$adm2_name, df_irq_pops$population_group)
    )],
    sector = case_when(
      indicator %in% c("s01", "s11", "s12", "s13") ~ "protection",
      indicator == "s02" ~ "education",
      indicator %in% c("s03", "s06") ~ "livelihoods emergency",
      indicator %in% c("s04", "s05") ~ "food security",
      indicator %in% c("s07", "s08") ~ "health",
      indicator == "s09" ~ "child protection",
      indicator == "s10" ~ "GBV",
      indicator %in% c("s14", "s15") ~ "SNFI",
      indicator %in% c("s16", "s17", "s18") ~ "WASH"
    ),
    indicator_label = indicator_name,
    indicator,
    severity,
    weight
  )

write_csv(
  df_cleaned,
  file_paths$save_path_hh_data
)
