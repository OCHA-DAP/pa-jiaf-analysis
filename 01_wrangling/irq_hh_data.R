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
        population_group = case_when(
          .x == "In-Camp-IDPs" ~ "idp_in",
          .x == "Out-of-Camp-IDPs" ~ "idp_out",
          .x == "Returnees" ~ "ret"
        ),
        target_population = pop_num
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
  skip = 1
) %>%
  clean_names() %>%
  type_convert()

population_fp <- file.path(
  file_paths$ocha_dir,
  paste(
    "Iraq 2022 HNO Final Intersectoral",
    "& Cluster PIN Estimates - Updated 20211129.xlsx"
  )
)

df_irq_pops <- read_in_disagg(population_fp) %>%
  mutate(
    target_population = ifelse(
      population_group == "idp_in" &
        adm2_name == "Al-Falluja",
      2500,
      target_population
    )
  ) %>%
  filter(target_population != 0 & adm1_name != "Total") %>%
  select(adm2_name, population_group, target_population)

df_cleaned <- df %>% 
  mutate(
    hh_id = paste0("IRQ", 1:nrow(.)),
    population_group = case_when(
      population_group == "idp_in_camp" ~ "idp_in",
      population_group == "idp_out_camp" ~ "idp_out",
      population_group == "returnee" ~ "ret"
    )
  ) %>%
  pivot_longer(cols = matches("^s[0-9]"),
               names_to = "indicator",
               values_to = "severity",
               values_drop_na = TRUE) %>%
  left_join(df_irq_pops, by = c("dist_cod" = "adm2_name", "population_group")) %>%
  transmute(
    hh_id,
    adm0_name = "Iraq",
    area = dist_cod,
    population_group,
    target_population,
    sector = case_when(
      indicator %in% c("s01", "s11", "s12", "s13") ~ "protection",
      indicator == "s02" ~ "education",
      indicator %in% c("s03", "s06") ~ "emergency recovery",
      indicator %in% c("s04", "s05") ~ "food security",
      indicator %in% c("s07", "s08") ~ "health",
      indicator == "s09" ~ "child protection",
      indicator == "s10" ~ "GBV",
      indicator %in% c("s14", "s15") ~ "SNFI",
      indicator %in% c("s16", "s17", "s18") ~ "WASH"
    ),
    indicator = as.numeric(gsub("s", "", indicator)),
    severity,
    weight
  )

write_csv(
  df_cleaned,
  file_paths$save_path_hh_data
)
