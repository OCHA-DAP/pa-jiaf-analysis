rm(list=ls(all=T))
library(tidyverse)
library(readxl)
library(janitor)
library(srvyr)
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
        adm1_pcode = admin1Pcode,
        adm2_pcode = admin2Pcode,
        adm1_name = gov_name,
        adm2_name = dist_name,
        population_group = case_when(.x == "In-Camp-IDPs" ~ "idp_in_camp",
                                     .x == "Out-of-Camp-IDPs" ~ "idp_out_camp",
                                     .x == "Returnees" ~ "returnee") ,
        pop_num
      )
  )
}

###################
#### DATA DIRS ####
###################

file_paths <- get_paths("Iraq")

############################
#### OCHA PROVIDED DATA ####
############################

ocha_fp <- file.path(
  file_paths$ocha_dir,
  paste(
    "Iraq 2022 HNO Final Intersectoral",
    "& Cluster PIN Estimates - Updated 20211129.xlsx"
  )
)

df_pops <- read_in_disagg(ocha_fp)
# %>%
#   filter(pop_num != 0 & adm1_name != "Total") %>%
#   mutate()

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
  col_names = F
)

#list of indicators
list_indicators <- data.frame(t(df[1:2,]), row.names = NULL) %>%
  rename(indicator_name = X1, indicator_code = X2) %>%
  filter(grepl("^s[0-9]", indicator_code))

#renaming the dataset
names(df) <- df[2, ]

df_cleaned <- df %>% filter(row_number() > 2) %>%
  clean_names() %>%
  select(
    adm1_name = gov_cod,
    adm1_pcode = gov_cod_pc,
    adm2_name = dist_cod,
    adm2_pcode = dist_cod_pc,
    population_group,
    weight,
    list_indicators$indicator_code
  ) %>%
  mutate_at(list_indicators$indicator_code,
            ~ ifelse(. >= 3, 1, 0)) %>%
  mutate(weight = as.numeric(weight),
         hh_inneed = ifelse(sum_row(across(list_indicators$indicator_code)) > 0, 1, 0))

df_cleaned[, list_indicators$indicator_code] <-
  lapply(df_cleaned[, list_indicators$indicator_code], FUN = as.numeric)
         

df_svy <- as_survey_design(
  df_cleaned, 
  weights = weight
)

df_summarized <- df_svy %>% 
  group_by(
    population_group,
    adm2_name
  ) %>%
  summarise_at(
    .vars = c(list_indicators$indicator_code, "hh_inneed"),
    ~ survey_mean(., na.rm = T, vartype = "ci", level = 0.90)
    ) %>%
  select(-contains("low"), -contains("upp"))

df_pin <- df_summarized %>%
  mutate(pop_num = df_pops$pop_num[match(
    paste0(adm2_name, population_group),
    paste0(df_pops$adm2_name, df_pops$population_group)
  )],
  #Al-Falluja's population for in camps was blank
  #checked from other files and it's 2,500
  pop_num = ifelse(pop_num == 0, 2500, pop_num)) %>%
  mutate_at(c(list_indicators$indicator_code, "hh_inneed"),
            ~ round(. * pop_num)) %>%
  mutate(
    indicator_pin = max_row(across(list_indicators$indicator_code)),
    #mine action and hlp are part of general protection
    prot_sector = max_row(across(c("s01", "s11", "s12", "s13"))), 
    edu_sector = s02,
    livl_sector = max_row(across(c("s03", "s06"))),
    fs_sector = max_row(across(c("s04", "s05"))),
    health_sector = max_row(across(c("s07", "s08"))),
    cp_sector = s09,
    gbv_sector = s10,
    snfi_sector = max_row(across(c("s14", "s15"))),
    wash_sector = max_row(across(c("s16", "s17", "s18"))),
    sectoral_pin = max_row(across(c("prot_sector",
                                "edu_sector",
                                "livl_sector",
                                "fs_sector",
                                "health_sector",
                                "cp_sector",
                                "gbv_sector",
                                "snfi_sector",
                                "wash_sector")))
  )

df_pin <- df_pin %>% 
  transmute(
  adm1_name = df_pops$adm1_name[match(adm2_name, df_pops$adm2_name)],
  adm1_pcode = df_pops$adm1_pcode[match(adm2_name, df_pops$adm2_name)],
  adm2_name,
  adm2_pcode = df_pops$adm2_pcode[match(adm2_name, df_pops$adm2_name)],
  population_group,
  pop_num,
  hh_inneed,
  indicator_pin,
  sectoral_pin
)

write_csv(
  df_pin,
  file.path(
    get_paths_analysis()$output_dir,
    "iraq_household_data_analyzed"
  )
)

















