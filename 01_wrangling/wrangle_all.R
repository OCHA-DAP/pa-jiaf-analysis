library(tidyverse)
library(renv)
source(here::here("99_helpers", "helpers.R"))

# get all filepaths
file_paths <- get_paths_analysis()

# Run all file wrangling for countries
wrangle_files <- list.files("01_wrangling")
wrangle_files <- wrangle_files[wrangle_files != "wrangle_all.R"]

walk(
  wrangle_files,
  function(x) {
    print(x)
    run(
      file.path(
        "01_wrangling",
        x
      )
    )
  }
)

# clean up cluster names
generalize_sector <- function(df) {
  df %>% mutate(
    sector = tolower(sector),
    sector = case_when(
      sector %in% c("cc", "cccm", "gsat") ~ "CCCM",
      # gestion des Sites d<U+2019>Accueil Temporaire
      sector %in% c(
        "early recovery",
        "early recovery & livelihoods",
        "rt",
        "el",
        "erl",
        "mpca"
      ) ~ "ERL",
      # rt is early recovery, el is emergency livelihoods
      # and mpca is Multi-Purpose Cash Assistance
      sector %in% c(
        "education",
        "educacion",
        "educ",
        "edu",
        "ed"
      ) ~ "Education",
      sector %in% c(
        "fs",
        "fsa",
        "fss",
        "fsl",
        "fsc",
        "food",
        "food_sec",
        "fslc",
        "fsac",
        "food security",
        "food_security",
        "seguridad_alimentaria",
        "san_seguridad_alimentaria",
        "seg_alimentaria",
        "secal"
      ) ~ "FS/FSL",
      # securit<U+00E9> alimentaire
      sector %in% c(
        "sante",
        "he",
        "hea",
        "health",
        "salud",
        "san",
        "hlt",
        "heat"
      ) ~ "Health",
      sector %in% c(
        "health & nutrition",
        "nutrition",
        "nutricion",
        "san_nutrition",
        "san_nutricion",
        "nut"
      ) ~ "Nutrition",
      sector %in% c(
        "protection",
        "proteccion",
        "prot",
        "pro",
        "protection_general",
        "protection_aor",
        "prt"
      ) ~ "Protection",
      sector %in% c(
        "protection_cp",
        "pro-cp",
        "ninez",
        "cp",
        "child_protection"
      ) ~ "Protection (CP)",
      sector %in% c(
        "vbg",
        "protection_gbv",
        "pro-gen pro",
        "gbv",
        "pro-gbv",
        "gb",
        "gp"
      ) ~ "Protection (GBV)",
      sector %in% c(
        "hlp",
        "ltb",
        "protection_hlp",
        "pro-hlp"
      ) ~ "Protection (HLP)",
      # Droit au Logement, <U+00E0> la Terre et aux Biens
      sector %in% c("ma", "minas", "lam", "pro-ma") ~ "Protection (MA)",
      # lutte anti-mine
      sector %in% c(
        "abris",
        "alojamiento_energia_y_enseres",
        "alojamientos",
        "abris_ame",
        "shl",
        "snfi",
        "shelter",
        "shelter & nfis",
        "shleter&nfis",
        "shelter and nfi",
        "sn",
        "nfi"
      ) ~ "Shelter",
      sector %in% c("wash", "wa", "wsh", "eha") ~ "WASH",
      sector %in% c(
        "refugees",
        "refugee response",
        "migrants",
        "rmms"
      ) ~ "Displaced pop.",
      sector %in% c(
        "inter_sectoral",
        "intersectorial",
        "intersectoral",
        "itc"
      ) ~ "intersectoral",
      sector == "intersectoral_unadjusted" ~ "JIAF1.1"
    )
  )
}


# Sectoral PiNs

sectoral_df <- map_dfr(
  list.files(
    file_paths$input_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
) %>%
  select(
    -source
  ) %>%
  generalize_sector()

sectoral_df %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_sectoral_pins.csv"
    )
  )

# Indicator PiNs

df_indicators <- map_dfr(
  list.files(
    file_paths$input_indicator_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  ~ read_csv(.) %>% mutate(indicator_number = as.character(indicator_number))
)

df_indicators %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_indicator_pins.csv"
    )
  )

# HH Data

df_hh_data <- map_dfr(
  list.files(
    file_paths$input_hh_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
)

df_hh_data %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_hh_data.csv"
    )
  )

# Severity data

df_sev_data <- map_dfr(
  list.files(
    file_paths$input_sev_sector_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
) %>%
  select(
    -source
  ) %>%
  generalize_sector()

df_sev_data %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_sectoral_sev.csv"
    )
  )

# Indicator severity data

df_ind_sev_data <- map_dfr(
  list.files(
    file_paths$input_sev_indicator_dir,
    pattern = ".csv",
    full.names = TRUE
  ),
  read_csv
)

df_ind_sev_data %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_indicator_sev.csv"
    )
  )
