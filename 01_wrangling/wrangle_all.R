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
        "gp",
        "pro-gen pro",
        "protection_general",
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
        "gbv",
        "pro-gbv",
        "gb"
      ) ~ "Protection (GBV)",
      sector %in% c(
        "hlp",
        "ltb",
        "protection_hlp",
        "pro-hlp"
      ) ~ "Protection (HLP)",
      # Droit au Logement, <U+00E0> la Terre et aux Biens
      sector %in% c(
        "ma",
        "minas",
        "lam",
        "protection_aor",
        "pro-ma"
      ) ~ "Protection (MA)",
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
      ) ~ "Intersectoral",
      sector == "intersectoral_unadjusted" ~ "Intersectoral (raw)"
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

# cleaning out NA and 0 severities
df_sev_data %>%
  filter(
    !is.na(severity) & severity > 0
  ) %>%
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
  filter(
    !is.na(severity) & severity > 0
  ) %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_indicator_sev.csv"
    )
  )

# read MSNA dataset and prepare it for alignement with sectoral PiN
df_msna <- read_csv(file.path(
  file_paths$agg_dir,
  "MSNA_data.csv"
)) %>%
  mutate(
    across(
      .cols = protection_lsg:livelihoods_lsg,
      .fns = ~ case_when(
        . == "4+" ~ 5,
        is.na(.) ~ NA_real_,
        TRUE ~ as.numeric(.)
      )
    ),
    across(
      .cols = matches("admin[1-3]"),
      .fns = ~ gsub(
        "[ ]|[-]|[.]|[_]|[,]",
        "",
        tolower(stringi::stri_trans_general(.x, "latin-ascii"))
      )
    )
  ) %>%
  pivot_longer(
    cols = ends_with("_lsg"),
    names_to = "sector",
    values_to = "severity"
  ) %>%
  transmute(
    uuid,
    adm0_pcode = admin0,
    adm_name = case_when(
      adm0_pcode == "COL" ~ admin1,
      !is.na(admin3_hno) ~ admin3_hno,
      admin2_hno == "baidoa" ~ "baydhaba",
      !is.na(admin2_hno) ~ admin2_hno,
      !is.na(admin1) ~ admin1
    ),
    sector = case_when(
      sector == "edu_lsg" ~ "Education",
      sector == "foodsec_lsg" ~ "FS/FSL",
      sector == "health_lsg" ~ "Health",
      sector == "markets_er_liv_lsg" ~ "ERL",
      sector == "protection_lsg" ~ "Protection",
      sector == "shelter_lsg" ~ "Shelter",
      sector == "wash_lsg" ~ "WASH"
    ),
    severity,
    weight = as.numeric(weights)
  )

df_msna %>%
  write_csv(
    file.path(
      file_paths$agg_dir,
      "2022_msna_wrangled.csv"
    )
  )
