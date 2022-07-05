fetch_random_pin_data <-
  function(admin_level = 2,
           gender_age_disagg = FALSE,
           pop_group_disagg = FALSE) {

    # read sectoral PiN
    df <- read_csv(
      file.path(
        file_paths$output_dir,
        "datasets",
        "2022_hno_pin_cluster_totals.csv"
      )
    ) %>%
      mutate(
        adm_name = case_when(
          adm0_pcode == "COL" ~ tolower(adm1_name),
          !is.na(adm3_name) ~ tolower(adm3_name),
          !is.na(adm2_name) ~ tolower(adm2_name),
          TRUE ~ tolower(adm1_name)
        ),
        adm_name = gsub("[ ]|[-]|[.]|[_]|[,]", "", adm_name),
        adm_pcode = case_when(
          admin_level < 2 ~ adm1_pcode,
          admin_level > 2 ~ adm3_pcode,
          TRUE ~ adm2_pcode
        ),
        age = case_when(
          is.na(age) ~ NA_character_,
          age %in% c("< 18 ans", "child", "below_18", "children") ~ "Children",
          TRUE ~ "Adult",
        ),
        sex = case_when(
          is.na(sex) ~ NA_character_,
          sex %in% c("female", "Femme", "f") ~ "Female",
          sex %in% c("male", "Homme", "m") ~ "Male"
        ),
        population_group = case_when(
          population_group %in% c(
            "idp",
            "idps",
            "IDPs",
            "In-Camp-IDPs",
            "Out-of-Camp-IDPs",
            "displaced",
            "pdi",
            "PDI"
          ) ~ "IDPs",
          population_group %in% c(
            "APV",
            "Autres populations",
            "vulnerable_people",
            "vul",
            "other"
          ) ~ "Other-vulnerable-people",
          population_group == "shock_affected" ~ "Shock-affected",
          population_group %in% c(
            "refugees",
            "ref_car",
            "ref_nga",
            "ref_other",
            "ref",
            "Refugees"
          ) ~ "Refugees",
          population_group %in% c(
            "host",
            "Non-displaced",
            "non_displaced",
            "non_pdi",
            "residents",
            "Residents"
          ) ~ "Residents",
          population_group %in% c(
            "ret",
            "Retournes",
            "return",
            "Rapatries",
            "returnees",
            "Returnees"
          ) ~ "Returnees"
        )
      ) %>%
      arrange(
        adm0_pcode,
        adm1_pcode,
        adm2_pcode,
        adm3_pcode,
        sector,
        population_group,
        sex,
        desc(age)
      )

    # read MSNA dataset and prepare it for alignement with sectoral PiN
    df_msna <- read_csv(file.path(
      dirname(getwd()),
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
      filter(
        !is.na(severity),
        admin0 %in% df$adm0_pcode
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
        adm_pcode = df$adm_pcode[match(adm_name, df$adm_name)],
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
      ) %>%
      filter(!is.na(adm_pcode)) %>%
      select(-adm_name)

    df_sectors <- df

    if (gender_age_disagg) {
      df_sectors <- df_sectors %>%
        filter(!is.na(sex) & !is.na(age))
    }

    if (pop_group_disagg) {
      df_sectors <- df_sectors %>%
        filter(!is.na(population_group))
    }

    temp <- df_sectors %>%
      mutate(
        disaggregation = case_when(
          gender_age_disagg & pop_group_disagg ~
            paste(
              population_group,
              sex,
              age
            ),
          pop_group_disagg ~ population_group,
          gender_age_disagg ~ paste(sex, age),
          TRUE ~ NA_character_
        )
      ) %>%
      filter(
        adm0_pcode %in% unique(df_msna$adm0_pcode),
        adm_pcode %in% df_msna$adm_pcode
      ) %>%
      group_by(
        adm0_pcode,
        adm_pcode,
        disaggregation,
        sector
      ) %>%
      summarize(
        pin = sum(round(pin), na.rm = TRUE),
        affected_population = sum(round(affected_population), na.rm = TRUE),
        .groups = "drop_last"
      ) %>%
      mutate(affected_population = max(affected_population, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(
        adm0_pcode,
        adm_pcode,
        disaggregation,
        sector
      )

    df_data <- temp %>%
      filter(adm0_pcode == sample(unique(temp$adm0_pcode), 1)) %>%
      pivot_wider(
        names_from = sector,
        values_from = pin,
        values_fill = 0
      )

    if (gender_age_disagg & pop_group_disagg) { # nolint
      df_data <- df_data %>%
        separate(disaggregation,
          into = c(
            "Population group",
            "Sex",
            "Age"
          ),
          " "
        )
    } else if (gender_age_disagg) {
      df_data <- df_data %>%
        separate(disaggregation,
          into = c(
            "Sex",
            "Age"
          ),
          " "
        )
    } else if (pop_group_disagg) {
      df_data <- df_data %>%
        rename(`Population group` = "disaggregation")
    } else {
      df_data <- df_data %>%
        select(-disaggregation)
    }

    df_msna <- df_msna %>%
      filter(adm_pcode %in% df_data$adm_pcode) %>%
      select(-adm0_pcode) %>%
      arrange(
        adm_pcode,
        uuid,
        sector
      )

    df_area_name <- df %>%
      filter(
        !is.na(adm_pcode),
        adm0_pcode %in% df_data$adm0_pcode
      ) %>%
      transmute(
        adm1_pcode = gsub("[A-Z]+", "XY", adm1_pcode),
        adm2_pcode = gsub("[A-Z]+", "XY", adm2_pcode),
        adm3_pcode = gsub("[A-Z]+", "XY", adm3_pcode),
        adm_pcode
      ) %>%
      unique()

    if (admin_level == 2) {
      df_area_name <- df_area_name %>%
        select(-adm3_pcode)
    }

    if (admin_level < 2) {
      df_area_name <- df_area_name %>%
        select(-adm2_pcode)
    }

    df_data_output <- left_join(
      df_area_name,
      df_data
    ) %>%
      select(-adm_pcode, -adm0_pcode) %>%
      filter(!is.na(affected_population)) %>%
      rename(`Affected population` = affected_population) %>%
      arrange(
        adm1_pcode
      ) %>%
      mutate(
        ID = row_number(),
        .before = "adm1_pcode"
      )

    names(df_data_output) <- gsub(
      "adm",
      "Admin",
      names(df_data_output)
    )

    names(df_data_output) <- gsub(
      "_pcode",
      " Pcode",
      names(df_data_output)
    )

    df_msna_output <- full_join(
      df_area_name,
      df_msna
    ) %>%
      select(-adm_pcode) %>%
      filter(!is.na(uuid)) %>%
      pivot_wider(
        names_from = sector,
        values_from = severity
      ) %>%
      rename(hh_id = "uuid") %>%
      mutate(
        across(
          .cols = -c(matches("adm"), hh_id, weight),
          ~ case_when(
            . >= 3 ~ "in need",
            . < 3 ~ "not in need",
            is.na(.) ~ "not applicable"
          )
        )
      )

    names(df_msna_output) <- gsub(
      "adm",
      "Admin",
      names(df_msna_output)
    )

    names(df_msna_output) <- gsub(
      "_pcode",
      " Pcode",
      names(df_msna_output)
    )

    write_csv(
      df_data_output,
      file.path(
        file_paths$output_dir,
        "datasets",
        "simulation_data",
        "2022_simulation_sectoral_pin.csv"
      )
    )

    write_csv(
      df_msna_output,
      file.path(
        file_paths$output_dir,
        "datasets",
        "simulation_data",
        "2022_simulation_msna.csv"
      )
    )
  }

library(tidyverse)
source("99_helpers/helpers.R")

file_paths <- get_paths_analysis()

fetch_random_pin_data(pop_group_disagg = TRUE)
