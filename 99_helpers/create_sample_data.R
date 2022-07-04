fetch_random_pin_data <-
  function(admin_level = 2,
           gender_age_disagg = FALSE,
           pop_group_disagg = FALSE,
           protection_subcluster = FALSE) {
    n_admin <- case_when(
      gender_age_disagg & pop_group_disagg ~ 20,
      gender_age_disagg ~ 30,
      pop_group_disagg ~ 40,
      TRUE ~ 70
    )

    # read sectoral PiN
    df_sectors <- read_csv(
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
        adm = case_when(
          admin_level < 2 ~ adm1_pcode,
          admin_level > 2 ~ adm3_pcode,
          TRUE ~ adm2_pcode
        ),
        sector = ifelse(
          !protection_subcluster & grepl("^Protection", sector),
          "Protection",
          sector
        )
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
        admin0 %in% df_sectors$adm0_pcode
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
        adm_name = df_sectors$adm[match(adm_name, df_sectors$adm_name)],
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
      filter(!is.na(adm_name))

    temp <- df_sectors %>%
      group_by(adm0_pcode) %>%
      summarize(
        n = n_distinct(adm),
        .groups = "drop"
      ) %>%
      filter(n >= n_admin)

    country_for_name <- sample(temp$adm0_pcode, 1)

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
              adm,
              population_group,
              sex,
              age
            ),
          pop_group_disagg ~ paste(adm, population_group, "-", "-"),
          gender_age_disagg ~ paste(adm, sex, age),
          TRUE ~ adm
        )
      ) %>%
      filter(
        adm0_pcode %in% unique(df_msna$adm0_pcode),
        adm %in% df_msna$adm_name
      ) %>%
      group_by(
        adm0_pcode,
        adm,
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
        adm,
        disaggregation,
        sector
      ) %>%
      filter(!sector %in% c(
        "ERL",
        "Displaced pop.",
        "CCCM",
        "Protection (HLP)"
      ))

    rand_areas <- temp %>%
      pivot_wider(
        names_from = sector,
        values_from = pin
      ) %>%
      select(adm) %>%
      unique() %>%
      filter(adm %in% sample(adm, size = n_admin))

    df_data <- temp %>%
      pivot_wider(
        names_from = sector,
        values_from = pin,
        values_fill = 0
      ) %>%
      filter(adm %in% rand_areas$adm)

    if (gender_age_disagg & pop_group_disagg) { # nolint
      df_data <- df_data %>%
        separate(disaggregation,
          into = c(
            "admin",
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
            "admin",
            "Sex",
            "Age"
          ),
          " "
        )
    } else if (pop_group_disagg) {
      df_data <- df_data %>%
        separate(disaggregation,
          into = c(
            "admin",
            "Population group"
          ),
          " "
        )
    }

    df_msna <- df_msna %>%
      filter(adm_name %in% df_data$adm) %>%
      select(-adm0_pcode)

    df_area_name <- df_sectors %>%
      select(
        adm0_name,
        adm0_pcode,
        adm1_name,
        adm1_pcode,
        adm2_name,
        adm2_pcode,
        adm3_name,
        adm3_pcode
      ) %>%
      filter(adm0_pcode == country_for_name) %>%
      unique() %>%
      janitor::remove_empty(which = "cols") %>%
      filter(row_number() %in% sample(1:nrow(.), n_admin)) %>% # nolint
      mutate(adm = df_data$adm)


    df_data <- left_join(
      df_area_name,
      df_data %>% select(-adm0_pcode, -disaggregation)
    ) %>%
      select(-adm)

    df_msna <- full_join(df_area_name,
      df_msna,
      by = c("adm" = "adm_name")
    ) %>%
      select(-adm) %>%
      filter(sector != "ERL") %>%
      pivot_wider(
        names_from = sector,
        values_from = severity
      ) %>%
      rename(hh_id = "uuid")

    write_csv(
      df_data,
      file.path(
        file_paths$output_dir,
        "datasets",
        "synthetic_data",
        paste0("2022_", tolower(df_data$adm0_name[1]), "_sectoral_pin.csv")
      )
    )

    write_csv(
      df_msna,
      file.path(
        file_paths$output_dir,
        "datasets",
        "synthetic_data",
        paste0("2022_", tolower(df_msna$adm0_name[1]), "_msna_data.csv")
      )
    )
  }
