library(tidyverse)
source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

#########################
#### Prepare datasets ###
#########################

df_sectors <- read_csv(
  file.path(
    file_paths$output_dir,
    "datasets",
    "2022_hno_pin_cluster_totals.csv"
  )
) %>%
  mutate(
    across(
      .cols = matches("adm[1-3]"),
      .fns = tolower
    ),
    adm_name = case_when(
      adm0_pcode == "COL" ~ adm1_name,
      !is.na(adm3_name) ~ adm3_name,
      !is.na(adm2_name) ~ adm2_name,
      TRUE ~ adm1_name
    ),
    adm_name = gsub("[ ]|[-]|[.]|[_]|[,]", "", adm_name)
  ) %>%
  group_by(
    adm0_pcode,
    adm_name,
    sector
  ) %>%
  summarize(
    pin = sum(pin, na.rm = TRUE),
    affected_population = sum(affected_population, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    affected_population = max(round(affected_population))
  ) %>%
  ungroup()

df_msna <- read_csv(
  file.path(
    dirname(getwd()),
    "MSNA_data.csv"
  )
) %>%
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
    sector = case_when(
      sector == "edu_lsg" ~ "Education",
      sector == "foodsec_lsg" ~ "FS/FSL",
      sector == "health_lsg" ~ "Health",
      sector == "markets_er_liv_lsg" ~ "ERL",
      sector == "protection_lsg" ~ "Protection",
      sector == "shelter_lsg" ~ "Shelter",
      sector == "wash_lsg" ~ "WASH"
    ),
    inneed = ifelse(severity >= 3, 1, 0),
    weight = as.numeric(weights)
  )

########################
### ANALYSE DATASETS ###
########################

msna_sectors <- unique(df_msna$sector)

df_max_sectors <- df_sectors %>%
  filter(adm0_pcode %in% unique(df_msna$adm0_pcode)) %>%
  group_by(
    adm0_pcode,
    adm_name,
    affected_population,
    .drop = TRUE
  ) %>%
  mutate(
    pin_2_std_dev = pin / (mean(pin) + 2 * sd(pin))
  ) %>%
  slice_max(
    order_by = pin,
    n = 2,
    with_ties = FALSE
  ) %>%
  summarize(
    max_pin = pin[1],
    max_sector = sector[1],
    max_2_pin = pin[2],
    max_2_sector = sector[2],
    max_2_std_dev = pin_2_std_dev[1],
    .groups = "drop"
  ) %>%
  mutate(
    max_sector = ifelse(grepl("Protection", max_sector),
      "Protection",
      max_sector
    ),
    max_2_sector = ifelse(grepl("Protection", max_2_sector),
      "Protection",
      max_2_sector
    ),
  )

df_msna_anlyse <- df_msna %>%
  full_join(df_max_sectors,
    by = c("adm0_pcode", "adm_name")
  ) %>%
  filter(!is.na(affected_population)) %>%
  mutate(
    pin_max = ifelse(sector == max_sector &
      inneed == 1, 1, 0),
    pin_max_2_sect_match = ifelse(
      max_sector %in% msna_sectors &
        sector == max_2_sector &
        inneed == 1,
      1,
      0
    ),
    pin_other_sect_match = ifelse(
      max_sector %in% msna_sectors &
        sector != max_sector &
        inneed == 1,
      1,
      0
    ),
    pin_max_2_sect_nomatch = ifelse(sector == max_2_sector &
      inneed == 1,
    1,
    0
    ),
    pin_other_sect_nomatch = ifelse(sector != max_sector &
      inneed == 1,
    1,
    0
    ),
    area_not_covered_msna = ifelse(is.na(uuid), 1, 0)
  ) %>%
  group_by(
    uuid,
    adm0_pcode,
    adm_name,
    max_sector,
    max_pin,
    max_2_sector,
    max_2_pin,
    max_2_std_dev,
    affected_population,
    weight
  ) %>%
  summarize(
    pin_max = sum(pin_max, na.rm = TRUE),
    pin_max_2_sect_match = sum(pin_max_2_sect_match, na.rm = TRUE),
    pin_other_sect_match = sum(pin_other_sect_match, na.rm = TRUE),
    pin_max_2_sect_nomatch = sum(pin_max_2_sect_nomatch, na.rm = TRUE),
    pin_other_sect_nomatch = sum(pin_other_sect_nomatch, na.rm = TRUE),
    area_not_covered_msna = sum(area_not_covered_msna),
    .groups = "drop"
  ) %>%
  mutate(
    pin_max_2_sect_match = ifelse(pin_max > 0,
      0,
      pin_max_2_sect_match
    ),
    pin_max_2_sect_nomatch = ifelse(pin_max > 0,
      0,
      pin_max_2_sect_nomatch
    ),
    pin_other_sect_match = case_when(
      pin_max > 0 ~ 0,
      pin_other_sect_match > 0 ~ 1,
      TRUE ~ 0
    ),
    pin_other_sect_nomatch = case_when(
      pin_max > 0 ~ 0,
      pin_other_sect_nomatch > 0 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(
    adm0_pcode,
    adm_name,
    max_sector,
    max_pin,
    max_2_sector,
    max_2_pin,
    max_2_std_dev,
    affected_population
  ) %>%
  summarize(
    perc_2nd_max_disjoint_match =
      weighted.mean(pin_max_2_sect_match, weight),
    perc_2nd_max_disjoint_nomatch =
      weighted.mean(pin_max_2_sect_nomatch, weight),
    perc_other_sectors_disjoint_match =
      weighted.mean(pin_other_sect_match, weight),
    perc_other_sectors_disjoint_nomatch =
      weighted.mean(pin_other_sect_nomatch, weight),
    area_not_covered_msna = sum(area_not_covered_msna),
    .groups = "drop"
  ) %>%
  mutate(
    perc_2nd_max_disjoint_match = replace_na(perc_2nd_max_disjoint_match, 0),
    perc_2nd_max_disjoint_nomatch = replace_na(
      perc_2nd_max_disjoint_nomatch,
      0
    ),
    perc_other_sectors_disjoint_match =
      replace_na(perc_other_sectors_disjoint_match, 0),
    perc_other_sectors_disjoint_nomatch =
      replace_na(perc_other_sectors_disjoint_nomatch, 0),
    pin_adj_max_2_match = max_pin +
      (affected_population * perc_2nd_max_disjoint_match),
    pin_max_2_match_exceed = ifelse(
      pin_adj_max_2_match > affected_population, 1, 0
    ),
    pin_adj_max_2_match = ifelse(
      pin_adj_max_2_match > affected_population,
      affected_population,
      pin_adj_max_2_match
    ),
    pin_adj_max_2_nomatch = max_pin +
      (affected_population * perc_2nd_max_disjoint_nomatch),
    pin_max_2_nomatch_exceed = ifelse(
      pin_adj_max_2_nomatch > affected_population, 1, 0
    ),
    pin_adj_max_2_nomatch = ifelse(
      pin_adj_max_2_nomatch > affected_population,
      affected_population,
      pin_adj_max_2_nomatch
    ),
    pin_adj_all_other_match = max_pin +
      (affected_population * perc_other_sectors_disjoint_match),
    pin_other_match_exceed = ifelse(
      pin_adj_all_other_match > affected_population, 1, 0
    ),
    pin_adj_all_other_match = ifelse(
      pin_adj_all_other_match > affected_population,
      affected_population,
      pin_adj_all_other_match
    ),
    pin_adj_all_other_nomatch = max_pin +
      (affected_population * perc_other_sectors_disjoint_nomatch),
    pin_other_nomatch_exceed = ifelse(
      pin_adj_all_other_nomatch > affected_population, 1, 0
    ),
    pin_adj_all_other_nomatch = ifelse(
      pin_adj_all_other_nomatch > affected_population,
      affected_population,
      pin_adj_all_other_nomatch
    )
  )

temp <- df_msna_anlyse %>%
  group_by(adm0_pcode) %>%
  summarise(
    max_pin = sum(round(max_pin)),
    pin_adj_max_2_match = sum(round(pin_adj_max_2_match)),
    pin_adj_all_other_match = sum(round(pin_adj_all_other_match)),
    affected_population = sum(round(affected_population)),
    .groups = "drop"
  )

write_csv(
  temp,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_max_pin_adj_by_msna.csv"
  )
)

temp %>%
  pivot_longer(
    cols = -adm0_pcode,
    names_to = "pin_type"
  ) %>%
  mutate(
    pin_type = case_when(
      pin_type == "affected_population" ~ "Targeted population",
      pin_type == "max_pin" ~ paste(
        "Max sectoral PiN",
        "(no adjustment)",
        sep = "\n"
      ),
      pin_type == "pin_adj_all_other_match" ~ paste(
        "Max sectoral PiN",
        "(adjusted by all other sectors)",
        sep = "\n"
      ),
      pin_type == "pin_adj_max_2_match" ~ paste(
        "Max sectoral PiN",
        "(adjusted by 2nd max sector)",
        sep = "\n"
      )
    ),
    value = round(value / 1000000, 2)
  ) %>%
  arrange(adm0_pcode, desc(value)) %>%
  ggplot(aes(
    y = value,
    x = reorder(pin_type, +value),
    label = paste0(value, "M"),
    fill = pin_type
  )) +
  facet_wrap(~adm0_pcode,
    strip.position = "bottom",
    scales = "free_x"
  ) +
  geom_col() +
  geom_text(
    position = position_identity(),
    hjust = -.2,
    size = 4
  ) +
  coord_flip(clip = "off") +
  labs(
    x = "",
    y = "PiN",
    title = paste0(
      "Impact of accounting for overlap on sectoral PiN"
    ),
    subtitle = paste0(
      "Only adjusting areas where the max sectoral PiN was ",
      "available in the MSNA"
    )
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = function(x) {
      paste0(x, "M")
    },
    expand = expansion(c(0, .2))
  ) +
  scale_fill_manual(
    values = c(rep("#1EBFB3", 3), "black")
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    panel.spacing = unit(1, "lines")
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_max_pin_adj_by_msna_match.png"
  ),
  height = 8,
  width = 12
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_max_pin_adj_by_msna.csv"
  )
)

# adjusted PiN not accounting for max secor avialability in the MSNA
temp <- df_msna_anlyse %>%
  group_by(adm0_pcode) %>%
  summarise(
    max_pin = sum(round(max_pin)),
    pin_adj_max_2_nomatch = sum(round(pin_adj_max_2_nomatch)),
    pin_adj_all_other_nomatch = sum(round(pin_adj_all_other_nomatch)),
    affected_population = sum(round(affected_population)),
    .groups = "drop"
  )

temp %>%
  pivot_longer(
    cols = -adm0_pcode,
    names_to = "pin_type"
  ) %>%
  mutate(
    pin_type = case_when(
      pin_type == "affected_population" ~ "Targeted population",
      pin_type == "max_pin" ~ paste(
        "Max sectoral PiN",
        "(no adjustment)",
        sep = "\n"
      ),
      pin_type == "pin_adj_all_other_nomatch" ~ paste(
        "Max sectoral PiN",
        "(adjusted by all other sectors)",
        sep = "\n"
      ),
      pin_type == "pin_adj_max_2_nomatch" ~ paste(
        "Max sectoral PiN",
        "(adjusted by 2nd max sector)",
        sep = "\n"
      )
    ),
    value = round(value / 1000000, 2)
  ) %>%
  arrange(adm0_pcode, desc(value)) %>%
  ggplot(aes(
    y = value,
    x = reorder(pin_type, +value),
    label = paste0(value, "M"),
    fill = pin_type
  )) +
  facet_wrap(~adm0_pcode,
    strip.position = "bottom",
    scales = "free_x"
  ) +
  geom_col() +
  geom_text(
    position = position_identity(),
    hjust = -.2,
    size = 4
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(rep("#1EBFB3", 3), "black")
  ) +
  labs(
    x = "",
    y = "PiN",
    title = "Impact of accounting for overlap on sectoral PiN",
    subtitle = paste0(
      "Adjusting the non-overlapping needs regardless of the ",
      "availability of the sector"
    )
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = function(x) {
      paste0(x, "M")
    },
    expand = expansion(c(0, .2))
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    legend.text = element_text(
      size = 12,
      family = "Roboto"
    ),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    panel.spacing = unit(1, "lines")
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_max_pin_adj_by_msna_nomatch.png"
  ),
  height = 8,
  width = 12
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_max_pin_adj_by_msna_nomatch.csv"
  )
)

# % of areas max PiN types don't match with MSNA
temp <- df_max_sectors %>%
  mutate(
    non_matching_sector = ifelse(max_sector %in% msna_sectors, 0, 1)
  ) %>%
  group_by(
    adm0_pcode
  ) %>%
  summarize(
    perc_non_matching = sum(non_matching_sector) / n()
  ) %>%
  left_join(
    df_msna_anlyse %>%
      group_by(
        adm0_pcode
      ) %>%
      summarize(
        perc_max_2_match_exceed = sum(pin_max_2_match_exceed) / n(),
        perc_max_2_nomatch_exceed = sum(pin_max_2_nomatch_exceed) / n(),
        perc_other_match_exceed = sum(pin_other_match_exceed) / n(),
        perc_other_nomatch_exceed = sum(pin_other_nomatch_exceed) / n(),
        perc_area_not_covered_msna = sum(area_not_covered_msna) / n()
      )
  )

temp %>%
  pivot_longer(
    cols = -adm0_pcode
  ) %>%
  mutate(
    name = case_when(
      name == "perc_non_matching" ~ "Sectors not matching with MSNA",
      name == "perc_max_2_match_exceed" ~ paste(
        "Adjusted PiN exceeded the affected population",
        "(2nd highest PiN, only if max sector in MSNA)",
        sep = "\n"
      ),
      name == "perc_max_2_nomatch_exceed" ~ paste(
        "Adjusted PiN exceeded the affected population",
        "(2nd highest PiN)",
        sep = "\n"
      ),
      name == "perc_other_match_exceed" ~ paste(
        "Adjusted PiN exceeded the affected population",
        "(all other PiNs, only if max sector in MSNA)",
        sep = "\n"
      ),
      name == "perc_other_nomatch_exceed" ~ paste(
        "Adjusted PiN exceeded the affected population",
        "(all other PiNs)",
        sep = "\n"
      ),
      name == "perc_area_not_covered_msna" ~ "MSNA data unavailable"
    ),
    value = round(value * 100)
  ) %>% # nolint
  arrange(adm0_pcode, desc(value)) %>%
  ggplot(
    aes(
      y = value,
      x = reorder(name, +value),
      label = paste0(value, "%")
    )
  ) +
  scale_fill_distiller(type = "seq", direction = 1) +
  geom_bar(
    stat = "identity",
    fill = "#1EBFB3"
  ) +
  facet_grid(~adm0_pcode) +
  coord_flip() +
  theme_minimal() +
  geom_text(
    position = position_identity(),
    hjust = -.1,
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    labels = function(x) {
      paste0(x, "%")
    }
  ) +
  labs(
    y = "% of areas affected",
    x = "",
    title = "Dynamics of sectoral PiN adjustment"
  ) +
  scale_x_discrete(
    labels = function(x) {
      str_wrap(x, width = 35)
    }
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      hjust = 0.5,
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    panel.border = element_rect(
      size = 0.8,
      fill = "transparent",
      color = "gray"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      margin = margin(20, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_msna_misalignments.png"
  ),
  height = 8,
  width = 13
)

##############################
#### OVERLAP AND OUTLIERS ####
##############################

temp <- df_msna_anlyse %>%
  mutate(
    max_2_pct = max_2_pin / max_pin
  ) %>%
  select(
    adm0_pcode:max_2_pin,
    max_2_pct,
    perc_2nd_max_disjoint_nomatch
  )

temp %>%
  ggplot(
    aes(
      x = max_2_pct,
      y = perc_2nd_max_disjoint_nomatch
    )
  ) +
  geom_point(
    alpha = 0.3,
    fill = "#1EBFB3"
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "2nd max PiN, % of max PiN",
    y = "% adjustment (2nd max overlap)",
    title = "Relationship between 2nd max PiN and overlap adjustment",
    subtitle = paste0(
      "Adjusting the non-overlapping needs regardless of the ",
      "availability of the sector"
    )
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      margin = margin(10, 10, 10, 10, "pt"),
      hjust = 0.5,
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      margin = margin(20, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  )


ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_overlap_and_2nd_max.png"
  ),
  height = 6,
  width = 9
)

write_csv(
  temp,
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "datasets",
    "2022_overlap_and_2nd_max.csv"
  )
)

#####################################
#### LOOK AT STD DEV AND OVERLAP ####
#####################################

df_msna_anlyse %>%
  ggplot(
    aes(
      x = max_2_std_dev,
      y = perc_2nd_max_disjoint_nomatch
    )
  ) +
  geom_rect(
    xmin = 0.7,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    fill = "#CCE5F9"
  ) +
  geom_rect(
    xmin = 1,
    xmax = 1.4,
    ymin = 0,
    ymax = 1,
    fill = "#FCE0DE"
  ) +
  geom_point(
    alpha = 0.3,
    fill = "#1EBFB3"
  ) +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::percent_format(1)
  ) +
  scale_x_continuous(
    labels = scales::percent_format(1)
  ) +
  labs(
    x = "Max PiN, % of mean + 2 std. dev. of PiNs",
    y = "% adjustment (2nd max overlap)",
    title = "Relationship between outliers and overlap adjustment",
    subtitle = paste0(
      "Adjusting the non-overlapping needs regardless of the ",
      "availability of the sector"
    )
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      hjust = 0.5,
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text = element_text(
      face = "bold",
      size = 10,
      family = "Roboto"
    ),
    axis.title.x = element_text(
      face = "bold",
      size = 12,
      margin = margin(20, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    )
  ) +
  geom_text(
    data = data.frame(
      x = c(0.71, 1.02),
      y = 0.7,
      label = c("Non-outliers", "Outliers")
    ),
    mapping = aes(
      x = x,
      y = y,
      label = label
    ),
    hjust = 0,
    fontface = "bold",
    family = "Roboto"
  )

ggsave(
  file.path(
    file_paths$output_dir,
    "graphs",
    "Option 1",
    "2022_overlap_and_std_dev.png"
  ),
  height = 5,
  width = 9
)
