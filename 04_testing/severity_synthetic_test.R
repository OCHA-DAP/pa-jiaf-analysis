library(tidyverse)

source(here::here("99_helpers", "helpers.R"))

file_paths <- get_paths_analysis()

#####################
#### CREATE DATA ####
#####################

gen_row <- function(n) {
  x <- sample(
    x = 1:5,
    size = n,
    replace = TRUE
  )

  data.frame(
    n = n,
    severity_mean = mean(x),
    severity_overlap = 5 * sum(x >= 3) / n
  )
}

# generating 10000 rows for each # of sectors
# from 5 to 10

df <- map_dfr(
  5:10,
  function(x) {
    map_dfr(
      1:10000,
      function(y) {
        gen_row(x)
      }
    )
  }
)

# take convex hull of the data and then plot the polygons on top of each other
# to see if there is a wider distribution the more sectors there are
df %>%
  nest(
    data = -n
  ) %>%
  mutate(
    data = lapply(
      data,
      function(df) {
        slice(
          df,
          chull(severity_mean, severity_overlap)
        )
      }
    )
  ) %>%
  unnest(
    cols = data
  ) %>%
  filter(
    n %in% c(5, 10)
  ) %>%
  mutate(
    n = factor(n, levels = c(5, 10))
  ) %>%
  ggplot(
    aes(
      x = severity_mean,
      y = severity_overlap
    )
  ) +
  geom_polygon(
    aes(
      group = n,
      fill = n
    ),
    alpha = 1
  ) +
  scale_fill_manual(
    values = c("#1EBFB3", "#78D9D1")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0,
      size = 22,
      margin = margin(10, 10, 10, 10, "pt"),
      family = "Roboto"
    ),
    plot.background = element_rect(
      fill = "white"
    ),
    axis.text.x = element_text(
      face = "bold",
      margin = margin(20, 0, 20, 0, "pt"),
      size = 10,
      family = "Roboto"
    ),
    axis.text.y = element_text(
      face = "bold",
      margin = margin(0, 20, 0, 20, "pt"),
      size = 10,
      family = "Roboto"
    ),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      size = 16,
      family = "Roboto"
    ),
    legend.position = "none"
  ) +
  labs(
    x = "Mean of sectoral severity",
    y = "Overlap (% of sectors with severity 3+, normalized to 5)",
    title = "Possible values of severity overlap and mean, by # of sectors"
  ) +
  geom_text(
    data = data.frame(
      severity_mean = 1.85,
      severity_overlap = c(0.2, 0.65),
      label = c("5 sectors", "10 sectors")
    ),
    aes(
      label = label
    ),
    family = "Roboto",
    color = "white",
    fontface = "bold"
  )


ggsave(
  file.path(
    file_paths$output_dir_sev,
    "graphs",
    "2022_overlap_mean_num_sectors.png"
  ),
  height = 6,
  width = 10,
  units = "in"
)
