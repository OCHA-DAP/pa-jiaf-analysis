#' Generate directories and paths for PiN analysis
#'
#' @param country String with country name
#'
#' @return Named list of:
#'     * OCHA data directory
#'     * Cluster data directory
#'     * Save file path
get_paths <- function(country) {
  # iso3 for use in save path
  iso3 <- tolower( # nolint
    countrycode::countryname(
      country,
      destination = "iso3c"
    )
  )
  data_dir <- Sys.getenv("JIAF_DATA_DIR")

  # directory with ocha provided data
  ocha_dir <- file.path(
    data_dir,
    "Data from country offices - OCHA",
    country
  )

  # directory with cluster provided data
  cluster_dir <- file.path(
    data_dir,
    "Data from country offices - Clusters",
    country
  )

  # file path to save pin data
  save_path <- file.path(
    data_dir,
    "Data aggregated for analysis",
    glue::glue("{iso3}_pins_2022.csv")
  )

  list(
    ocha_dir = ocha_dir,
    cluster_dir = cluster_dir,
    save_path = save_path
  )
}
