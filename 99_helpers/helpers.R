# Module constants
aggregated_data_dir <- "Data aggregated for analysis"


#' Generate directories and paths for PiN analysis
#'
#' @param country String with country name
#' @param country_name If `country` is not an easily interpretable
#'     country name, and thus retrieval of an ISO3 code impossible,
#'     pass `country_name` instead.
#'
#' @return Named list of:
#'     * OCHA data directory
#'     * Cluster data directory
#'     * Save file path
get_paths <- function(country, country_name = NULL) {
  if (is.null(country_name)) {
    country_name <- country
  }

  # iso3 for use in save path
  iso3 <- tolower( # nolint
    countrycode::countryname(
      country_name,
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

  fn <- glue::glue("{iso3}_pins_2022.csv")

  # file path to save sectoral pin data
  save_path <- file.path(
    data_dir,
    aggregated_data_dir,
    "sectoral_pins",
    fn
  )

  # file path to save indicator pin data
  save_path_indicator <- file.path(
    data_dir,
    aggregated_data_dir,
    "indicator_pins",
    fn
  )

  # file path to save sectoral sev data
  save_path_sev <- file.path(
    data_dir,
    aggregated_data_dir,
    "sectoral_sev",
    fn
  )

  # file path to save indicator severity data
  save_path_indicator_sev <- file.path(
    data_dir,
    aggregated_data_dir,
    "indicator_sev",
    fn
  )

  # file path to save hh data
  save_path_hh_data <- file.path(
    data_dir,
    aggregated_data_dir,
    "hh_data",
    fn
  )

  list(
    ocha_dir = ocha_dir,
    cluster_dir = cluster_dir,
    save_path = save_path,
    save_path_sev = save_path_sev,
    save_path_hh_data = save_path_hh_data,
    save_path_indicator = save_path_indicator,
    save_path_indicator_sev = save_path_indicator_sev
  )
}

get_paths_analysis <- function() {
  data_dir <- Sys.getenv("JIAF_DATA_DIR")

  agg_dir <- file.path(
    data_dir,
    aggregated_data_dir
  )

  output_dir <- file.path(
    data_dir,
    "Data analyzed"
  )

  list(
    agg_dir = agg_dir,
    input_dir = file.path(agg_dir, "sectoral_pins"),
    input_hh_dir = file.path(agg_dir, "hh_data"),
    input_indicator_dir = file.path(agg_dir, "indicator_pins"),
    input_sev_sector_dir = file.path(agg_dir, "sectoral_sev"),
    input_sev_indicator_dir = file.path(agg_dir, "indicator_sev"),
    output_dir = output_dir
  )
}
