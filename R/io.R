#' split long data by year and write each year to disk
#' @param calls_df long data frame containing all the calls
#' @param path data folder path. defaults to `data/raw/annual`
#' @param years numeric vector of years to write out. defaults to all years between 2020 and the current year.
#' @return
split_and_write <- function(
  calls_df,
  path = "data/raw/annual",
  years = seq(2020, lubridate::year(Sys.Date()))
) {
  nested <- calls_df |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::filter(year %in% years) |>
    tidyr::nest(.by = year)

  purrr::pwalk(nested, function(year, data) {
    print(sprintf("Writing csv for %i", year))
    readr::write_csv(
      data |> dplyr::ungroup() |> dplyr::arrange(date, time, caller),
      fs::path(path, year, ext = "csv")
    )
  })
}

#' reads separate annual csv's and binds them together
#' @param path data folder path. defaults to `data/raw/annual`
#' @return tibble data frame with all calls since 2020.
read_annual_data <- function(
  path = Sys.getenv("DATA_FOLDER")
) {
  if (path == "") {
    stop(
      "environment variable DATA_FOLDER is not set. See README on how to set up the .env file."
    )
  }

  col_types_l <- readr::cols(
    id = readr::col_character(),
    date = readr::col_date(),
    time = readr::col_time(),
    caller = readr::col_character(),
    duration_inbound = readr::col_integer(),
    duration_outbound = readr::col_integer(),
    success = readr::col_logical(),
    Ortsnetzkennzahl = readr::col_character(),
    Bundesland = readr::col_character(),
    PLZ = readr::col_character(),
    firstcall = readr::col_logical(),
    is_landline = readr::col_logical(),
    modul_name = readr::col_character()
  )
  df <- fs::dir_ls(path, glob = "*.csv") |>
    purrr::map_dfr(readr::read_csv, col_types = col_types_l)
}

read_current_data <- function(
  file = "data/raw/current_data.csv",
  col_types = "Dtciillccccl"
) {
  readr::read_csv(file, col_types = col_types)
}
