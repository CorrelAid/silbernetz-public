#' split long data by year and write each year to disk
#' @param calls_df long data frame containing all the calls
#' @param path data folder path. defaults to `data/raw/annual`
#' @return
split_and_write <- function(calls_df, path = "data/raw/annual") {
  nested <- calls_df |>
    mutate(year = lubridate::year(date)) |>
    tidyr::nest(.by = year)

  pwalk(nested, function(year, data) {
    print(sprintf("Writing csv for %i", year))
    readr::write_csv(
      data |> dplyr::arrange(date),
      fs::path(path, year, ext = "csv")
    )
  })
}

#' reads separate annual csv's and binds them together
#' @param path data folder path. defaults to `data/raw/annual`
#' @return tibble data frame with all calls since 2020.
read_annual_data <- function(path = "data/raw/annual") {
  fs::dir_ls(path) |>
    purrr::map_dfr(readr::read_csv, col_types = "Dtciillclccc")
}

read_current_data <- function(
  file = "data/raw/current_data.csv",
  col_types = "Dtciillclccc"
) {
  readr::read_csv(file, col_types = col_types)
}
