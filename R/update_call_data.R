#' gets new calls from API, updates annual csvs
#' @param current_data tibble containing calls (from "Numbers" API endpoint).
#' @param okz geo / Ortskennzahlen (okz) data mastertable
#' @details
#' takes newest date in current data and gets all calls from the API that have happened since that date.
#' writes out annual csvs for affected years to disk.
#'
update_call_data <- function(current_data, okz) {
  start_date <- max(current_data$date)
  # remove from current_data so that we don't create duplicate rows because we had
  # downloaded "half a day" last time
  current_data <- current_data |> filter(date != start_date)

  new_data <- download_numbers(
    start_date = start_date,
    end_date = Sys.Date() # today
  )
  print("Adding geodata and hashing.")
  new_data <- new_data |>
    # function that wraps several functions to ensure things are done in the correct order
    # -> see modify_columns.R
    clean_numbers()
  # create updated data from old data + new rows
  updated_data <- dplyr::bind_rows(current_data, new_data) |>
    add_first_call_column()

  return(updated_data)
}
