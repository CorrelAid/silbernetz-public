#' gets new calls from API, updates annual csvs
#' @param current_data tibble containing calls (from "Numbers" API endpoint).
#' @details
#' takes newest date in current data and gets all calls from the API that have happened since that date.
#' writes out annual csvs for affected years to disk.
#'
update_call_data <- function(current_data) {
  start_date <- max(current_data$date)
  # remove from current_data so that we don't create duplicate rows because we had
  # downloaded "half a day" last time
  current_data <- current_data |> filter(date != start_date)

  new_data <- download_numbers(
    start_date = start_date,
    end_date = Sys.Date() # today
  )
  print("Adding geodata and hashing. This takes a while.")
  new_data |>
    new_data |>
    add_geodata_to_numbers(geo = geo) %>%
    remove_redundant_cols() %>%
    hash_col()

  # which years are among the new data? affects which datasets we write out.
  affected_years <- unique(lubridate::year(new_data$date))
  # create updated data from old data + new rows
  updated_data <- dplyr::bind_rows(current_data, new_data) |>
    add_first_call_column()

  # write updated data - only years that were part of new data
  # most of the time this is only one year but around new year this might be different
  #  so let's be careful
  affected_data <- updated_data |>
    mutate(year = lubridate::year(date)) |>
    filter(year %in% affected_years)
  split_and_write(affected_data)

  return(updated_data)
}
