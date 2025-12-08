#' add firstcall column
#'
#' adds the boolean firstcall column to the data set that indicates whether a given call was the first call the person did.
#' @param data dataset. this should be the complete dataset since 2020, so that we can correctly identify first calls
#' @return data.frame the data set with the firstcall column added or updated.
#' @details
#' The information in ouput column "firstcall" refers to whether a call has been the first
#' one of the respective caller.
add_first_call_column <- function(data) {
  data <- data |>
    group_by(caller) |>
    arrange(date, time, .by_group = TRUE) |>
    mutate(firstcall = row_number() == 1) |>
    ungroup()
}


add_is_landline_column <- function(data) {
  data |>
    dplyr::mutate(
      is_landline = stringr::str_detect(
        caller,
        "^00491|^491|^0049800|^49800", # in case we find more that are not landline, add here
        negate = TRUE
      )
    )
}


#' Removes columns that are provided by the API but not needed by us
#'
#' @param data data set with data
#' @param red_cols Columns to remove, defaults to "service", "service_caption",
#'  "tarif".
#'
#' @return Data with columns removed
#' @export

remove_redundant_cols <- function(
  data,
  red_cols = c("service", "service_caption", "tarif")
) {
  return(dplyr::select(data, -dplyr::one_of(red_cols)))
}


#' Replaces a column by its hash-value to increase privacy
#'
#' @param d dataframe with data
#' @param column column that is supposed to be replaced, defaults to caller
#'
#' @import openssl
#' @return Dataframe with the specified column replaced
#' @export
#'
hash_col <- function(data, column = 'caller') {
  data[[column]] <- openssl::sha256(data[[column]]) |> as.character()
  return(data)
}

#' removes duplicate calls
#' @param numbers_raw
#' @return tibble of calls with unique calls and with the following columns removed: id, modul_name, first_call
#' @details
#' Due to how the hotline works, some calls are rerouted through a "Routingpunkt". This results in two entries for the same call, with the same date, time, caller
#' but a different id and potentially modul_name. also the firstcall column that is added by us will have different values. in order for `distinct` to work its magic, we remove those columns here.
remove_duplicate_calls <- function(numbers_raw) {
  numbers_raw |>
    dplyr::select(-id, -modul_name, firstcall) |>
    dplyr::distinct()
}

#' clean_numbers
#' @param numbers_raw raw numbers data frame as returned by download_numbers
#' @param okz Ortskennzahlen table
#' @return tibble with duplicates removed, added columns is_landline, added Vorwahl & Bundesland, removed redundant columns and number hashed
#' @details does several data cleaning and manipulation steps in the correct order.
#' - type conversions date & time
#' - adds is_landline column
#' - adds Vorwahl/Ortskennzahl and the Bundesland column (`add_geodata_to_numbers`)
#' - remove unused columns
#' - hashes the number of the caller
clean_numbers <- function(numbers_raw, okz) {
  numbers_raw |>
    # type conversions for date, id, time
    dplyr::mutate(
      date = lubridate::ymd(date),
      time = hms::as_hms(time),
      id = as.character(id)
    ) |>
    add_is_landline_column() |>
    remove_redundant_cols() |>
    add_geodata_to_numbers(okz = okz) |>
    hash_col()
}
