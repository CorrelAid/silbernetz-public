#' Add geographic info to phone numbers
#'
#' Adds columns with information on "Ortsnetzname", "Bundesland" and "PLZ" to
#' a downloaded dataframe of type "Numbers", as well as a column that
#' indicates whether a call came from landline.
#'
#' @param numbers A dataframe of endpoint type "Numbers",
#' as can be returned by [download_data()].
#' @param geo_cols A character vector that specifies which
#' geographic information should be added as columns (a subset of "Ortsnetzname",
#' "Bundesland", "PLZ").
#'
#' @return Returns the input dataframe with new columns added to the right.
#'
#' @details
#' Note that phone codes ("Ortsnetzkennzahl") can overlap
#' with multiple post codes ("PLZ") and vice versa. Therefore, post codes
#' are encoded as character vectors.
#'
#' Geographic data has been compiled from multiple sources,
#' all of which have been accessed between July and August 2021:
#' - Bundesnetzagentur
#' - http://www.suche-postleitzahl.org/downloads
#' - http://www.xn--datenbrse-57a.net/item/Postleitzahlen-Datenbank_Deutschland
#' - http://wogibtes.info
#'
#' @import dplyr tidyr readr fuzzyjoin stringr
#'
#' @examples
#' \dontrun{
#' add_geodata_to_numbers(download_data("Numbers",
#' start_date = "2021-05-31", end_date = "2021-05-31"))
#' }
#'
#' @export
add_geodata_to_numbers <- function(
  numbers,
  geo,
  geo_cols = c("Ortsnetzname", "Bundesland", "PLZ")
) {
  # select relevant cols from geodata master table
  geo %>%
    select(Ortsnetzkennzahl, any_of(geo_cols))

  # change column type for data and add landline column
  numbers <- numbers %>%
    mutate(
      date = lubridate::ymd(date),
      landline = str_detect(caller, "00491", negate = TRUE)
    )

  # merge
  numbers_geo <- numbers %>%
    fuzzyjoin::fuzzy_left_join(
      .,
      geo,
      by = c("caller" = "Ortsnetzkennzahl"),
      match_fun = str_detect
    )
  return(numbers_geo)
}
