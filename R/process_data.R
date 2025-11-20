#' Add OKZ and geographic info to phone numbers
#'
#' Adds columns with information on "Ortsnetzname", "Bundesland" and "PLZ" to
#' a downloaded dataframe of type "Numbers", as well as a column that
#' indicates whether a call came from landline.
#'
#' @param numbers A dataframe of endpoint type "Numbers",
#' as can be returned by [download_numbers()]. Need to have `is_landline` column added beforehand.
#' @param okz a dataframe containing the information on Ortsnetzkennzahlen.
#' @param okz_cols A character vector that specifies which
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
#' @import dplyr tidyr readr stringr
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
  okz,
  okz_cols = c("Ortsnetzname", "Bundesland", "PLZ")
) {
  # select relevant cols from geodata master table
  okz <- okz |>
    dplyr::select(Ortsnetzkennzahl, any_of(okz_cols))

  # only for landline we have to find the matching OKZ
  # we bind_rows again later.
  landline_df <- numbers |>
    dplyr::filter(is_landline)

  # vorwahlen (Ortsnetzkennzahlen) have differing lengths between 3 and 6
  # 030, 0228 etc.
  # we use this knowledge to our advantage to avoid fuzzy joins
  vorw_lengths <- 3:6 + 3 # +3 because 0049 instead of leading 0 in the data
  # we create a expanded dataset that has "caller" truncated to different lengths
  # tidyr::crossing removes duplicates...
  nmb_ll_candidates <- tidyr::expand_grid(
    caller = landline_df$caller,
    vorw_lengths
  ) |>
    dplyr::mutate(Ortsnetzkennzahl = stringr::str_sub(caller, 1, vorw_lengths)) # + 3 adjust for 0049
  # join
  nmb_ll_joined <- nmb_ll_candidates |>
    dplyr::left_join(okz, by = c("Ortsnetzkennzahl"))

  # check. if it worked for all, the join resulted in :
  # NA, NA, NA, match for each landline number. so if we
  # use distinct on a column from okz, we should have two rows
  # for each.
  check <- nmb_ll_joined |>
    dplyr::distinct(caller, Ortsnetzname) |>
    dplyr::ungroup() |>
    dplyr::count(caller, name = "matches")

  if (!all(check$matches == 2)) {
    print(table(check$matches)) # 2 is expected, 1 means we probably only found NA, more than 2 is weird.
    not_matched <- check |> dplyr::filter(matches == 1) |> dplyr::pull(caller)
    too_many <- check |> dplyr::filter(matches > 2) |> dplyr::pull(caller)
    stop(
      sprintf(
        "There were problems when matching landline numbers to reference data. \n Numbers not matched: %s\n Numbers with too many matches: %s",
        paste(not_matched, collapse = ", "),
        paste(too_many, collapse = ", ")
      )
    )
  }

  # prepare for join
  nmb_ll_joined <- nmb_ll_joined |>
    dplyr::filter(!is.na(Ortsnetzname)) |>
    dplyr::distinct() |> # only one row per caller
    dplyr::select(-vorw_lengths) # not used anymore

  numbers_okz <- landline_df |>
    # expand_grid removed the duplicates, so join with original landline_df here
    dplyr::left_join(nmb_ll_joined, by = "caller") |>
    dplyr::bind_rows(numbers |> dplyr::filter(!is_landline)) # add calls from mobile

  return(numbers_okz |> dplyr::arrange(date, time, caller))
}
