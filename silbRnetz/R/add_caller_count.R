#' Adds a column to the dataframe indicating for each phone number
#' how many times it has already called Silbernetz.
#' In order to do so, it downloads a caller list from the API and
#' merges it with the provided dataframe
#'
#' @param numbers Dataframe or tibble with a column called "callers"
#' that contains the phone numbers
#'
#' @return Returns the provided dataframe/tibble with an additional
#' column that describes the number of times each phone number has
#' already called Silbernetz
#'
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' add_caller_count(download_data("Numbers",start_date = "2021-05-01",
#' end_date = "2021-05-03"))}
#'
#' @export

add_caller_count <- function(numbers){

  # Some numbers in the caller list are anonymised (with an XXX ending)
  # others are not. Therefore, we anonymise all of them and are willing
  # to tolerate a small error here if two numbers get confused
  # Anonymise phone numbers
  numbers[['callerXXX']] <- paste(substr(numbers[['caller']], 0, nchar(numbers[['caller']])-3),
                               "XXX",
                               sep = "")

  # Download most recent caller list
  # Specified dates should make no difference, but to make sure:
  caller <- download_data("Callerlists",
                          start_date = min(numbers$date),
                          end_date   = max(numbers$date))

  caller[['callerXXX']] <- paste(substr(caller[['caller']],
                                     0,
                                     nchar(caller[['caller']])-3),
                              "XXX",
                              sep = "")

  # As described earlier, we summarise numbers that have the same ending
  caller <- caller %>%
    dplyr::select(callerXXX, count_try) %>%
    dplyr::group_by(callerXXX) %>%
    dplyr::summarise(count_try = min(count_try))


  # Merge the new column and delete callerXXX
  res <- dplyr::left_join(numbers, caller, by = 'callerXXX')
  res[['callerXXX']] <- NULL
  return(res)

}


