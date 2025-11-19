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
