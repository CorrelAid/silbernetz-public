#' Removes columns that are provided by the API but not needed by us
#'
#' @param data data set with data
#' @param red_cols Columns to remove, defaults to "service", "service_caption",
#'  "tarif" and "modul_name".
#'
#' @return Data with columns removed
#' @export

remove_redundant_cols <- function(data, red_cols = c("service", "service_caption", "tarif", "modul_name")){
  return(dplyr::select(data, -dplyr::one_of(red_cols)))
}
