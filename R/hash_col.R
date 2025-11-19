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
