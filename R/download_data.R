#' Download Silternetz raw data
#'
#' Provides a connection to the INOPLA-API and allows to download the data from
#' specified periods.
#'
#' @param endpoint Specifies which data to download. Has to be one of
#'  one of Dest, Dest_Count, Numbers, Numbers_Count, Callerlists.
#' @param start_date First date of the period of interest. Format: YYYY-MM-DD.
#' @param end_date Last date of the period of interest. Format: YYYY-MM-DD.
#' @param verbose Boolean if verbose mode is turned on
#' @param create_firstcall_column whether a column should be created that
#' shows that a call was a first call by a caller. When the function is
#' called from within the Silbernetz app, this should be `FALSE` because the app
#' does its own handling of the first caller column (saving time).
#'
#' @return Downloaded dataframe.
#'
#' @details
#' The information in ouuput column "firstcall" refers to whether a call has been the first
#' one of the respective caller among those in the time frame that is being downloaded.
#'
#' @examples
#' \dontrun{
#' download_data("Dest", start_date = "2021-05-01", end_date = "2021-05-31")}
#'
#' @export
download_data <- function(
  endpoint,
  start_date = NULL,
  end_date = NULL,
  verbose = TRUE,
  create_firstcall_column = TRUE
) {
  if (
    !endpoint %in%
      c("Dest", "Dest_Count", "Numbers", "Numbers_Count", "Callerlists")
  ) {
    warning(
      "Endpoint has to be one of Dest, Dest_Count, Numbers, Numbers_Count, Callerlists"
    )
    return(NULL)
  }

  # Using source in a package is disencouraged because it changes the current environment
  # However, I don't think its a problem here
  # Is there a better solution without having to write more code?
  source(here::here("R", "api_spec.R"))
  url <- api_urls[[endpoint]]

  # Specify parameters if they are provided
  if (!is.null(start_date) && !is.null(end_date)) {
    # Make sure that the date format is correct
    start_date <- lubridate::ymd(start_date)
    end_date <- lubridate::ymd(end_date)

    param = paste('?start_date=', start_date, sep = '')
    param = paste(param, "&end_date=", sep = '')
    param = paste(param, end_date, sep = '')

    url <- paste(url, param, sep = '')
  }
  if (xor(is.null(start_date), is.null(end_date))) {
    warning("Either both start_date and end_date must be specified or none")
    return(NULL)
  }

  # Make GET request and extract the data
  if (verbose) {
    print("Making API request...")
  }
  r <- httr::GET(url)
  if (verbose) {
    print("Parsing response...")
  }
  js <- jsonlite::fromJSON(httr::content(r, as = 'text'))
  if (verbose) {
    print("Download finished!")
  }

  # If we ask for "XY_Count", we only get one number
  if (endpoint %in% c("Dest_Count", "Numbers_Count")) {
    return(js$response)
  }

  dat <- dplyr::as_tibble(js$response$data) %>%
    #mutate(date = lubridate::ymd(date)) %>%
    dplyr::select(any_of(
      c(
        "date",
        "time",
        "service",
        "service_caption",
        "caller",
        "tarif",
        "modul_name",
        "duration_inbound",
        "duration_outbound",
        "success"
      )
    ))

  if (isTRUE(create_firstcall_column)) {
    dat <- dat %>%
      group_by(caller) %>%
      mutate(
        datetime = lubridate::ymd_hms(paste(date, time)),
        firstcall = datetime == min(datetime)
      ) %>%
      ungroup %>%
      select(-datetime)
  }
  return(dat)
}
