#' Check if an endpoint is a count endpoint
#' @param endpoint A character string representing an endpoint. as passed to [download_data]
#' @return `TRUE` if the endpoint ends with "Count", otherwise `FALSE`.
#' @examples
#' is_count_endpoint("Numbers_Count")   # TRUE
#' is_count_endpoint("Numbers")    # FALSE
is_count_endpoint <- function(endpoint) {
  return(stringr::str_detect(endpoint, "Count$"))
}


#' Build query parameters for start and end dates
#' Creates a query string with `start_date` and `end_date` parameters.
#' Both dates must be provided together; otherwise `NULL` is returned
#' with a warning.
#' @param start_date Start date (character or Date).
#' @param end_date End date (character or Date).
#' @return A query string (e.g., `"?start_date=2024-01-01&end_date=2024-01-31"`)
#'   or `NULL` if only one date is supplied.
#' @examples
#' build_query_params("2024-01-01", "2024-01-31")
build_query_params <- function(start_date, end_date) {
  # return empty if wrong parameter specification
  if (xor(is.null(start_date), is.null(end_date))) {
    warning("Either both start_date and end_date must be specified or none")
    return(NULL)
  }

  params <- ""
  if (!is.null(start_date) && !is.null(end_date)) {
    # both are provided -> build param string
    params <- sprintf(
      "?start_date=%s&end_date=%s",
      lubridate::ymd(start_date),
      lubridate::ymd(end_date)
    )
  }
  return(params)
}

#' Download Silternetz raw data
#'
#' Provides a connection to the hotline provider API and allows to download the data from
#' specified periods.
#'
#' @param endpoint Specifies which data to download. Has to be one of
#'  one of Dest, Dest_Count, Numbers, Numbers_Count, Callerlists.
#' @param start_date First date of the period of interest. Format: YYYY-MM-DD.
#' @param end_date Last date of the period of interest. Format: YYYY-MM-DD.
#' @return A tibble of response data, a single count value (for `*_Count`
#'   endpoints), or `NULL` if the endpoint is invalid.
#'
#' @examples
#' \dontrun{
#' download_data("Dest", start_date = "2021-05-01", end_date = "2021-05-31")}
#'
download_data <- function(
  endpoint,
  start_date = NULL,
  end_date = NULL
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

  # Make GET request and extract the data
  params <- build_query_params(start_date, end_date)
  res <- api_get(endpoint = endpoint, params = params)

  if (is_count_endpoint(endpoint)) {
    # count only return one number
    return(res$response)
  }
  dat <- dplyr::as_tibble(res$response$data)
  return(dat)
}

download_numbers <- function(start_date, end_date) {
  # call count API to see whether we exceed 100k rows
  count <- download_data("Numbers_Count", start_date, end_date)
  print(sprintf("Downloading Numbers between %s and %s", start_date, end_date))

  if (count > 100000) {
    print("more than 100k rows. we have to do multiple calls.")
    num_calls <- ceiling(count / 70000) # let's do smaller chunks, don't risk anything
    # find the start and end dates of each chunk
    day_seq <- seq(as.Date(start_date), as.Date(end_date), "days")
    num_days <- length(day_seq) # how many days in total
    each_range <- num_days / num_calls # how many days each call?
    end_dates_i <- floor(each_range * 1:num_calls)
    start_dates_i <- c(1, end_dates_i[1:(length(end_dates_i) - 1)] + 1) # + 1 so we don't do some days twice

    dat <- purrr::map2_dfr(
      start_dates_i,
      end_dates_i,
      function(start_i, end_i) {
        start_date <- day_seq[start_i]
        end_date <- day_seq[end_i]
        return(download_numbers(start_date, end_date)) # recursive shenanigans
      }
    )
  } else {
    dat <- download_data("Numbers", start_date, end_date)
  }

  # only keep specific columns
  dat |>
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
    )) |>
    # type conversions for date and time
    dplyr::mutate(
      date = lubridate::ymd(.data$date),
      time = hms::as_hms(.data$time)
  )
}

#' Perform a GET request to an API endpoint
#'
#' Builds and sends a GET request using an endpoint-specific path from
#' environment variables, then parses the JSON response.
#'
#' @param endpoint Character string naming the endpoint.
#' @param params Query string parameters (e.g., `"?id=1"`).
#' @param base_url Base API URL. Defaults to `API_BASE_URL` environment variable.
#' @param verbose Logical; print progress messages.
#'
#' @return A parsed JSON object.
#' @throws Error if `base_url` or the required endpoint path env var is missing,
#'   or if the API request fails.
#'
#' @examples
#' # api_get("users", "?limit=10")
api_get <- function(
  endpoint,
  params,
  base_url = Sys.getenv("API_BASE_URL"),
  verbose = TRUE
) {
  if (base_url == "") {
    stop(
      "Need a valid base_url to call. Specify directly or as a environment variable API_URL using a .env file."
    )
  }
  # get api path from environment
  path <- get_api_path_from_env(endpoint)

  # Make GET request and extract the data
  if (verbose) {
    print("Making API request...")
  }

  # make GET request
  r <- httr::GET(sprintf("%s/%s%s", base_url, path, params))
  httr::stop_for_status(r)

  if (verbose) {
    print("Parsing response...")
  }
  js <- jsonlite::fromJSON(httr::content(r, as = 'text'))
  if (verbose) {
    print("Download finished!")
  }
  return(js)
}

#' Get API path for endpoint from environment
#' Retrieves an API path based on an endpoint name by reading the
#' corresponding environment variable (`API_PATH_<ENDPOINT>`).
#'
#' @param endpoint A character string naming the endpoint. as passed to [download_data].
#' @return The API path as a character string.
#' @throws Error if the required environment variable is not set.
#'
#' @examples
get_api_path_from_env <- function(endpoint) {
  env_var <- sprintf("API_PATH_%s", toupper(endpoint))
  value <- Sys.getenv(env_var)
  if (value == "") {
    stop(sprintf(
      "Environment variable %s is not available. Please provide it, e.g. using an .env file. See README.md.",
      env_var
    ))
  }
  return(value)
}
