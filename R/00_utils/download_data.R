#' Provides a connection to the INOPLA-API and allows to download the data from 
#' specified periods.
#' REQUIREMENT: The file "api_spec.R" has to be downloaded separately and placed 
#' into "data/raw"
#'
#' @param endpoint Specifies which data to download Has to be one of
#'  one of Dest, Dest_Count, Numbers, Numbers_Count, Callerlists.
#' @param start_date First date of the period of interest. Format: YYYY-MM-DD.
#' @param end_date Last date of the period of interest. Format: YYYY-MM-DD.
#'
#' @return Downloaded dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' download_data("Dest", start_date = "2021-05-01", end_date = "2021-05-31")
#' }


download_data <- function(endpoint, start_date = NULL, end_date = NULL){
  
  if(!endpoint %in% c("Dest", "Dest_Count", "Numbers", "Numbers_Count", "Callerlists")){
    warning("Endpoint has to be one of Dest, Dest_Count, Numbers, Numbers_Count, Callerlists")
    return(NULL)
  }
  
  # Using source in a package is disencouraged because it changes the current environment
  # However, I don't think its a problem here
  # Is there a better solution without having to write more code?
  source(here::here("data", "raw", "api_spec.R"))
  url <- api_urls[[endpoint]]
  
  # Specify parameters if they are provided
  if(!is.null(start_date) && !is.null(end_date)){
    # Make sure that the date format is correct
    start_date <- lubridate::ymd(start_date)
    end_date <- lubridate::ymd(end_date)
    
    param = paste('?start_date=', start_date, sep = '')
    param = paste(param, "&end_date=", sep = '')
    param = paste(param, end_date, sep = '')
    
    url <- paste(url, param, sep = '')
  } 
  if(xor(is.null(start_date), is.null(end_date))){
    warning("Either both start_date and end_date must be specified or none")
    return(NULL)
  }
  
  # Make GET request and extract the data
  r <- httr::GET(url)
  js <- jsonlite::fromJSON(httr::content(r, as='text'))
  
  return(js$response$data)
}

