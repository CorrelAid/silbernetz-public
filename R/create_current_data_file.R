#' Function to create current data file
#' This file is supposed to be called once per installation
#'
#' @param end_date: Current date, formate "YYYY-MM-DD"
#' @param write_path: Where to write the resulting csv, should correspond the "file" variable in shinyapp.R
#'
#' @import lubridate dplyr
#' @return None, only side effect of writing the file
#' @export


create_current_data <- function(end_date, write_path = "../data/raw/current_data.csv"){
    df_complete1 <- download_data("Numbers", start_date = "2020-01-01", end_date = "2021-07-26")
    # loading is split, because access is limited to 100,000 rows at a time
    df_complete2 <- download_data("Numbers", start_date = "2021-07-27", end_date = end_date)
    df_complete <- rbind(df_complete1,
                         df_complete2) %>%
      mutate(date = lubridate::ymd(date))

    df_complete <- df_complete %>%
      #add_caller_count() %>%
      add_geodata_to_numbers() %>%
      remove_redundant_cols() %>%
      hash_col()

    df_complete <- df_complete %>%
      group_by(caller) %>%
      mutate(datetime = lubridate::ymd_hms(paste(date,time)),
             firstcall = datetime == min(datetime)) %>%
      ungroup %>% select(-datetime)

    # Put the data where shiny will try to access it
    # (look at the "file" variable in shinyapp.R)
    readr::write_csv(df_complete, write_path)
}
