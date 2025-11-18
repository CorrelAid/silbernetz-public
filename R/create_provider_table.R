#' Function to build Elke's Provider Table which summarises
#' the number of calls per week or month
#'
#' @param df Dataframe/Tibble containing the input data
#' @param unit Aggregate to either weeks ("week"), months ("month") or days ("day") or years ("year")
#' @param period_start Start of the reporting period
#' @param period_stop End of the reporting perios
#'
#' @return Returns Elke's provider table
#'
#' @import dplyr lubridate
#'
#' @export
create_provider_table <- function(df, unit, period_start, period_stop){
  # Check for appropriate unit input
  if(!unit %in% c("week", "month", "day", "year")){
    stop("unit must be either 'week', 'month', 'day' or 'year'")
  }

  # Build the data accordingly to the chosen time frame
  df$date = as.Date(df$date, tryFormats = c("%Y-%m-%d"), optional = FALSE)
  if(unit == "week"){
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(Kalenderwoche = as.integer(lubridate::isoweek(date)),
             Jahr = lubridate::isoyear(date)) %>%
      group_by(Jahr, Kalenderwoche)

  } else if(unit == "month"){
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(Monat = lubridate::month(date),
             Jahr = lubridate::year(date)) %>%
      group_by(Jahr, Monat)
  } else if(unit == "year"){
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(Jahr = lubridate::year(date)) %>%
      group_by(Jahr)
  } else{
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      group_by(date)
  }

  # Build the final provider table
  # Some columns are disabled but left as a comment in case we
  # want to include them later
  provider_table <- grouped_table %>%
    summarise(
      # To make sorting easier we use YYYY-MM-DD format
      Start = format(min(date), "%Y-%m-%d"),
      Ende = format(max(date), "%Y-%m-%d"),
      Anrufe = length(which(!is.na(caller))),
      `Anrufe (erfolgreich)` = length(which(success)),
      `Telefonate < 1min` = length(which(duration_outbound < 60 & success)),
      #`Telefonate mind. 1min` = length(which(duration_outbound >= 60 & success)), # not needed I'd say (sg)
      `Mittlere Telefonatdauer (min)` = round(mean(duration_outbound[success])/60,1),
      `Längstes Telefonat (min)` = round(max(duration_outbound[success])/60,1),
      `Versch. Anrufer*innen` = length(unique(caller)), #  this is still wrong - Tine: actually I don't think it is
      # Anrufer = length(unique(caller[which(success=="TRUE")])) #  this is still wrong
      `Mittlere Anrufe pro Anrufer*in` = round(Anrufe/`Versch. Anrufer*innen`,1),
      `Erstanrufer*innen` = sum(firstcall)
    )

  # If we build the daily table, clean up a bit:
  if(unit=="day"){
    provider_table <- provider_table %>%
      rename("Tag" = "date") %>%
      select(-any_of(c("Start", "Ende"))) %>%
      mutate(Wochentag = lubridate::wday(Tag, label = TRUE)) %>%
      select(Tag, Wochentag, everything())
  } else {
    provider_table <- provider_table %>%
      mutate(Start = lubridate::as_date(Start)) %>%
      mutate(Ende = lubridate::as_date(Ende))
  }


  return(provider_table)
}
