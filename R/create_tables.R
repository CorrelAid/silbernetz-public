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
create_provider_table <- function(df, unit, period_start, period_stop) {
  # Check for appropriate unit input
  if (!unit %in% c("week", "month", "day", "year")) {
    stop("unit must be either 'week', 'month', 'day' or 'year'")
  }

  # Build the data accordingly to the chosen time frame
  df$date = as.Date(df$date, tryFormats = c("%Y-%m-%d"), optional = FALSE)
  if (unit == "week") {
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(
        Kalenderwoche = as.integer(lubridate::isoweek(date)),
        Jahr = lubridate::isoyear(date)
      ) %>%
      group_by(Jahr, Kalenderwoche)
  } else if (unit == "month") {
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(Monat = lubridate::month(date), Jahr = lubridate::year(date)) %>%
      group_by(Jahr, Monat)
  } else if (unit == "year") {
    grouped_table <- df %>%
      filter(date >= period_start & date <= period_stop) %>%
      mutate(Jahr = lubridate::year(date)) %>%
      group_by(Jahr)
  } else {
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
      `Mittlere Telefonatdauer (min)` = round(
        mean(duration_outbound[success]) / 60,
        1
      ),
      `Längstes Telefonat (min)` = round(
        max(duration_outbound[success]) / 60,
        1
      ),
      `Versch. Anrufer*innen` = length(unique(caller)), #  this is still wrong - Tine: actually I don't think it is
      # Anrufer = length(unique(caller[which(success=="TRUE")])) #  this is still wrong
      `Mittlere Anrufe pro Anrufer*in` = round(
        Anrufe / `Versch. Anrufer*innen`,
        1
      ),
      `Erstanrufer*innen` = sum(firstcall)
    )

  # If we build the daily table, clean up a bit:
  if (unit == "day") {
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


#' Function to build Elke's Herkunft Table which summarises
#' the number of calls per Bundesland across weeks
#'
#' @param df Dataframe/Tibble containing the input data
#' @param start_date Start of the reporting period
#' @param end_date End of the reporting period
#' @param bula_mapping tibble with full names and short names, to join to the calls df.
#' @return Returns table
#'
#' @import dplyr lubridate tidyr
#'
#' @export
create_herkunft_table <- function(df, start_date, end_date, bula_mapping) {
  # create large table with anrufe and anrufer weekly by bundesland
  df_processed <- df %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::left_join(
      bula_mapping |> select(Bundesland, bl_short),
      by = c("Bundesland")
    ) |>
    dplyr::mutate(
      bl_short_display = case_when(
        # landline and no bundesland recorded -> unknown
        is.na(bl_short) & is_landline ~ "Unbk.",
        is_landline == FALSE ~ "Mobil",
        .default = bl_short
      )
    )
  # filter(success) %>% # we do not filter here, but take all callers, even those
  # who didn't get through
  herkunft_table <- df_processed %>%
    dplyr::mutate(
      Kalenderwoche = lubridate::isoweek(date),
      Jahr = lubridate::isoyear(date)
    ) %>%
    dplyr::group_by(Jahr, Kalenderwoche) %>%
    dplyr::mutate(
      Start = format(min(date), "%Y-%m-%d"),
      Ende = format(max(date), "%Y-%m-%d")
    ) %>%
    dplyr::group_by(Jahr, Start, bl_short_display) %>%
    dplyr::summarise(
      Kalenderwoche = first(Kalenderwoche),
      Ende = first(Ende),
      Anrufe = n(),
      `Anrufer*innen` = length(unique(caller)),
      `Erstanrufer*innen` = sum(firstcall),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = bl_short_display,
      values_from = c(Anrufe, `Anrufer*innen`, `Erstanrufer*innen`),
      names_glue = "{bl_short_display} {.value}"
    ) %>%
    dplyr::mutate(
      `Alle Anrufer*innen` = rowSums(
        across(ends_with(" Anrufer*innen")),
        na.rm = TRUE
      ),
      `Alle Anrufe` = rowSums(across(ends_with(" Anrufe")), na.rm = TRUE),
      `Alle Erstanrufer*innen` = rowSums(
        across(ends_with(
          " Erstanrufer*innen"
        )),
        na.rm = TRUE
      )
    ) %>%
    dplyr::mutate(
      across(contains("ruf"), ~ ifelse(is.na(.x), 0, .x)),
      Jahr = as.character(Jahr)
    )

  # create a nice order of columns
  cols_at_start <- c(
    "Jahr",
    "Kalenderwoche",
    "Start",
    "Ende"
  )
  herkunft_table <- herkunft_table %>%
    dplyr::select(
      tidyselect::all_of(cols_at_start),
      contains("Alle"),
      contains("Mobil"),
      sort(setdiff(
        names(.),
        c(fixed, names(select(., contains("Alle"), contains("Mobil"))))
      )) # takes all columns that have not been sorted yet and sorts them alphabetically.  works out fine because "Unbek." is after the last Bundesland (TH)
    ) %>%
    dplyr::arrange(desc(Start)) %>%
    dplyr::mutate(Start = lubridate::as_date(Start)) %>%
    dplyr::mutate(Ende = lubridate::as_date(Ende)) %>%
    dplyr::mutate(Kalenderwoche = as.integer(Kalenderwoche))

  # The following is not really needed, information can be gained from other app features
  # more robustly

  # # ceare a summary row
  # gesamt <- df_processed %>%
  #   group_by(Bundesland) %>%
  #   summarise(Anrufe = n(),
  #             `Anrufer*innen` = length(unique(caller)),
  #             `Erstanrufer*innen` = sum(firstcall)) %>%
  #   pivot_wider(names_from = Bundesland, values_from = c(Anrufe, `Anrufer*innen`, `Erstanrufer*innen`),
  #               names_glue = "{Bundesland} {.value}") %>%
  #   mutate(`Alle Anrufer*innen` = rowSums(across(ends_with(" Anrufer*innen")), na.rm = TRUE),
  #          `Alle Anrufe` = rowSums(across(ends_with(" Anrufe")), na.rm = TRUE),
  #          `Alle Erstanrufer*innen` = rowSums(across(ends_with(" Erstanrufer*innen")), na.rm = TRUE)) %>%
  #   mutate(across(contains("ruf"), ~ifelse(is.na(.x), 0, .x)),
  #          Jahr = "Gesamt",
  #          Kalenderwoche = NA,
  #          Start = NA,
  #          Ende = NA)
  #
  # # order columns as in herkunft_table
  # gesamt <- gesamt[,names(herkunft_table)]
  #
  # # merge
  # herkunft_table <- bind_rows(
  #   gesamt,
  #   herkunft_table)

  return(herkunft_table)
}
