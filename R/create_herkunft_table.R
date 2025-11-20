#' Function to build Elke's Herkunft Table which summarises
#' the number of calls per Bundesland across weeks
#'
#' @param df Dataframe/Tibble containing the input data
#' @param start_date Start of the reporting period
#' @param end_date End of the reporting period
#'
#' @return Returns table
#'
#' @import dplyr lubridate tidyr
#'
#' @export
create_herkunft_table <- function(df, start_date, end_date) {
  # create large table with anrufe and anrufer weekly by bundesland
  df_processed <- df %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(
      Bundesland = case_when(
        # use abbreviations for more handy layout
        Bundesland == "Baden-Wuerttemberg" ~ "BW",
        Bundesland == "Bayern" ~ "BY",
        Bundesland == "Berlin" ~ "BE",
        Bundesland == "Brandenburg" ~ "BB",
        Bundesland == "Bremen" ~ "BR",
        Bundesland == "Hamburg" ~ "HH",
        Bundesland == "Hessen" ~ "HE",
        Bundesland == "Mecklenburg-Vorpommern" ~ "MV",
        Bundesland == "Niedersachsen" ~ "NI",
        Bundesland == "Nordrhein-Westfalen" ~ "NW",
        Bundesland == "Rheinland-Pfalz" ~ "RP",
        Bundesland == "Saarland" ~ "SL",
        Bundesland == "Sachsen" ~ "SN",
        Bundesland == "Sachsen-Anhalt" ~ "ST",
        Bundesland == "Schleswig-Holstein" ~ "SH",
        Bundesland == "Thueringen" ~ "TH",
        is.na(Bundesland) & landline ~ "Unbk.",
        landline == FALSE ~ "Mobil"
      )
    )
  # filter(success) %>% # we do not filter here, but take all callers, even those
  # who didn't get through

  herkunft_table <- df_processed %>%
    mutate(Kalenderwoche = isoweek(date), Jahr = lubridate::isoyear(date)) %>%
    group_by(Jahr, Kalenderwoche) %>%
    mutate(
      Start = format(min(date), "%Y-%m-%d"),
      Ende = format(max(date), "%Y-%m-%d")
    ) %>%
    group_by(Jahr, Start, Bundesland) %>%
    summarise(
      Kalenderwoche = first(Kalenderwoche),
      Ende = first(Ende),
      Anrufe = n(),
      `Anrufer*innen` = length(unique(caller)),
      `Erstanrufer*innen` = sum(firstcall),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = Bundesland,
      values_from = c(Anrufe, `Anrufer*innen`, `Erstanrufer*innen`),
      names_glue = "{Bundesland} {.value}"
    ) %>%
    mutate(
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
    mutate(
      across(contains("ruf"), ~ ifelse(is.na(.x), 0, .x)),
      Jahr = as.character(Jahr)
    )

  # create a nice order of columns
  herkunft_table <- herkunft_table %>%
    select(
      Jahr,
      Kalenderwoche,
      Start,
      Ende,
      `Alle Anrufe`,
      `Alle Anrufer*innen`,
      `Alle Erstanrufer*innen`,
      contains("Mobil"),
      contains("BW "),
      contains("BY "),
      contains("BE "),
      contains("BB "),
      contains("BR "),
      contains("HH "),
      contains("HE "),
      contains("MV "),
      contains("NI "),
      contains("NW "),
      contains("RP "),
      contains("SL "),
      contains("SN "),
      contains("ST "),
      contains("SH "),
      contains("TH "),
      contains("Unbk.")
    ) %>%
    arrange(desc(Start)) %>%
    mutate(Start = lubridate::as_date(Start)) %>%
    mutate(Ende = lubridate::as_date(Ende)) %>%
    mutate(Kalenderwoche = as.integer(Kalenderwoche))

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
