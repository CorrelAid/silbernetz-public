#' Plot number of calls over time
#'
#' @param data Data frame
#' @param show_success Boolean whether only successfull calls should be shown
#' @param show_firstcall Boolean whether only first calls should be shown
#' @param start_date start date, format "YYYY-MM-DD"
#' @param end_date End date, format "YYYY-MM-DD"
#' @param y_lim_zero Whether the lower limit of the y axis should be zero
#'
#' @import lubridate dplyr scales
#' @return plot
#' @export
plot_CallsOverTime <- function(
  data,
  show_success,
  show_firstcall,
  start_date,
  end_date,
  nr_breaks,
  state = 'Alle',
  y_lim_zero = FALSE
) {
  #Check if OS is Mac and adjust fonts:
  if (Sys.info()['sysname'] == "Darwin") {
    font <- 'sans'
  } else {
    # check if Silbernetz font (Open Sans) installed
    installed_fonts <- extrafont::fonts()
    extrafont::loadfonts(device = "win", quiet = TRUE)
    if ((!"Open Sans" %in% installed_fonts)) {
      warning(
        "The font Open Sans does not seem to be installed. Using Arial instead."
      )
      font <- "Arial"
    } else {
      font <- "Open Sans"
    }
  }

  # Filter data accordingly to the parameters
  if (show_success) {
    data <- data %>% filter(success)
  }
  if (show_firstcall) {
    data <- data %>% filter(firstcall)
  }
  if (state != 'Alle') {
    data <- data %>% filter(Bundesland == state)
  }

  # Build relevant dataset
  rel_data <- data %>%
    mutate(date = lubridate::as_date(date)) %>%
    filter(date >= start_date & date <= end_date) %>%
    mutate(
      unique_week = paste0(
        lubridate::isoweek(date),
        "-",
        lubridate::isoyear(date)
      )
    ) %>%
    group_by(unique_week) %>%
    summarise(
      n_calls = n(),
      start_date = min(date),
      end_date = max(date),
      .groups = "drop"
    )

  # Remove incomplete calendar weeks (can occur at beginning and end of selected time frame)
  # as they bias the look of the time trend
  rel_data <- rel_data %>%
    group_by(unique_week) %>%
    mutate(is_incomplete = start_date + 6 != end_date) %>%
    ungroup %>%
    filter(!is_incomplete)

  # If there is no data to display, show an emtpy plot with an explanation
  if (nrow(rel_data) == 0) {
    final_plot <- ggplot(data.frame(x = 1:100, y = 1:100)) +
      geom_point(aes(x, y), color = 'white', size = 0) +
      annotate(
        "text",
        x = 50,
        y = 50,
        label = "No data / full calendar weeks available",
        size = 7
      ) +
      theme_void()
    return(final_plot)
  }

  # Build caption
  if (show_firstcall & show_success) {
    title <- "Anzahl der Anrufe\n(nur Erstanrufer*innen und zustandegekommene Verbindungen)"
  }
  if (!show_firstcall & show_success) {
    title <- "Anzahl der Anrufe\n(nur zustandegekommene Verbindungen)"
  }
  if (show_firstcall & !show_success) {
    title <- "Anzahl der Anrufe\n(nur Erstanrufer*innen)"
  }
  if (!show_firstcall & !show_success) {
    title <- "Anzahl der Anrufe"
  }

  scale_round <- function(x) ceiling(x, 0)

  # Generate final plot
  final_plot <- ggplot(rel_data) +
    geom_line(
      aes(x = start_date, y = n_calls),
      col = corporate_purple,
      size = 0.8
    ) +
    geom_point(
      aes(x = start_date, y = n_calls),
      col = corporate_purple,
      size = 1.6
    ) +
    labs(x = "Datum", y = "Anrufe pro Kalenderwoche", title = title) +
    theme_bw() +
    theme(text = element_text(family = font)) +
    scale_x_date(
      date_labels = "%d. %b %Y",
      breaks = scales::pretty_breaks(nr_breaks)
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(7))

  # Set y-axis according to parameters
  if (y_lim_zero) {
    final_plot <- final_plot +
      scale_y_continuous(breaks = scales::pretty_breaks(7), limits = c(0, NA))
  }

  return(final_plot)
}
