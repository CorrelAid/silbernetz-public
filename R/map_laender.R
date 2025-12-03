#' Bundeslaender heatmap
#'
#' Creates a heatmap for frequency of calls or callers by Bundesland for
#' a specified time period.
#'
#' @param numbers_geo A dataframe of endpoint type "Numbers" with geographic data
#' added, as returned by [add_geodata_to_numbers()]
#' as can be returned by [download_data()].
#' @param bula_geo a sf object containing the multipolygons for the Bundesländer
#' @param variable Which variable should be counted? Either "Anrufe", "Anrufer" oder "Erstanrufer".
#' @param colors named character vector containing silbernetz colors
#' @param start_date Start date of time span to be plotted (if not specified,
#' minimum date in `numbers_geo` is used).
#' @param end_date End date of time span to be plotted (if not specified,
#' maximum date in `numbers_geo` is used).
#' @param filter_success If only successfull calls ("Verbunden") should
#' be taken into account.
#' @param show_zeros If areas with zero count should be included in the color scale
#' as numeric zeros. Default alternative is to show them as grey areas without any label.

#' @return Returns a ggplot.
#'
#' @import dplyr tidyr stringr ggplot2 patchwork ggtext sf
#'
#' @examples
bundeslaender_map <- function(
  numbers_geo,
  bula_geo,
  variable,
  colors,
  start_date = NULL,
  end_date = NULL,
  filter_success = FALSE,
  show_zeros = FALSE
) {
  if (!variable %in% c("Anrufe", "Anrufer", "Erstanrufer")) {
    stop("variable has to be one of 'Anrufe', 'Anrufer' or 'Erstanrufer'")
  }

  # SETUP
  # set gendered variable lable for plot
  variable_label <- dplyr::case_when(
    variable == "Anrufer" ~ "Anrufer*innen",
    variable == "Erstanrufer" ~ "Erstanrufer*innen",
    TRUE ~ "Anrufe"
  )

  # for plot title, turn labels into German standard format
  start_date_f <- strftime(start_date, "%d.%m.%Y")
  end_date_f <- strftime(end_date, "%d.%m.%Y")
  plot_title <- ifelse(
    start_date != end_date,
    sprintf(
      "Anzahl der %s im Zeitraum\n %s bis %s",
      variable_label,
      start_date_f,
      end_date_f
    ),
    sprintf("Anzahl der %s am\n%s", variable_label, start_date_f)
  )

  # FILTER
  # filter only successful calls
  if (filter_success) {
    numbers_geo <- numbers_geo %>% filter(success)
  }

  # filter for date range
  # if parameter is null, we take min of dataset. if not null, take later date (natural lower limit is dataset min)
  start_date <- max(start_date, min(numbers_geo$date)) |> lubridate::ymd()
  end_date <- min(end_date, max(numbers_geo$date)) |> lubridate::ymd() # same for end_date
  numbers_geo <- numbers_geo |>
    filter(date >= start_date & date <= end_date)

  # if no data between start and end date available, print message
  # TODO replace with error message
  if (nrow(numbers_geo) == 0) {
    final_plot <- ggplot(data.frame(x = 1:100, y = 1:100)) +
      geom_point(aes(x, y), color = 'white', size = 0) +
      annotate(
        "text",
        x = 50,
        y = 50,
        label = sprintf(
          "No data available between %s and %s",
          start_date,
          end_date
        ),
        size = 7
      ) +
      theme_void()
    return(final_plot)
  }

  # AGGREGATION AND JOIN TO GEODATA
  # aggregate calls by bundesland
  calls_aggr <- numbers_geo %>%
    dplyr::mutate(
      Bundesland = if_else(is_landline == FALSE, "mobil", Bundesland)
    ) %>%
    dplyr::group_by(Bundesland, .drop = FALSE) %>%
    dplyr::summarise(
      n = case_when(
        {{ variable }} == "Anrufer" ~ length(unique(caller)),
        {{ variable }} == "Anrufe" ~ n(),
        {{ variable }} == "Erstanrufer" ~ sum(firstcall)
      )
    ) |>
    dplyr::mutate(n = if_else(is.na(n), 0, n))

  # merge spatial data and caller data
  bula_geo <- dplyr::left_join(
    bula_geo,
    calls_aggr %>% filter(Bundesland != "mobil"),
    by = c("Bundesland")
  ) |>
    dplyr::mutate(n = if_else(n == 0, NA, n)) |> # set 0 to NA so that it gets colored in grey
    dplyr::mutate(label_n = if_else(is.na(n), 0, n)) # but for labelling we want to have 0 instead of NA

  # coordinates for the label (Short BuLa label)
  label_coordinates <- sf::st_point_on_surface(bula_geo) |>
    dplyr::mutate(
      lon = st_coordinates(geometry)[, 1] + lon_correct,
      lat = st_coordinates(geometry)[, 2] + lat_correct
    ) |>
    sf::st_drop_geometry() %>%
    sf::st_as_sf(coords = c("lon", "lat")) %>%
    sf::st_set_crs(sf::st_crs(bula_geo))

  # CREATE MAP
  plot_map <- ggplot(
    bula_geo
  ) +
    geom_sf(aes(fill = n)) +
    geom_sf_label(
      mapping = aes(
        label = bl_short
      ),
      data = label_coordinates,
      label.size = 0.0, # thickness of the label border
      label.r = unit(0.15, "lines"), # border corner roundness
      label.padding = unit(0.1, "lines"),
      alpha = .5,
      fill = "white",
      color = "black", # text color
    ) + # internal padding around text+
    geom_sf_label(
      mapping = aes(
        label = scales::label_comma(big.mark = ".", decimal.mark = ",")(label_n)
      ),
      fontface = "bold",
      data = label_coordinates,
      nudge_y = 0.25, # put value above the bundesland short label
      label.size = 0.0, # thickness of the label border
      label.r = unit(0.15, "lines"), # border corner roundness
      label.padding = unit(0.1, "lines"),
      alpha = .5,
      fill = "white",
      color = "black", # text color
    ) + # internal padding around text+
    scale_fill_gradient(
      low = colors["corporate_rose"],
      high = colors["corporate_red"],
      breaks = scales::pretty_breaks(n = 4),
      na.value = "grey90",
    ) +
    theme_void(base_size = 14) +
    theme(
      text = element_text(family = "Open Sans"),
      plot.title = element_text(hjust = 1 / 2),
      legend.position = "bottom",
      plot.margin = margin(
        t = 1,
        r = 50,
        b = 1,
        l = 1
      ),
      legend.text = element_text(size = 7)
    ) +
    labs(title = plot_title, fill = NULL) +
    guides(fill = guide_colourbar(barwidth = 12, barheight = 1))

  plot_yellow_circle <- create_yellow_circle(calls_aggr, colors)
  # inset both counts into plot
  p <- plot_map +
    patchwork::inset_element(
      plot_yellow_circle,
      left = 0.7,
      bottom = 0.7,
      right = 1,
      top = 0,
      align_to = "full"
    )
  p
}


create_yellow_circle <- function(calls_aggr, colors) {
  plot_map_mobil <- ggplot2::ggplot() + # create mobile phone count
    ggplot2::theme_void(14) +
    ggplot2::geom_point(
      aes(x = 0, y = 0),
      size = 50,
      col = colors["corporate_yellow"]
    ) +
    ggplot2::geom_text(
      aes(
        x = 0,
        y = 0,
        label = paste0(
          "Mobilfunk:\n",
          format(
            calls_aggr %>%
              filter(Bundesland == "mobil") %>%
              pull(n),
            big.mark = ".",
            decimal.mark = ","
          ),
          "\n",
          "Festnetz:\n",
          format(
            calls_aggr %>%
              filter(Bundesland != "mobil") %>%
              pull(n) %>%
              sum,
            big.mark = ".",
            decimal.mark = ","
          )
        )
      ),
      family = "Open Sans",
      fontface = "bold"
    )
}
