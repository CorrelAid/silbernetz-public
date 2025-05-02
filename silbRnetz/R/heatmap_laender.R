#' Bundeslaender heatmap
#'
#' Creates a heatmap for frequency of calls or callers by Bundesland for
#' a specified time period.
#'
#' @param numbers_geo A dataframe of endpoint type "Numbers" with geographic data
#' added, as returned by [add_geodata_to_numbers()]
#' as can be returned by [download_data()].
#' @param variable Which variable should be counted? Either "Anrufe", "Anrufer" oder "Erstanrufer".
#' @param start_date Start date of time span to be plotted (if not specified,
#' minimum date in `numbers_geo` is used).
#' @param end_date End date of time span to be plotted (if not specified,
#' maximum date in `numbers_geo` is used).
#' @param filter_success If only successfull calls ("Verbunden") should
#' be taken into account.
#' @param show_zeros If areas with zero count should be included in the color scale
#' as numeric zeros. Default alternative is to show them as grey areas without any label.
#'
#' @return Returns a ggplot.
#'
#' @import dplyr tidyr stringr ggplot2 patchwork ggtext
#'
#' @examples
#' \dontrun{
#' heatmap_laender(add_geodata_to_numbers(download_data("Numbers",
#' start_date = "2021-05-30", end_date = "2021-05-31")),
#' variable = "Anrufer")
#' }
#'
#' @export
heatmap_laender <- function(numbers_geo, variable, start_date = NULL, end_date = NULL,
                            filter_success = FALSE, show_zeros = FALSE){

  if(!variable %in% c("Anrufe", "Anrufer", "Erstanrufer")){
    stop("variable has to be one of 'Anrufe', 'Anrufer' or 'Erstanrufer'")
  }

  # set gendered variable lable for plot
  variable_label <- case_when(
    variable == "Anrufer" ~ "Anrufer*innen",
    variable == "Erstanrufer" ~ "Erstanrufer*innen",
    TRUE ~ "Anrufe"
  )

  #Check if OS is Mac:
  if(Sys.info()['sysname'] == "Darwin"){
    font <- 'sans'
  } else{
    # check if Silbernetz font (Open Sans) installed
    installed_fonts <- extrafont::fonts()
    extrafont::loadfonts(device="win", quiet = TRUE)
    if((!"Open Sans" %in% installed_fonts)){
      warning("The font Open Sans does not seem to be installed. Using Arial instead.")
      font <- "Arial"}else{
        font <- "Open Sans"
      }
  }

  # turn shape file into dataframe
  df_germany_lev1 <- fortify(germany_lev1 , region = "NAME_1") %>%
    mutate(across(.cols=c(id, group), ~str_replace(.x, "Ã¼", "ue"))) %>%
    mutate(across(.cols=c(id, group), ~str_replace(.x, "ü", "ue"))) # need 'ue' to match with bundeslaendern

  # filter data if requested
  if (filter_success) {
    numbers_geo <- numbers_geo %>% filter(success)
  }

  if (!is.null(start_date)) {
    numbers_geo <- numbers_geo %>%
      filter(date >= lubridate::ymd(start_date))
  } else
    (start_date <- min(numbers_geo$date))

  if (!is.null(end_date)) {
    numbers_geo <- numbers_geo %>%
      filter(date <= lubridate::ymd(end_date))
  } else
    (end_date <- max(numbers_geo$date))

  # if no data between start and end date available, print message
  if(nrow(numbers_geo) == 0){
    final_plot <- ggplot(data.frame(x = 1:100, y = 1:100) )+
      geom_point(aes(x,y), color = 'white', size = 0) +
      annotate("text", x=50, y=50, label= "No data available",
               size = 7)+
      theme_void()
    return(final_plot)
  }

  # for plot title, turn labels into German standard format
  start_date <- paste(str_sub(start_date, 9, 10),
                      str_sub(start_date, 6, 7),
                      str_sub(start_date, 1, 4), sep = ".")
  end_date <- paste(str_sub(end_date, 9, 10),
                    str_sub(end_date, 6, 7),
                    str_sub(end_date, 1, 4), sep = ".")

  # aggregate calls by bundesland
  calls_aggr <- numbers_geo %>%
    mutate(Bundesland = ifelse(landline == FALSE, "mobil", Bundesland)) %>%
    group_by(Bundesland) %>%
    summarise(Anrufer = length(unique(caller)),
              Anrufe = n(),
              Erstanrufer = sum(firstcall),
              .groups = "drop")

  # merge spatial data and caller data
  df_germany_lev1 <- left_join(df_germany_lev1,
                               calls_aggr %>% filter(Bundesland != "mobil"),
                               by = c("id" = "Bundesland"))

  # replace zero counts in any bundesland by NA (for the special coloring)
  df_germany_lev1 <- df_germany_lev1 %>%
    mutate(!!as.symbol(variable) := ifelse(!!as.symbol(variable) == 0, NA, !!as.symbol(variable)))

  # dynamically create new dataframe with coordinates for bundesland names and count
  coord_names <- df_germany_lev1 %>%
    mutate(Bundesland_kuerzel =
             case_when( # use abbreviations for more handy layout
               id == "Baden-Wuerttemberg" ~ "BW",
               id == "Bayern" ~ "BY",
               id == "Berlin" ~ "BE",
               id == "Brandenburg" ~ "BB",
               id == "Bremen" ~ "BR",
               id == "Hamburg" ~ "HH",
               id == "Hessen" ~ "HE",
               id == "Mecklenburg-Vorpommern" ~ "MV",
               id == "Niedersachsen" ~ "NI",
               id == "Nordrhein-Westfalen" ~ "NW",
               id == "Rheinland-Pfalz" ~ "RP",
               id == "Saarland" ~ "SL",
               id == "Sachsen" ~ "SN",
               id == "Sachsen-Anhalt" ~ "ST",
               id == "Schleswig-Holstein" ~ "SH",
               id == "Thueringen" ~ "TH"
             )) %>%
    group_by(id) %>%
    summarise(Bundesland_kuerzel = unique(Bundesland_kuerzel),
              centr_long = median(long),
              centr_lat  = median(lat),
              !!as.symbol(variable) := first(!!as.symbol(variable)),
              .groups = "drop") %>%
    mutate(centr_lat = case_when( # some manual adjustments of label positions
      id == "Berlin"        ~ centr_lat - 0.175,
      id == "Bremen"        ~ centr_lat - 0.475,
      id == "Niedersachsen" ~ centr_lat - 0.85,
      id == "Saarland"      ~ centr_lat + 0.1,
      id == "Schleswig-Holstein" ~ centr_lat -0.3,
      id == "Sachsen" ~ centr_lat + 0.2,
      id == "Brandenburg"   ~ centr_lat + 0.4,
      id == "Baden-Wuerttemberg" ~ centr_lat + 1.1,
      id == "Mecklenburg-Vorpommern" ~ centr_lat - 0.3,
      id == "Sachsen-Anhalt" ~ centr_lat  + 0.2,
      id == "Hessen" ~ centr_lat + 0.1,
      id == "Bayern" ~ centr_lat + 0.25,
      id == "Thueringen" ~ centr_lat + 0.1,
      TRUE ~ centr_lat
    ),
    centr_long = case_when(
      id == "Brandenburg"   ~ centr_long -0.35,
      id == "Bremen"        ~ centr_long +0.15,
      id == "Hamburg"        ~ centr_long -0.1,
      id == "Niedersachsen" ~ centr_long +1.8,
      id == "Sachsen-Anhalt" ~ centr_long - 0.2,
      id == "Baden-Wuerttemberg" ~ centr_long + 0.45,
      id == "Bayern" ~ centr_long + 0.85,
      id == "Schleswig-Holstein" ~ centr_long +0.85,
      id == "Mecklenburg-Vorpommern" ~ centr_long - 0.8,
      id == "Nordrhein-Westfalen" ~ centr_long + 0.7,
      id == "Thueringen" ~ centr_long - 0.15,
      id == "Saarland"   ~ centr_long - 0.075,
      TRUE ~ centr_long
    ))

  print(coord_names[,variable])
  print(coord_names$centr_long)
  print(coord_names$centr_lat)
  
  # ggplotting
  plot_map_bdl <- ggplot(df_germany_lev1) +
    geom_polygon(aes(
      x = long,
      y = lat,
      group = id,
      subgroup = group,
      fill = !!as.symbol(variable)),
      col = "black",
      linewidth = 0.125) +
    theme_void(14) +
    # background boxes (count)
    # geom_richtext(
    #   data = coord_names,
    #   aes(
    #     x = centr_long,
    #     y = centr_lat,
    #     label = ifelse(
    #       is.na(!!as.symbol(variable)), NA,
    #       format(!!as.symbol(variable), big.mark = ".", decimal.mark = ","))
    #   ),
    #   fontface = "bold",
    #   col = "darkgrey",
    #   lineheight = 0,
    #   label.colour = NA,
    #   label.r = unit(0, "lines"),
    #   label.padding = unit(c(0.125,0.125,0,0.125), "lines"),
    #   alpha = 0.4,
    #   family = font) +
    # text (count)
    geom_text(
      data = coord_names,
      aes(
        x = centr_long,
        y = centr_lat,
        label = ifelse(
          is.na(!!as.symbol(variable)), NA,
          format(!!as.symbol(variable), big.mark = ".", decimal.mark = ","))
      ),
      fontface = "bold",
      col = "black",
      lineheight = 0,
      label.colour = NA,
      label.r = unit(0, "lines"),
      label.padding = unit(c(0.125,0.125,0,0.125), "lines"),
      fill = NA,
      alpha = 1,
      family = font) +
    # text (bundesland name)
    geom_text(
      data = coord_names,
      aes(
        x = centr_long,
        y = centr_lat-1/5,
        label = Bundesland_kuerzel
      ),
      family = font) +
    scale_fill_gradient(low = corporate_rose,
                        high = corporate_red,
                        breaks = scales::pretty_breaks(n = 4),
                        na.value="grey90") + # corporate_lightblue, corporate_blue
    theme(
      plot.title = element_text(hjust = 1 / 2, family = font),
      legend.position = "bottom",
      plot.margin = margin(
        t = 1,
        r = 50,
        b = 1,
        l = 1),
      legend.text = element_text(size = 7, family = font))+
    guides(fill = guide_colourbar(barwidth = 12, barheight = 1))


  if(start_date != end_date){ # add title, which either refers to a period or single day
    plot_map_bdl <- plot_map_bdl + labs(
      title = paste0("Anzahl der ", variable_label, " im Zeitraum\n",
                     start_date," bis ", end_date),
      fill = NULL)
  } else(
    plot_map_bdl <- plot_map_bdl + labs(
      title = paste0("Anzahl der ", variable_label, " am\n",
                     start_date),
      fill = NULL))

  plot_map_mobil <- ggplot() + # create mobile phone count
    theme_void(14) +
    geom_point(aes(x = 0, y = 0), size = 42, col = corporate_yellow) +
    geom_text(aes(
      x = 0,
      y = 0,
      label = paste0(
        "Mobilfunk:\n",
        format(calls_aggr %>%
                 filter(Bundesland == "mobil") %>%
                 pull(!!as.symbol(variable)),
               big.mark = ".", decimal.mark = ","), "\n",
        "Festnetz:\n",
        format(calls_aggr %>%
                 filter(Bundesland != "mobil") %>%
                 pull(!!as.symbol(variable)) %>% sum,
               big.mark = ".", decimal.mark = ","))),
      family = font,
      fontface = "bold")

  # inset both counts into plot
  plot_map <- plot_map_bdl + inset_element(
    plot_map_mobil,
    left = 0.75,
    bottom = 0.7,
    right = 1,
    top = 0,
    align_to = "full")

  return(plot_map)
}
