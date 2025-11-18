library(shiny)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(here)
library(sf)

# read in data
geo <- readr::read_csv("data/geo/mastertable_vorw_bdl.csv")
bula_map <- readr::read_csv("data/geo/mapping-bula-short.csv")
bula_geo <- sf::read_sf("data/geo/DEU_adm/", layer = "DEU_adm1") |>
  dplyr::rename(Bundesland = NAME_1) |>
  dplyr::inner_join(bula_map, by = "Bundesland")


current_data <- fs::dir_ls('data/raw/annual') |>
  purrr::map_dfr(readr::read_csv, col_types = "Dtciillicccl") |>
  add_first_call_column()

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Silbernetz"),
  hr(),

  tabsetPanel(
    ## Tabellen-Panel ----------------------------------------------------------
    tabPanel(
      "Tabellen",
      fluidRow(column(12, h4("Reporting updaten:"))),
      fluidRow(
        column(
          12,
          align = 'center',
          tags$head(tags$style(
            type = "text/css",
            "
             #loadmessage {
               font-weight: bold;
             }
          "
          )),
          actionButton(
            "update_numbers",
            "Zahlen aktualisieren!",
            class = "btn-lg btn-success"
          ),
          conditionalPanel(
            condition = "$('html').hasClass('shiny-busy')",
            tags$div("Zahlen werden aktualisiert...", id = "loadmessage")
          ),
        )
      ),
      fluidRow(
        column(8, textOutput("last_update"))
      ),
      fluidRow(
        column(8, textOutput("oldest_date"))
      ),
      tabsetPanel(
        tabPanel(
          "Wöchentliche Übersicht",
          div(style = "height:10px"),
          fluidRow(
            column(4, downloadButton("download_week"))
          ),
          div(style = "height:20px"),
          DT::dataTableOutput("table_week") %>% withSpinner()
        ),
        tabPanel(
          "Monatliche Übersicht",
          div(style = "height:10px"),
          fluidRow(
            column(4, downloadButton("download_month"))
          ),
          div(style = "height:20px"),
          DT::dataTableOutput("table_month") %>% withSpinner()
        ),
        tabPanel(
          "Tagesübersicht",
          div(style = "height:10px"),
          fluidRow(
            column(4, downloadButton("download_day"))
          ),
          div(style = "height:20px"),
          DT::dataTableOutput("table_day") %>% withSpinner()
        ),
        tabPanel(
          "Jahresübersicht",
          div(style = "height:10px"),
          fluidRow(
            column(4, downloadButton("download_year"))
          ),
          div(style = "height:20px"),
          DT::dataTableOutput("table_year") %>% withSpinner()
        ),
        tabPanel(
          "Herkunft",
          div(style = "height:10px"),
          fluidRow(
            column(4, downloadButton("download_herkunft"))
          ),
          div(style = "height:20px"),
          DT::dataTableOutput("table_herkunft") %>% withSpinner()
        )
      )
    ),

    ## Karten-Panel ------------------------------------------------------------
    tabPanel(
      "Karten",
      fluidRow(column(12, h4("Abbildungen:"))),
      fluidRow(
        column(
          4,
          selectInput(
            "graph_id",
            "Welcher Graph?",
            choices = list(
              "Deutschlandkarte (Anrufe)" = "karte_anrufe",
              "Deutschlandkarte (Anrufer*innen)" = "karte_anruferinnen",
              "Deutschlandkarte (Erstanrufer*innen)" = "karte_erstanruferinnen"
            )
          )
        ),
        column(8, dateRangeInput("period", "Zeitraum?", format = "yyyy-mm-dd"))
      ),
      fluidRow(
        column(4, downloadButton("download_plot"))
      ),
      fluidRow(column(
        12,
        h5(
          "Hinweis: Karten enthalten alle Anrufe, auch nicht zustandegekommene Verbindungen."
        )
      )),
      fluidRow(
        plotOutput("plot", width = "400px") %>% withSpinner()
      )
    ),

    ## Zeitreihen-Panel --------------------------------------------------------
    tabPanel(
      "Zeitreihen",
      fluidRow(column(12, h4("Abbildungen:"))),
      fluidRow(
        column(
          4,
          selectInput(
            "ts_bundesland",
            "Daten für welches Bundesland?",
            choices = list(
              "Alle" = "Alle",
              "Baden-Württemberg" = "Baden-Wuerttemberg",
              "Bayern" = "Bayern",
              "Berlin" = "Berlin",
              "Brandenburg" = "Brandenburg",
              "Bremen" = "Bremen",
              "Hamburg" = "Hamburg",
              "Hessen" = "Hessen",
              "Mecklenburg-Vorpommern" = "Mecklenburg-Vorpommern",
              "Niedersachsen" = "Niedersachsen",
              "Nordrhein-Westfalen" = "Nordrhein-Westfalen",
              "Rheinland-Pfalz" = "Rheinland-Pfalz",
              "Saarland" = "Saarland",
              "Sachsen" = "Sachsen",
              "Sachsen-Anhalt" = "Sachsen-Anhalt",
              "Schleswig-Holstein" = "Schleswig-Holstein",
              "Thüringen" = "Thueringen"
            )
          )
        ),
        column(
          8,
          dateRangeInput("ts_period", "Zeitraum?", format = "yyyy-mm-dd"),
          h5(
            "Hinweis: Es werden automatisch nur vollständig erfasste Kalenderwochen dargestellt."
          )
        )
      ),
      fluidRow(
        column(
          4,
          checkboxInput('ts_firstcall', "Nur Erstanrufer", value = FALSE)
        )
      ),
      fluidRow(
        column(
          4,
          checkboxInput(
            'ts_success',
            "Nur zustandegekommene Verbindungen",
            value = TRUE
          )
        )
      ),
      fluidRow(
        column(
          4,
          checkboxInput('ts_y_axis', "Starte Y-Achse bei 0", value = TRUE)
        )
      ),
      fluidRow(
        column(
          4,
          sliderInput(
            'ts_nr_breaks',
            "Häufigkeit der Datumsbeschriftungen",
            value = 5,
            min = 1,
            max = 20,
            step = 1,
            ticks = FALSE
          )
        )
      ),
      fluidRow(
        column(4, downloadButton("download_plot_ts"))
      ),
      fluidRow(
        plotOutput("plot_ts", width = "400px") %>% withSpinner()
      )
    )
  )
)

# Update-Function ---------------------------------------------------------
update_data <- function(data, path) {
  d <- download_data(
    "Numbers",
    start_date = max(data$date),
    end_date = Sys.Date(),
    create_firstcall_column = FALSE
  ) %>%
    add_geodata_to_numbers(geo = geo) %>%
    remove_redundant_cols() %>%
    hash_col() %>%
    mutate(date = lubridate::ymd(date))

  # create firstcall column in OLD data, if there is none yet, otherwise leave it alone
  if (!"firstcall" %in% names(data)) {
    data <- data %>%
      group_by(caller) %>%
      mutate(
        datetime = lubridate::ymd_hms(paste(date, time)),
        firstcall = datetime == min(datetime)
      ) %>%
      ungroup %>%
      select(-datetime)
  }

  # create firstcall column in NEW data
  d <- d %>%
    group_by(caller) %>%
    mutate(
      datetime = lubridate::ymd_hms(paste(date, time)),
      firstcall = ifelse(
        caller %in% unique(data$caller),
        FALSE,
        datetime == min(datetime)
      )
    ) %>%
    ungroup %>%
    select(-datetime)

  # create updated data from old data + new rows
  d <- rbind(data, d)
  d <- d[!duplicated(d), ]

  # write updated data
  write.csv(d, path, row.names = FALSE)
  return(d)
}


# Server-Function ---------------------------------------------------------
server <- function(input, output, session) {
  ## Initialise app with old data --------------------------------------------
  # Load currently present data when app started
  # TODO: NEED TO CHANGE THIS
  old_data <- current_data
  old_data$date <- lubridate::ymd(old_data$date)
  present_data <- reactiveValues(data = old_data)
  old_data <- NULL

  # Print last update and oldest date
  output$last_update <- renderText(paste(
    "Letzte Aktualisierung:",
    max(present_data$data$date),
    sep = " "
  ))

  output$oldest_date <- renderText(paste(
    "Beginn des Datensatzes (Bezugspunkt für Erstanrufe):",
    min(present_data$data$date),
    sep = " "
  ))

  ## Update-button -----------------------------------------------------------
  # Update numbers when button pressed:
  observeEvent(input$update_numbers, {
    present_data$data <- update_data(present_data$data, file)
    write.csv(present_data$data, file, row.names = FALSE) # overwrite old data
  })

  ## Daily table -------------------------------------------------------------
  # Build data and assign it to output
  table_day_Input <- function() {
    dplyr::arrange(
      create_provider_table(
        present_data$data,
        unit = "day",
        min(present_data$data$date),
        max(present_data$data$date)
      ),
      desc(Tag)
    )
  }
  output$table_day <- DT::renderDataTable(
    table_day_Input(),
    filter = "top",
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE
  )
  # Provide functionality to download the data as xlsx
  output$download_day <- downloadHandler(
    filename = function() {
      paste0(
        "tagestabelle_",
        min(table_day_Input()$Tag),
        "_",
        max(table_day_Input()$Tag),
        ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(table_day_Input(), file)
    }
  )

  ## Weekly Table ------------------------------------------------------------
  # Build table and assign it to output
  table_week_Input <- function() {
    dplyr::arrange(
      create_provider_table(
        present_data$data,
        unit = "week",
        min(present_data$data$date),
        max(present_data$data$date)
      ),
      desc(Start)
    )
  }

  output$table_week <- DT::renderDataTable(
    table_week_Input(),
    filter = list(position = "top", clear = TRUE, plain = TRUE),
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE
  )
  # Provide functionality to download the data as xlsx
  output$download_week <- downloadHandler(
    filename = function() {
      paste0(
        "wochentabelle_",
        min(table_week_Input()$Start),
        "_",
        max(table_week_Input()$Ende),
        ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(table_week_Input(), file)
    }
  )

  ## Monthly Table -----------------------------------------------------------
  # Build table and assign it to output
  table_month_Input <- function() {
    dplyr::arrange(
      create_provider_table(
        present_data$data,
        unit = "month",
        min(present_data$data$date),
        max(present_data$data$date)
      ),
      desc(Start)
    )
  }

  output$table_month <- DT::renderDataTable(
    table_month_Input(),
    filter = list(position = "top", clear = TRUE, plain = TRUE),
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE
  )
  # Provide functionality to download the data as xlsx
  output$download_month <- downloadHandler(
    filename = function() {
      paste0(
        "monatstabelle_",
        min(table_month_Input()$Start),
        "_",
        max(table_month_Input()$Ende),
        ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(table_month_Input(), file)
    }
  )

  ## Yearly table -------------------------------------------------------------
  # Build data and assign it to output
  table_year_Input <- function() {
    dplyr::arrange(
      create_provider_table(
        present_data$data,
        unit = "year",
        min(present_data$data$date),
        max(present_data$data$date)
      ),
      desc(Jahr)
    )
  }
  output$table_year <- DT::renderDataTable(
    table_year_Input(),
    filter = list(position = "top", clear = TRUE, plain = TRUE),
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE
  )
  # Provide functionality to download the data as xlsx
  output$download_year <- downloadHandler(
    filename = function() {
      paste0(
        "jahrestabelle_",
        min(table_year_Input()$Jahr),
        "_",
        max(table_year_Input()$Jahr),
        ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(table_year_Input(), file)
    }
  )

  # table herkunft
  table_herkunft_Input <- function() {
    create_herkunft_table(
      present_data$data,
      min(present_data$data$date),
      max(present_data$data$date)
    )
  }

  ## Table Herkunft ----------------------------------------------------------
  output$table_herkunft <- DT::renderDataTable(
    table_herkunft_Input(),
    filter = list(position = "top", clear = TRUE, plain = TRUE),
    options = list(pageLength = 15, scrollX = TRUE),
    rownames = FALSE
  )

  output$download_herkunft <- downloadHandler(
    filename = function() {
      paste0(
        "herkunftstabelle_",
        min(
          table_herkunft_Input() %>%
            filter(Jahr != "Gesamt") %>%
            pull(Start) %>%
            as.Date.character()
        ),
        "_",
        max(
          table_herkunft_Input() %>%
            filter(Jahr != "Gesamt") %>%
            pull(Ende) %>%
            as.Date.character()
        ),
        ".xlsx"
      )
    },
    content = function(file) {
      write.xlsx(table_herkunft_Input(), file)
    }
  )

  ## Plot functions for map --------------------------------------------------
  plotInput <- function() {
    if (input$graph_id == "karte_anrufe") {
      bundeslaender_map(
        present_data$data,
        bula_geo,
        variable = "Anrufe",
        start_date = input$period[1],
        end_date = input$period[2],
        colors = silbernetz_colors
      )
    } else if (input$graph_id == "karte_anruferinnen") {
      bundeslaender_map(
        present_data$data,
        bula_geo,
        variable = "Anrufer",
        start_date = input$period[1],
        end_date = input$period[2],
        colors = silbernetz_colors
      )
    } else {
      bundeslaender_map(
        present_data$data,
        bula_geo,
        variable = "Erstanrufer",
        start_date = input$period[1],
        end_date = input$period[2],
        colors = silbernetz_colors
      )
    }
  }

  ## Plot functions for time series ------------------------------------------
  plotInput_ts <- function() {
    plot_CallsOverTime(
      present_data$data,
      show_success = input$ts_success,
      show_firstcall = input$ts_firstcall,
      start_date = input$ts_period[1],
      end_date = input$ts_period[2],
      state = input$ts_bundesland,
      y_lim_zero = input$ts_y_axis,
      nr_breaks = input$ts_nr_breaks
    )
  }

  output$plot <- renderPlot(
    print(plotInput()),
    res = 120,
    height = 1000,
    width = 725
  )
  output$plot_ts <- renderPlot(
    print(plotInput_ts()),
    res = 120,
    height = 725,
    width = 1000
  )

  # Download handlers for both maps and time series plots
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$graph_id, "_", input$period[1], "_", input$period[2], ".png")
    },
    content = function(file) {
      device <- function(..., width, height, res) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = res,
          units = "px"
        )
      }
      ggplot2::ggsave(
        file,
        plot = plotInput(),
        device = device(width = 725, height = 1000, res = 120)
      )
    }
  )

  output$download_plot_ts <- downloadHandler(
    filename = function() {
      paste0(
        "Zeitreihe",
        "_",
        input$ts_period[1],
        "_",
        input$ts_period[2],
        ".png"
      )
    },
    content = function(file) {
      device <- function(..., width, height, res) {
        grDevices::png(
          ...,
          width = width,
          height = height,
          res = res,
          units = "px"
        )
      }
      ggplot2::ggsave(
        file,
        plot = plotInput_ts(),
        device = device(width = 1000, height = 725, res = 120)
      )
    }
  )
}

shinyApp(ui, server)

# runApp(list(
#   ui = fluidPage(downloadButton('foo')),
#   server = function(input, output) {
#     plotInput = function() {
#       qplot(speed, dist, data = cars)
#     }
#     output$foo = downloadHandler(
#       filename = 'test.png',
#       content = function(file) {
#         device <- function(..., width, height) {
#           grDevices::png(..., width = width, height = height,
#                          res = 300, units = "in")
#         }
#         ggsave(file, plot = plotInput(), device = device)
#       })
#   }
# ))
