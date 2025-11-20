# ==============================================================================
# Application Shiny - Analyse du Trafic Aerien NYC
# Aeroports de Paris (ADP)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)

# Chargement des donnees
data(flights)
data(airports)
data(airlines)
data(planes)
data(weather)

# ==============================================================================
# UI (Interface Utilisateur)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",

  # En-tete
  dashboardHeader(title = "Trafic Aerien NYC - ADP"),

  # Barre laterale
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vue d'ensemble", tabName = "overview", icon = icon("dashboard")),
      menuItem("Destinations", tabName = "destinations", icon = icon("plane-departure")),
      menuItem("Compagnies", tabName = "airlines", icon = icon("building")),
      menuItem("Vols", tabName = "flights", icon = icon("plane")),
      menuItem("Retards & Performance", tabName = "delays", icon = icon("clock")),
      menuItem("Donnees brutes", tabName = "data", icon = icon("table"))
    )
  ),

  # Contenu principal
  dashboardBody(
    tabItems(

      # ========================================================================
      # ONGLET 1: VUE D'ENSEMBLE
      # ========================================================================
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_flights"),
                valueBoxOutput("total_airlines"),
                valueBoxOutput("total_airports")
              ),
              fluidRow(
                valueBoxOutput("cancelled_flights"),
                valueBoxOutput("total_planes"),
                valueBoxOutput("avg_delay")
              ),
              fluidRow(
                box(
                  title = "Top 10 Destinations",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("top_destinations_plot")
                ),
                box(
                  title = "Vols par Aeroport d'Origine",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("flights_by_origin_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Distribution des Vols par Mois",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("flights_by_month_plot")
                )
              )
      ),

      # ========================================================================
      # ONGLET 2: DESTINATIONS
      # ========================================================================
      tabItem(tabName = "destinations",
              fluidRow(
                box(
                  title = "Filtres",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("dest_origin", "Aeroport d'origine:",
                              choices = c("Tous", sort(unique(flights$origin))),
                              selected = "Tous"),
                  selectInput("dest_carrier", "Compagnie:",
                              choices = c("Tous", sort(unique(flights$carrier))),
                              selected = "Tous")
                )
              ),
              fluidRow(
                box(
                  title = "Carte des Destinations",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("destinations_map", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Statistiques des Destinations",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("destinations_table")
                )
              )
      ),

      # ========================================================================
      # ONGLET 3: COMPAGNIES
      # ========================================================================
      tabItem(tabName = "airlines",
              fluidRow(
                box(
                  title = "Destinations par Compagnie",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("airlines_destinations_plot")
                ),
                box(
                  title = "Vols par Compagnie",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("airlines_flights_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Couverture des Compagnies",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("airlines_coverage_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Destinations Exclusives",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("exclusive_destinations_table")
                )
              )
      ),

      # ========================================================================
      # ONGLET 4: VOLS
      # ========================================================================
      tabItem(tabName = "flights",
              fluidRow(
                box(
                  title = "Recherche de Vols",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3, selectInput("flight_origin", "Origine:",
                                          choices = c("Tous", sort(unique(flights$origin))),
                                          selected = "Tous")),
                    column(3, selectInput("flight_dest", "Destination:",
                                          choices = c("Tous", sort(unique(flights$dest))),
                                          selected = "Tous")),
                    column(3, selectInput("flight_carrier", "Compagnie:",
                                          choices = c("Tous", sort(unique(flights$carrier))),
                                          selected = "Tous")),
                    column(3, selectInput("flight_month", "Mois:",
                                          choices = c("Tous", 1:12),
                                          selected = "Tous"))
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("filtered_flights_count"),
                valueBoxOutput("filtered_avg_delay"),
                valueBoxOutput("filtered_cancelled_count")
              ),
              fluidRow(
                box(
                  title = "Resultats de la Recherche",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("flights_search_table")
                )
              )
      ),

      # ========================================================================
      # ONGLET 5: RETARDS & PERFORMANCE
      # ========================================================================
      tabItem(tabName = "delays",
              fluidRow(
                box(
                  title = "Distribution des Retards au Depart",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("dep_delay_dist_plot")
                ),
                box(
                  title = "Distribution des Retards a l'Arrivee",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("arr_delay_dist_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Retard Moyen par Compagnie",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("delay_by_carrier_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Performance par Aeroport d'Origine",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("performance_by_origin_plot")
                )
              )
      ),

      # ========================================================================
      # ONGLET 6: DONNEES BRUTES
      # ========================================================================
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Selection de la Table",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput("data_table", "Choisir une table:",
                              choices = c("Flights" = "flights",
                                          "Airlines" = "airlines",
                                          "Airports" = "airports",
                                          "Planes" = "planes",
                                          "Weather" = "weather"),
                              selected = "flights")
                )
              ),
              fluidRow(
                box(
                  title = "Donnees",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("raw_data_table")
                )
              )
      )
    )
  )
)

# ==============================================================================
# SERVER (Logique de l'Application)
# ==============================================================================

server <- function(input, output, session) {

  # ============================================================================
  # VUE D'ENSEMBLE - Value Boxes
  # ============================================================================

  output$total_flights <- renderValueBox({
    valueBox(
      format(nrow(flights), big.mark = " "),
      "Total Vols",
      icon = icon("plane"),
      color = "blue"
    )
  })

  output$total_airlines <- renderValueBox({
    valueBox(
      nrow(airlines),
      "Compagnies",
      icon = icon("building"),
      color = "green"
    )
  })

  output$total_airports <- renderValueBox({
    n_airports <- flights %>%
      select(origin, dest) %>%
      pivot_longer(cols = everything()) %>%
      distinct(value) %>%
      nrow()

    valueBox(
      n_airports,
      "Aeroports",
      icon = icon("map-marker"),
      color = "purple"
    )
  })

  output$cancelled_flights <- renderValueBox({
    cancelled <- flights %>%
      filter(is.na(dep_time) | is.na(arr_time)) %>%
      nrow()

    valueBox(
      format(cancelled, big.mark = " "),
      "Vols Annules",
      icon = icon("times-circle"),
      color = "red"
    )
  })

  output$total_planes <- renderValueBox({
    valueBox(
      format(nrow(planes), big.mark = " "),
      "Avions",
      icon = icon("plane-departure"),
      color = "yellow"
    )
  })

  output$avg_delay <- renderValueBox({
    avg_delay <- flights %>%
      filter(!is.na(dep_delay)) %>%
      summarise(avg = round(mean(dep_delay), 1)) %>%
      pull(avg)

    valueBox(
      paste(avg_delay, "min"),
      "Retard Moyen",
      icon = icon("clock"),
      color = "orange"
    )
  })

  # ============================================================================
  # VUE D'ENSEMBLE - Graphiques
  # ============================================================================

  output$top_destinations_plot <- renderPlotly({
    top_dest <- flights %>%
      group_by(dest) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      left_join(airports, by = c("dest" = "faa"))

    plot_ly(top_dest, x = ~reorder(dest, n), y = ~n, type = "bar",
            marker = list(color = "steelblue"),
            text = ~paste(name, "<br>", format(n, big.mark = " "), "vols"),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Destination"),
             yaxis = list(title = "Nombre de vols"),
             margin = list(b = 100))
  })

  output$flights_by_origin_plot <- renderPlotly({
    by_origin <- flights %>%
      group_by(origin) %>%
      summarise(n = n()) %>%
      left_join(airports, by = c("origin" = "faa"))

    plot_ly(by_origin, labels = ~origin, values = ~n, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#3498db", "#2ecc71", "#e74c3c"))) %>%
      layout(title = "")
  })

  output$flights_by_month_plot <- renderPlotly({
    by_month <- flights %>%
      group_by(month) %>%
      summarise(n = n()) %>%
      arrange(month)

    month_names <- c("Jan", "Fev", "Mar", "Avr", "Mai", "Juin",
                     "Juil", "Aout", "Sep", "Oct", "Nov", "Dec")

    by_month$month_label <- month_names[by_month$month]

    plot_ly(by_month, x = ~month, y = ~n, type = "scatter",
            mode = "lines+markers",
            line = list(color = "steelblue", width = 3),
            marker = list(size = 8),
            text = ~paste(month_label, ":", format(n, big.mark = " "), "vols"),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Mois",
                          tickmode = "array",
                          tickvals = 1:12,
                          ticktext = month_names),
             yaxis = list(title = "Nombre de vols"))
  })

  # ============================================================================
  # DESTINATIONS
  # ============================================================================

  filtered_dest_data <- reactive({
    data <- flights

    if (input$dest_origin != "Tous") {
      data <- data %>% filter(origin == input$dest_origin)
    }

    if (input$dest_carrier != "Tous") {
      data <- data %>% filter(carrier == input$dest_carrier)
    }

    data
  })

  output$destinations_map <- renderPlotly({
    dest_data <- filtered_dest_data() %>%
      group_by(dest) %>%
      summarise(n = n()) %>%
      left_join(airports, by = c("dest" = "faa")) %>%
      filter(!is.na(lat) & !is.na(lon))

    plot_ly(dest_data, lon = ~lon, lat = ~lat, type = "scattergeo",
            mode = "markers",
            marker = list(size = ~sqrt(n)/5, color = "red", opacity = 0.7),
            text = ~paste(name, "<br>", format(n, big.mark = " "), "vols"),
            hoverinfo = "text") %>%
      layout(geo = list(scope = "usa",
                        projection = list(type = "albers usa"),
                        showland = TRUE,
                        landcolor = toRGB("gray95")))
  })

  output$destinations_table <- renderDT({
    filtered_dest_data() %>%
      group_by(dest) %>%
      summarise(nb_vols = n(),
                nb_compagnies = n_distinct(carrier),
                retard_moyen = round(mean(arr_delay, na.rm = TRUE), 1)) %>%
      left_join(airports, by = c("dest" = "faa")) %>%
      select(dest, name, nb_vols, nb_compagnies, retard_moyen) %>%
      arrange(desc(nb_vols)) %>%
      datatable(colnames = c("Code", "Nom", "Vols", "Compagnies", "Retard Moy."),
                options = list(pageLength = 10))
  })

  # ============================================================================
  # COMPAGNIES
  # ============================================================================

  output$airlines_destinations_plot <- renderPlotly({
    airline_dest <- flights %>%
      group_by(carrier) %>%
      summarise(nb_dest = n_distinct(dest)) %>%
      left_join(airlines, by = "carrier") %>%
      arrange(desc(nb_dest))

    plot_ly(airline_dest, x = ~reorder(carrier, -nb_dest), y = ~nb_dest,
            type = "bar", marker = list(color = "coral"),
            text = ~paste(name, "<br>", nb_dest, "destinations"),
            hoverinfo = "text") %>%
      layout(xaxis = list(title = "Compagnie"),
             yaxis = list(title = "Nombre de destinations"))
  })

  output$airlines_flights_plot <- renderPlotly({
    airline_flights <- flights %>%
      group_by(carrier) %>%
      summarise(nb_vols = n()) %>%
      left_join(airlines, by = "carrier") %>%
      arrange(desc(nb_vols))

    plot_ly(airline_flights, labels = ~carrier, values = ~nb_vols,
            type = "pie", textposition = "inside",
            textinfo = "label+percent",
            hovertext = ~paste(name, ":", format(nb_vols, big.mark = " "), "vols"),
            hoverinfo = "text")
  })

  output$airlines_coverage_plot <- renderPlotly({
    coverage <- flights %>%
      group_by(carrier, origin) %>%
      summarise(nb_dest = n_distinct(dest), .groups = "drop") %>%
      left_join(airlines, by = "carrier")

    plot_ly(coverage, x = ~carrier, y = ~nb_dest, color = ~origin,
            type = "bar", colors = c("#3498db", "#2ecc71", "#e74c3c")) %>%
      layout(xaxis = list(title = "Compagnie"),
             yaxis = list(title = "Nombre de destinations"),
             barmode = "group")
  })

  output$exclusive_destinations_table <- renderDT({
    flights %>%
      group_by(dest) %>%
      summarise(nb_compagnies = n_distinct(carrier),
                compagnie = first(carrier),
                nb_vols = n()) %>%
      filter(nb_compagnies == 1) %>%
      left_join(airports, by = c("dest" = "faa")) %>%
      left_join(airlines, by = c("compagnie" = "carrier")) %>%
      select(dest, name.x, compagnie, name.y, nb_vols) %>%
      arrange(desc(nb_vols)) %>%
      datatable(colnames = c("Code", "Destination", "Code Compagnie",
                             "Nom Compagnie", "Vols"),
                options = list(pageLength = 10))
  })

  # ============================================================================
  # VOLS - Recherche
  # ============================================================================

  filtered_flights_data <- reactive({
    data <- flights

    if (input$flight_origin != "Tous") {
      data <- data %>% filter(origin == input$flight_origin)
    }

    if (input$flight_dest != "Tous") {
      data <- data %>% filter(dest == input$flight_dest)
    }

    if (input$flight_carrier != "Tous") {
      data <- data %>% filter(carrier == input$flight_carrier)
    }

    if (input$flight_month != "Tous") {
      data <- data %>% filter(month == as.integer(input$flight_month))
    }

    data
  })

  output$filtered_flights_count <- renderValueBox({
    valueBox(
      format(nrow(filtered_flights_data()), big.mark = " "),
      "Vols Trouves",
      icon = icon("search"),
      color = "blue"
    )
  })

  output$filtered_avg_delay <- renderValueBox({
    avg <- filtered_flights_data() %>%
      filter(!is.na(dep_delay)) %>%
      summarise(avg = round(mean(dep_delay), 1)) %>%
      pull(avg)

    avg <- ifelse(length(avg) == 0, 0, avg)

    valueBox(
      paste(avg, "min"),
      "Retard Moyen",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$filtered_cancelled_count <- renderValueBox({
    cancelled <- filtered_flights_data() %>%
      filter(is.na(dep_time)) %>%
      nrow()

    valueBox(
      cancelled,
      "Vols Annules",
      icon = icon("times"),
      color = "red"
    )
  })

  output$flights_search_table <- renderDT({
    filtered_flights_data() %>%
      left_join(airlines, by = "carrier") %>%
      select(month, day, carrier, name, flight, origin, dest,
             dep_time, arr_time, dep_delay, arr_delay) %>%
      head(1000) %>%
      datatable(colnames = c("Mois", "Jour", "Code", "Compagnie", "Vol",
                             "Origine", "Dest", "Dep", "Arr",
                             "Retard Dep", "Retard Arr"),
                options = list(pageLength = 15, scrollX = TRUE))
  })

  # ============================================================================
  # RETARDS & PERFORMANCE
  # ============================================================================

  output$dep_delay_dist_plot <- renderPlotly({
    delay_data <- flights %>%
      filter(!is.na(dep_delay), dep_delay > -30, dep_delay < 300)

    plot_ly(x = ~delay_data$dep_delay, type = "histogram",
            marker = list(color = "steelblue"),
            nbinsx = 50) %>%
      layout(xaxis = list(title = "Retard au depart (min)"),
             yaxis = list(title = "Nombre de vols"))
  })

  output$arr_delay_dist_plot <- renderPlotly({
    delay_data <- flights %>%
      filter(!is.na(arr_delay), arr_delay > -30, arr_delay < 300)

    plot_ly(x = ~delay_data$arr_delay, type = "histogram",
            marker = list(color = "coral"),
            nbinsx = 50) %>%
      layout(xaxis = list(title = "Retard a l'arrivee (min)"),
             yaxis = list(title = "Nombre de vols"))
  })

  output$delay_by_carrier_plot <- renderPlotly({
    delay_carrier <- flights %>%
      filter(!is.na(dep_delay)) %>%
      group_by(carrier) %>%
      summarise(
        retard_moyen = mean(dep_delay),
        retard_median = median(dep_delay)
      ) %>%
      left_join(airlines, by = "carrier") %>%
      arrange(desc(retard_moyen))

    plot_ly(delay_carrier, x = ~reorder(carrier, -retard_moyen),
            y = ~retard_moyen, type = "bar", name = "Moyenne",
            marker = list(color = "tomato"),
            text = ~paste(name, "<br>Moyenne:", round(retard_moyen, 1), "min"),
            hoverinfo = "text") %>%
      add_trace(y = ~retard_median, name = "Mediane",
                marker = list(color = "steelblue"),
                text = ~paste(name, "<br>Mediane:", round(retard_median, 1), "min")) %>%
      layout(xaxis = list(title = "Compagnie"),
             yaxis = list(title = "Retard (min)"),
             barmode = "group")
  })

  output$performance_by_origin_plot <- renderPlotly({
    perf_origin <- flights %>%
      group_by(origin) %>%
      summarise(
        total_vols = n(),
        vols_annules = sum(is.na(dep_time)),
        taux_annulation = round(vols_annules / total_vols * 100, 2),
        retard_moyen = round(mean(dep_delay, na.rm = TRUE), 1)
      ) %>%
      left_join(airports, by = c("origin" = "faa"))

    plot_ly(perf_origin, x = ~origin, y = ~taux_annulation,
            type = "bar", name = "Taux annulation (%)",
            marker = list(color = "red")) %>%
      add_trace(y = ~retard_moyen, name = "Retard moyen (min)",
                marker = list(color = "orange"),
                yaxis = "y2") %>%
      layout(
        xaxis = list(title = "Aeroport"),
        yaxis = list(title = "Taux d'annulation (%)"),
        yaxis2 = list(overlaying = "y", side = "right", title = "Retard moyen (min)"),
        barmode = "group"
      )
  })

  # ============================================================================
  # DONNEES BRUTES
  # ============================================================================

  output$raw_data_table <- renderDT({
    data <- switch(input$data_table,
                   "flights" = flights %>% head(1000),
                   "airlines" = airlines,
                   "airports" = airports,
                   "planes" = planes,
                   "weather" = weather %>% head(1000))

    datatable(data, options = list(pageLength = 15, scrollX = TRUE))
  })
}

# ==============================================================================
# Lancement de l'Application
# ==============================================================================

shinyApp(ui = ui, server = server)
