library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(shinyjs)  # Importar la biblioteca shinyjs

# Leer los datos desde un archivo CSV
datos_empleo_genero <- read_csv("datos/datos_empleo_genero.csv")

# UI
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "Des(empleo) y Género en América Latina y el Caribe",
    titleWidth = 500
  ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem(h4("Introducción"), tabName = "introduccion", icon = icon("person", class = "fa-2x", style = "color: #6930c3;")),
      menuItem(h4("Tabla"), tabName = "tabla", icon = icon("table", class = "fa-2x", style = "color: #6930c3;")),
      menuItem(h4("Histograma"), tabName = "histograma", icon = icon("chart-column", class = "fa-2x", style = "color: #6930c3;")),
      menuItem(h4("Barras"), tabName = "barras", icon = icon("chart-bar", class = "fa-2x", style = "color: #6930c3;")),
      menuItem(h4("Líneas"), tabName = "lineas", icon = icon("chart-line", class = "fa-2x", style = "color: #6930c3;"))
    )
  ),
  dashboardBody(
    useShinyjs(),  # Inicializar shinyjs
    tabItems(
      tabItem(tabName = "introduccion",
              p(h4(br(), br(), "En esta aplicación se puede visualizar alguna información sobre datos de des(empleo) y género para algunos países de Latinoamérica y el Caribe, entre los años 1970 - 2018.", br(), br(), "Fuente de los datos: Banco Mundial."))
      ),
      tabItem("tabla",
              fluidRow(
                column(3,
                       selectInput("variable_seleccionada",
                                   label = "Seleccione las variables deseadas:",
                                   choices = colnames(datos_empleo_genero),
                                   multiple = TRUE
                       )
                ),
                column(5,
                       sliderInput("anyo",
                                   label = "Seleccione el año",
                                   value = c(1970, 2018),
                                   min = min(unique(datos_empleo_genero$anyo)),
                                   max = max(unique(datos_empleo_genero$anyo))
                       )
                ),
                column(4),
                actionButton("tabular", "Filtrar datos", icon = icon("filter", lib = "font-awesome", style = "color: #6930c3;", class = "fa-2x")),
                div(style = "margin-bottom: 10px;"),
                dataTableOutput("dataTable")
              )
      ),
      tabItem("histograma",
              selectInput("variable_histograma",
                          label = "Seleccione una variable continua:",
                          choices = c(colnames(datos_empleo_genero[4:23])),
                          multiple = FALSE
              ),
              actionButton("graficar_histograma", "Mostrar histograma", icon = icon("arrows-rotate", lib = "font-awesome", style = "color: #6930c3;", class = "fa-2x")),
              div(style = "margin-bottom: 20px;"),
              plotOutput("histogramPlot")
      ),
      tabItem("barras",
              selectInput("variable_barras",
                          label = "Seleccione una variable categórica:",
                          choices = c(colnames(datos_empleo_genero[1:3])),
                          multiple = FALSE
              ),
              actionButton("graficar_barras", "Mostrar gráfico de barras", icon = icon("arrows-rotate", lib = "font-awesome", style = "color: #6930c3;", class = "fa-2x")),
              div(style = "margin-bottom: 20px;"),
              plotOutput("barrasPlot")
      ),
      tabItem("lineas",
              selectInput("variable_lineas",
                          label = "Seleccione una variable continua:",
                          choices = c(colnames(datos_empleo_genero[4:23])),
                          multiple = FALSE
              ),
              actionButton("graficar_lineas", "Mostrar gráfico de líneas", icon = icon("arrows-rotate", lib = "font-awesome", style = "color: #6930c3;", class = "fa-2x")),
              div(style = "margin-bottom: 20px;"),
              plotOutput("lineasPlot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  datos_empleo_genero <- read_csv("datos/datos_empleo_genero.csv")
  
  observe({
    # Aplicar estilo CSS al sliderInput utilizando shinyjs
    shinyjs::runjs("$('.irs-bar').css('background', '#421e86');")
  })
  
  # Tabla
  tabla_datos <- eventReactive(input$tabular, {
    selected_cols <- input$variable_seleccionada
    selected_year <- input$anyo
    datos_empleo_genero %>%
      filter(anyo >= selected_year[1] & anyo <= selected_year[2]) %>%
      select(all_of(selected_cols))
  })
  output$dataTable <- renderDataTable({
    tabla_datos()
  })
  
  # Histograma
  histogram_data <- eventReactive(input$graficar_histograma, {
    selected_variable <- input$variable_histograma
    data_to_plot <- datos_empleo_genero %>%
      filter(!is.na(.data[[selected_variable]]))
    
    ggplot(data = data_to_plot, aes(x = .data[[selected_variable]])) +
      geom_histogram(fill = "#421e86") +
      theme_minimal() +
      labs(
        title = "Distribución de la variable seleccionada",
        subtitle = "(Datos para América Latina y el Caribe)",
        caption = "Fuente: Banco Mundial, 1970-2018"
      ) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  output$histogramPlot <- renderPlot({
    histogram_data()
  })
  
  # Barras (horizontal)
  grafico_barras <- eventReactive(input$graficar_barras, {
    variable_seleccionada <- input$variable_barras
    datos_filtrados <- datos_empleo_genero %>%
      filter(!is.na(.data[[variable_seleccionada]]))
    
    ggplot(datos_filtrados, aes(y = .data[[variable_seleccionada]])) +
      geom_bar(fill = "#421e86", stat = "count") +
      labs(title = "Gráfico de Barras Horizontales",
           x = "Conteo",
           y = variable_seleccionada,
           subtitle = "(Datos para América Latina y el Caribe)",
           caption = "Fuente: Banco Mundial, 1970-2018"
      ) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  output$barrasPlot <- renderPlot({
    grafico_barras()
  })
  
  # Líneas con geom_smooth
  grafico_lineas <- eventReactive(input$graficar_lineas, {
    variable_seleccionada <- input$variable_lineas
    datos_filtrados <- datos_empleo_genero %>%
      filter(!is.na(.data[[variable_seleccionada]]))
    
    ggplot(datos_filtrados, aes(x = anyo, y = .data[[variable_seleccionada]])) +
      geom_line() +
      geom_smooth(method = "lm", se = FALSE, color = "#421e86") +
      labs(title = "Gráfico de Líneas",
           x = "Año",
           y = variable_seleccionada,
           subtitle = "(Datos para América Latina y el Caribe)",
           caption = "Fuente: Banco Mundial, 1970-2018"
      ) +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })
  output$lineasPlot <- renderPlot({
    grafico_lineas()
  })
}

# Ejecutamos la aplicación Shiny
shinyApp(ui, server)
