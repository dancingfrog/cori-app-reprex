library(leaflet)
library(magrittr)
library(shiny)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  absolutePanel(top = 10, right = 10,
  # fluidRow(
  #   column(9,
  #          tags$span("")
  #   ),
    column(3, class = "number-of-points",
             selectInput("n", "Number of Points", 1:100),
             tags$style(type = "text/css", ".number-of-points { margin-top: 10px; min-width: 200px; z-index: 1000 }")
    )
  # ),
  ),
  # fluidRow(
  #   column(12,
        leafletOutput("map", width = "100%", height = "100%")
  #   )
  # )
)

server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[1:as.numeric(input$n),]
  })

  observe({

    awesomeIcons <- leaflet::awesomeIcons(
      icon = 'circle',
      iconColor = "black",
      markerColor = "red",
      library = "fa"
    )

    leafletProxy("map", data = filteredData()) %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R", icons <- awesomeIcons) %>%
      leaflet::addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag), icons <- awesomeIcons)
  })

  output$map <- renderLeaflet({
    leaflet(quakes) %>%
      leaflet::addTiles() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
}

shiny::shinyApp(ui, server)
