library(magrittr)

ui <- shiny::bootstrapPage(
  shiny::tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # shiny::absolutePanel(top = 10, right = 10,
  shiny::fluidRow(
    shiny::column(9,
           shiny::tags$span("")
    ),
    shiny::column(3, class = "number-of-points",
           shiny::selectInput("n", "N", 1:100),
                  shiny::tags$style(type = "text/css", ".number-of-points { margin-top: 10px; min-width: 200px; z-index: 1000 }")
    )
  ),
  # ),
  shiny::fluidRow(
    shiny::column(12,
      leaflet::leafletOutput("map", width = "100%")
    )
  )
)

server <- function(input, output, session) {
  # data(quakes)

  awesome_icons <- leaflet::awesomeIcons(
    icon = 'circle',
    iconColor = "black",
    markerColor = "red",
    library = "fa"
  )

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[1:as.numeric(input$n),]
  })

  shiny::observe({
    message("Update points");

    leaflet::leafletProxy("map", data = filteredData()) %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      leaflet::addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag), icons <- awesome_icons) %>%
      leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R", icons <- awesome_icons)
  })

  output$map <- leaflet::renderLeaflet({
    # Show user selected rows from the `quakes` dataset
    leaflet::leaflet(quakes) %>%
      leaflet::addTiles() %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R", icons <- awesome_icons)
  })
}

shiny::shinyApp(ui, server)
