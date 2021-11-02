library(magrittr)

ui <- shiny::bootstrapPage(
  shiny::tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # shiny::absolutePanel(top = 10, right = 10,
  shiny::fluidRow(
    shiny::column(9,
           shiny::tags$span("")
    ),
    shiny::column(3, class = "number-of-points",
           shiny::selectInput("n", "Number of Points", 1:100, selected = 10),
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

  awesome_icons <- leaflet::makeAwesomeIcon(
    icon = 'circle',
    iconColor = "black",
    markerColor = "red",
    library = "fa",
    fontFamily = "monospace",
    text = "R"
  )

  getColor <- function(quakes) {
    sapply(quakes$mag, function(mag) {
      if(mag <= 4) {
        "green"
      } else if(mag <= 5) {
        "orange"
      } else {
        "blue"
      } })
  }

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[1:as.numeric(input$n),]
  })

  shiny::observe({
    message("Update points")

    icon_colors <- getColor(filteredData())

    quake_icons <- leaflet::awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = icon_colors
    )

    leaflet::leafletProxy("map", data = filteredData()) %>%
      leaflet::clearShapes() %>%
      leaflet::clearMarkers() %>%
      # leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      leaflet::addAwesomeMarkers(~long, ~lat, icon = quake_icons, label = ~as.character(mag)) %>%
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, icon = awesome_icons, label = ~as.character(mag), popup="The birthplace of R")
  })

  output$map <- leaflet::renderLeaflet({
    # Show user selected rows from the `quakes` dataset
    leaflet::leaflet(quakes) %>%
      leaflet::addTiles() %>%
      leaflet::fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      # Note: sometimes the order of params = args, ... actually matters, as in the case of this call to addAwesomeMarkers
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, icon = awesome_icons, label = ~as.character(mag), popup="The birthplace of R")
  })
}

shiny::shinyApp(ui, server)
