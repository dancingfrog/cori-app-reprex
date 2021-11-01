library(magrittr)
library(shiny)

ui <- shiny::fluidPage(
  shiny::selectInput("n", "N", 1:100),
  leaflet::leafletOutput("mymap")
)

server <- function(input, output, session) {
  data(quakes)

  output$mymap <- leaflet::renderLeaflet({
    # Show user selected rows from the `quakes` dataset
    m <- leaflet::leaflet(data = quakes[1:as.numeric(input$n),]) %>%
      leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles %>%
      leaflet::addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag)) %>%
      leaflet::addAwesomeMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R", icons <- leaflet::makeAwesomeIcon(
        iconColor = "black",
        markerColor = "red",
        library = "fa"
      ))
    m  # Print the map
  })
}

shiny::shinyApp(ui, server)

