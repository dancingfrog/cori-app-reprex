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
           shinymaptool::mapOutput("map")
    )
  )
)

server <- function(input, output, session) {
  # data(quakes)

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[1:as.numeric(input$n),]
  })

  # The call to shinymapTool will happen in observer() *before* output$... <- function() {}
  # so we need to check init state:
  check_init <- reactiveValues(map_ready = 0)

  shiny::observe({
    message("Update points")

    quake_data <- filteredData()

    if (check_init$map_ready > 0) {

      shinymaptool::renderMapTool({
        shinymaptool::mapTool(
          map_id = paste0("map", "-map"),
          geo_geom = (function () {
            sf_data <- sf::st_as_sf(quake_data, coords = c("long", "lat"), crs = 4326, agr = "constant", remove = FALSE) # <= VERY IMPORTANT to keep lat, long columns
            json_result <- jsonlite::fromJSON(
              geojsonsf::sf_geojson(sf_data %>% dplyr::mutate(name = mag, lat = lat, lon = long)),
              simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
            json_result[["srid"]] <- 4326
            return(json_result)
          })()
        )
      })
    }
  })

  output$map <- shinymaptool::renderMapTool({
    shinymaptool::mapTool(

      title = "Shiny Map Tool",
      help_doc = "",
      map_id = paste0("map", "-map"),
      geo_geom = (function () {
        sf_data <- sf::st_as_sf(quakes[1:10,], coords = c("long", "lat"), crs = 4326, agr = "constant", remove = FALSE) # <= VERY IMPORTANT to keep lat, long columns
        json_result <- jsonlite::fromJSON(
          geojsonsf::sf_geojson(sf_data %>% dplyr::mutate(lat = lat, lon = long)),
          simplifyVector = FALSE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
        json_result[["srid"]] <- 4326

        if (check_init$map_ready < 1) {
          check_init$map_ready <- 1
        }

        return(json_result)
      })(),
      geo_table = list(
        mapbox = list(
          apiKey = "pk.eyJ1IjoicnVyYWxpbm5vIiwiYSI6ImNqeHl0cW0xODBlMm0zY2x0dXltYzRuazUifQ.zZBovoCHzLIW0wCZveEKzA", #Sys.getenv("MAPBOX_API_TOKEN"),
          basemaps = list(),
          init = list(
            longitude = 174.768,
            latitude = -36.852,
            zoom = 3,
            pitch = 5,
            bearing = 0
          )
        ),
        #filter_by_selection = TRUE,
        show_info_box = TRUE
      )
    )
  })
}

shiny::shinyApp(ui, server)
