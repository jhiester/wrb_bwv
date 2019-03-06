library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(RColorBrewer)


data <- read.csv("data_all.csv", header = TRUE)
wrb_area <- readOGR("wrb.shp", layer = "wrb", GDAL1_integer64_policy = TRUE)
wrb_area_transform <- spTransform(wrb_area, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

lat_lon <- data.frame(data$lon, data$lat)
lat_lon <- SpatialPoints(lat_lon, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
in_basin <- over(x = lat_lon, y = wrb_area_transform)

server <- function(input, output, session) {


  filteredData <- reactive({
    if(input$basinToggle){
      data.frame(data[!is.na(in_basin[,1]),])
    } else if(!input$basinToggle){
      data
    }
  })
  
  selectedData <- reactive({
    if(input$type_dropDown != "A"){
      subset(filteredData(), Type == input$type_dropDown)
    } else if (input$type_dropDown == "A"){
      filteredData()
    }
  })
  
  output$dataTable <- renderDataTable(selectedData()[,c(1:3)], options = list(pageLength = 5))
  
  output$map <- renderLeaflet({
      leaflet(selectedData()) %>% addProviderTiles("Thunderforest.Outdoors") %>%
        addPolygons(data = wrb_area_transform, weight = 2, fillColor = "yellow") %>%
        addCircleMarkers(lng = subset(selectedData(), Type == "B")$lon, lat = subset(selectedData(), Type == "B")$lat, color = brewer.pal(8,"Dark2")[6], popup = ~Name) %>%
        addCircleMarkers(lng = subset(selectedData(), Type == "W")$lon, lat = subset(selectedData(), Type == "W")$lat, color = "purple", popup = ~Name) %>%
        setView(-122.522409, 44.585929, zoom = 8)
    })
}    


ui <- fluidPage(theme=shinytheme("spacelab"),
                titlePanel("Willamette River Basin Breweries and Wineries"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    selectInput("type_dropDown", "Type", c("All" = "A", "Breweries" = "B", "Wineries and Vineyards" = "W")),
                    checkboxInput("basinToggle", label = "Filter to basin", value = TRUE),
                    dataTableOutput("dataTable")
                  ),
                  
                  
                  mainPanel(
                    leafletOutput("map", width = 1024, height = 768)
                  )
                )
)

shinyApp(ui = ui, server = server)

