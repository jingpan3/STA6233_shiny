library(shiny)
library(leaflet)
library(leaflet.extras)
library(rsconnect)
library(ggplot2)
load(url("https://raw.githubusercontent.com/jingpan3/STA6233_shiny/master/data/wine_data.RData"))
ui <- fluidPage(
  titlePanel("Winery Location"),
  leafletOutput("wine_map"),
  
  titlePanel("Wine Heatmap"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "intensity", 
                  label = "Intensity:",
                  choices = c("price","points"))
    ),
    mainPanel(
      leafletOutput("wine_heat_map") 
    )
  ),
  
  titlePanel("Wine Manufacturer"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "manu", 
                  label = "Manufacture:",
                  choices = c("Imported","Domestic"),
                  selected = "Imported")
    ),
    mainPanel(
      plotOutput(outputId = "wine_import")
    )
  ))
server <- function(input, output, session) {
  output$wine_map <- renderLeaflet({
    leaflet(data = wine_data) %>% addTiles() %>%
      addCircles(
        ~ lon,
        ~ lat,
      )
  })
  
  output$wine_heat_map <- renderLeaflet({
    leaflet(data = wine_data) %>% addTiles() %>% addHeatmap(lng = ~lon, lat = ~lat, intensity = ~input$intensity,
                                                            blur = 20, max = 0.05, radius = 15)
  })
  
  wine_import_data = reactive({
    if(input$manu == "Imported"){
      data = wine_data[wine_data$imported,]
    }
    else{
      data = wine_data[!wine_data$imported,]
    }
    
    return(data)
  })
  
  output$wine_import <- renderPlot({
    # draw the histogram with the specified number of bins
    ggplot(wine_import_data(), aes_string(x=wine_import_data()$points, y=wine_import_data()$price)) + geom_point() + xlab("Rating") + ylab("Price") #Notice the difference between the ggplots
  })
  
}

shinyApp(ui, server)
