library(shiny)
library(leaflet)
library(tidyverse)
library(readr)
library(lubridate)
library(RColorBrewer)
x <- c("ggmap", "rgdal", "rgeos", "maptools", "tmap")
lapply(x, library, character.only = TRUE)

#Preambule
Netherlands <- read_csv("../Data/Shiny_nl.csv") %>%
    select(-1)
Belgium <- read_csv("../Data/Shiny_be.csv") %>%
    select(-1)
France <- read_csv("../Data/Shiny_fr.csv") %>%
    select(-1)

colnames(Netherlands) <- c("NUTS_CODE", "Date", "montant")
countries <- rbind(Netherlands ,Belgium, France) 

Europe <- readOGR(layer = "NUTS_RG_03M_2016_4326_LEVL_1", dsn = "../Data")
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)

#User interface
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(bottom = 10, right = 10,
                  sliderInput("Date", "Select a Date", 
                              min = ymd("2020-03-01"),
                              max = today(),
                              value = ymd("2020-03-15")
                  )
                  )
)

server <- function(input, output, session) {
    
   
    # Reactive expression for the data subsetted to what the user selected
    filteredData <- reactive({
        #The reactive function
        countries2 <- countries[countries$Date == input$Date,]
        test <- sp::merge(Europe, countries2, by.x = "NUTS_ID", by.y = "NUTS_CODE", duplicateGeoms = TRUE) 
        
        #pal <- colorBin("viridis", domain = test$montant, bins = bins)
        test
    })
    
    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        m1 <- leaflet() %>%
            addProviderTiles("OpenStreetMap.Mapnik")  %>%
            setView(4.8945, 52.3667, zoom = 5) 
        })
    
    

    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe ({ 
        labels <- sprintf(
            "<strong>%s</strong><br/>%g Cas confirmés",
            filteredData()$NUTS_NAME, filteredData()$montant) %>% 
            lapply(htmltools::HTML)
        
        pal <- colorBin("viridis", domain = filteredData()$montant, bins = bins)
        
        
        leafletProxy("map") %>%
            clearShapes() %>%
        addPolygons(data = filteredData(),
                    fillColor = ~pal(montant), 
                        color = "purple",
                        fillOpacity = 0.7,
                        weight = 1,
                        highlight = highlightOptions(
                            weight = 5,
                            color = "#666",
                            fillOpacity = 0.05,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")
            )
      
    })
    
}

shinyApp(ui, server)
