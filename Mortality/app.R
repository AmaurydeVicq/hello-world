#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)
library(shiny)

# Load in data
BE_DR <-  read.csv("./Data/BE_DeathRate.txt", sep="")
NL_DR <- read_excel("./Data/NL_DR.xlsx")
FR_DR <-  read.csv("./Data/FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("./Data/DE_DeathRate.txt", sep="")
USA_DR <-  read.csv("./Data/USA_DeathRate.txt", sep="")


BE <- BE_DR %>% 
    select(1,2,5) %>%
    mutate(country = "Belgium", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))
NL <- NL_DR %>%
    select(1,2,5) %>%
    mutate(country = "Netherlands", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))
FR <- FR_DR %>% 
    select(1,2,5) %>%
    mutate(country = "France", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))
DE <- DE_DR %>%
    select(1,2,5) %>%
    mutate(country = "Germany", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))
USA <- USA_DR %>% 
    select(1,2,5) %>%
    mutate(country = "United States", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))

All <- rbind(BE,NL,FR,DE,USA)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mortality in different countries conditional on Age"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Age",
                        "Mortality conditional on age",
                        min = min(All$Age, na.rm = TRUE),
                        max = max(All$Age, na.rm = TRUE),
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    filteredData <- reactive({
        
        All <- All %>%
            filter(Age == input$Age)
        All
        
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- filteredData()
        
        # draw the histogram with the specified number of bins
       ggplot(x, aes(x = Year, y = Total, color = country, group = country)) + geom_line()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
