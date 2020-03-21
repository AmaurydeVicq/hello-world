# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)
library(tidyr)
library(readr)
library(lubridate)
library(scales)
library(shiny)

# Load in data
setwd("../Data")
SPC <- read_excel("SPC.xlsx")

# Amend other data
SPC2 <- SPC%>%
  separate(Date, c("Year", "Month"))

SPC3 <- SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  mutate(Year = as.numeric(Year))

# Flus & countries
SpanishFlu <- seq(1900,1930,1)
AsianFlu <- seq(1950,1965,1)
HongKongFlu <- seq(1960,1970,1)
RussianFlu <-seq(1970,1980,1)
SwineFlu <- seq(2000,2010,1)

# Shiny
ui <- fluidPage(
  titlePanel("S&P Completion Index"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("PAN", 
                  label = "Choose a Pandemic",
                  choices = c("Spanish Flu", 
                              "Asian Flu",
                              "Hong Kong Flu", 
                              "Russian Flu"),
                  selected = "Spanish Flu"
      )
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server logic
server <- function(input, output, session) {

# Hier maken we een functie van jouw namen naar de dataset van jaren
  datasetInput <- reactive({
    switch(input$PAN,
           "Spanish Flu" = SpanishFlu,
           "Asian Flu" = AsianFlu,
           "Hong Kong Flu" = HongKongFlu,
           "Russian Flu" = RussianFlu)
  })
  
# Hier maken we een functie van jouw namen naar de uiteinden van geom_vline()
  
  geomlineInput <- reactive({
    switch(input$PAN,
           "Spanish Flu" = c(ymd("1918-01-01"), ymd("1920-01-01")),
           "Asian Flu" = c(ymd("1954-01-01"), ymd("1956-01-01")), #Je moet zelf even de dates veranderen
           "Hong Kong Flu" = c(ymd("1962-01-01"), ymd("1964-01-01")),
           "Russian Flu" = c(ymd("1971-01-01"), ymd("1973-01-01"))
    )
    })
  
# Hier gaan we de data filteren als functie van jouw argumenten
filteredData <- reactive({
  SPC3[SPC3$Year %in% datasetInput(),]
})
  

# output?  
output$plot<-renderPlot({
  ggplot(data = filteredData(),aes(x=date,y=`Real Price`)) + 
    geom_line() + 
    scale_x_date(date_breaks = "5 year", date_labels = "%b %Y") +
    geom_vline(xintercept = geomlineInput()[1]) +
    geom_vline(xintercept = geomlineInput()[2])
  })
}


# launch app
shinyApp(ui, server)
