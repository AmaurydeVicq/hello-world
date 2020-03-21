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
setwd("C:/Users/Amaur/Desktop/hello-world/Data")
SPC <- read_excel("SPC.xlsx")

# Amend other data
SPC2 <- SPC%>%
  separate(Date, c("Year", "Month"))

SPC3 <- SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date))


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
server <- function(input, output) {

# input?  
    
# output?  
output$plot<-renderPlot({
  ggplot(SPC3,aes(x=date,y=`Real Price`)) + geom_line() + scale_x_date(date_breaks = "10 year", date_labels = "%b %Y")}

# launch app
shinyApp(ui, server)
