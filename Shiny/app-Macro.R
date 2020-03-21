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
BE_DR <-  read.csv("BE_DeathRate.txt", sep="")
NL_DR <- read_excel("NL_DR.xlsx")
FR_DR <-  read.csv("FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("DE_DeathRate.txt", sep="")
WDE_DR <-  read.csv("WestDE_DeathRate.txt", sep="")
USA_DR <-  read.csv("USA_DeathRate.txt", sep="")
SPC <- read_excel("SPC.xlsx")
AEX <- read_excel("AEX.xlsx")
GDPC <- read_excel("GDPperCapita.xlsx")
Life <- read_excel("Life.xlsx")
BondYield <- read_excel("BondYield.xlsx")
RealWage <- read_excel("RealWage.xlsx")
Debt <- read_excel("Debt.xlsx")
CO2 <- read_excel("CO2.xlsx")

# Flus & countries
SpanishFlu <- seq(1900,1930,1)
AsianFlu <- seq(1950,1965,1)
HongKongFlu <- seq(1960,1970,1)
RussianFlu <-seq(1970,1980,1)
SwineFlu <- seq(2000,2010,1)
countries <- c("Belgium", "Netherlands", "France", "United Kingdom", "United States", "China", "Germany", "Italy")

# Amend Mortality data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
WDE_DR$Year <- as.integer(WDE_DR$Year)
USA_DR$Year <- as.integer(USA_DR$Year)

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

WDE <- WDE_DR %>%
  select(1,2,5) %>%
  mutate(country = "West-Germany", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))

USA <- USA_DR %>% 
  select(1,2,5) %>%
  mutate(country = "United States", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))

All <- rbind(BE,NL,FR,DE, WDE, USA)
All <- na.omit(All)

# Amend other data
GDPC2 <- GDPC %>% 
  pivot_longer(-`country name`,names_to = "year", values_to = "GDP per Capita") %>%
  filter(year >= 1900) 
GDPC2 <- GDPC2[-1,]
GDPC2 <- GDPC2 %>%
  mutate(year = as.numeric(year))

RealWage2 <- RealWage %>%
  pivot_longer(-`country name`, names_to = "year", values_to = "Real Wage") %>%
  filter(year >= 1900)
RealWage2 <- RealWage2[-1,]  
RealWage2 <- na.omit(RealWage2)
RealWage2 <- RealWage2 %>%
  mutate(year = as.numeric(year))

Life2 <- Life %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "LifeEx") %>%
  filter(year >= 1900)
Life2 <- Life2[-1,]  
Life2 <- na.omit(Life2)
Life2 <- Life2 %>%
  mutate(year = as.numeric(year))

CO2 <- CO2 %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "CO2") %>%
  filter(year >= 1900)
CO2 <- CO2[-1,] 
CO2 <- na.omit(CO2)
CO2 <- CO2 %>%
  mutate(year = as.numeric(year))

# De colname van elke variabele moet hetzelfde heten
colnames(Life2)[3] <- "variable"
colnames(CO2)[3] <- "variable"
colnames(RealWage2)[3] <- "variable"
colnames(GDPC2)[3] <- "variable"

# Shiny
ui <- fluidPage(
  titlePanel("Selected variables in a pandemic"),
  sidebarLayout(
    sidebarPanel(
      selectInput("PAN", 
                  label = "Choose a Pandemic",
                  choices = c("Spanish Flu", 
                              "Asian Flu",
                              "Hong Kong Flu", 
                              "Russian Flu"),
                  selected = "Spanish Flu"),
                  
    selectInput("CT", 
               label = "Choose a country",
                              choices = c("Belgium", 
                                          "Netherlands",
                                          "Germany", 
                                          "Italy",
                                          "United Kingdom",
                                          "United States",
                                          "China"),
                              selected = "Belgium"),
    selectInput("VAR",
                label = "Choose a variable",
                choices = c("GDP per Capita",
                            "Real wages",
                            "Life expectancy",
                            "CO2 emissions"
                            ))
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server logic
server <- function(input, output, session) {
  
# Hier maken we een functie van de namen naar de dataset van jaren
datasetInput1 <- reactive({
    switch(input$PAN,
           "Spanish Flu" = SpanishFlu,
           "Asian Flu" = AsianFlu,
           "Hong Kong Flu" = HongKongFlu,
           "Russian Flu" = RussianFlu)
  })
  
# Hier maken we een functie van de namen van landen (HIER LOOP IK VAST)
datasetInput2 <- reactive({
  switch(input$VAR,
         "GDP per Capita" = GDPC2,
         "Real wages" = RealWage2,
         "Life expectancy" = Life2,
         "CO2 emissions" = CO2)
})
  
  
# Hier gaan we de data filteren als functie van de argumenten 
filteredData <- reactive({
    datasetInput2()[datasetInput2()$year %in% datasetInput1() & datasetInput2()$`country name`== input$CT,]
  })
  
# output?  
  output$plot<-renderPlot({
    ggplot(data = filteredData(),aes(x=year,y=variable)) + 
      geom_line() + ggtitle(paste(input$VAR,"over time"))
  })
}


# launch app
shinyApp(ui, server)
