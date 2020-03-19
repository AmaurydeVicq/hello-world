# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)

# Load in data
<<<<<<< HEAD

BE_DR <-  read.csv("./Data/BE_DeathRate.txt", sep="")
NL_DR <- read_excel("./Data/NL_DR.xlsx")
FR_DR <-  read.csv("./Data/FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("./Data/DE_DeathRate.txt", sep="")
USA_DR <-  read.csv("./Data/USA_DeathRate.txt", sep="")
=======
setwd("C:/Users/Amaur/Desktop/hello-world/Data")
BE_DR <-  read.csv("BE_DeathRate.txt", sep="")
NL_DR <- read_excel("NL_DR.xlsx")
FR_DR <-  read.csv("FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("DE_DeathRate.txt", sep="")
WDE_DR <-  read.csv("WestDE_DeathRate.txt", sep="")
USA_DR <-  read.csv("USA_DeathRate.txt", sep="")
SPC <- read_excel("SPC.xlsx")
AEX <- read_excel("AEX.xlsx")
>>>>>>> upstream/master

# Amend data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
WDE_DR$Year <- as.integer(WDE_DR$Year)
USA_DR$Year <- as.integer(DE_DR$Year)

<<<<<<< HEAD
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

#Graphique 

All %>%
  filter(Age == 40) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line()
=======
BE <- BE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
NL <- NL_DR %>% select(1,2,5) %>% filter(Year >= 1900)
FR <- FR_DR %>% select(1,2,5) %>% filter(Year >= 1900)
DE <- DE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
WDE <- WDE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
USA <- USA_DR %>% select(1,2,5) %>% filter(Year >= 1900)
>>>>>>> upstream/master

# Join datasets
BENL <- left_join(BE,NL, by = "Year")
FRWDE <- left_join(WDE, FR, by = "Year")
EU <- left_join(BENL,FRWDE, by = "Year") ## ik begrijp niet waarom R hier crashed! Kan je dit bekijken?
ALL <- full_join(EU,USA, by = "Year")