# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)

# Load in data

BE_DR <-  read.csv("./Data/BE_DeathRate.txt", sep="")
NL_DR <- read_excel("./Data/NL_DR.xlsx")
FR_DR <-  read.csv("./Data/FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("./Data/DE_DeathRate.txt", sep="")
USA_DR <-  read.csv("./Data/USA_DeathRate.txt", sep="")

# Amend data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
USA_DR$Year <- as.integer(DE_DR$Year)

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

# Join datasets
BENL <- full_join(BE,NL, by = "Year")
FRDE <- full_join(DE, FR, by = "Year")
EU <- full_join(BENL,FRDE, by = "Year")
ALL <- full_join(EU,USA, by = "Year")