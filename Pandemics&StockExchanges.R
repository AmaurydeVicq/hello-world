# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)
library(ggplot2)
library(tidyr)
library(readr)

# Load in data
setwd("C:/Users/Amaur/Desktop/hello-world/Data")
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

# Amend Mortality data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
WDE_DR$Year <- as.integer(WDE_DR$Year)
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

WDE <- WDE_DR %>%
  select(1,2,5) %>%
  mutate(country = "West-Germany", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))

USA <- USA_DR %>% 
  select(1,2,5) %>%
  mutate(country = "United States", Age = as.numeric(as.character(Age)), Total = as.numeric(as.character(Total)))

All <- rbind(BE,NL,FR,DE, WDE, USA)
BENEFRUSA <- rbind(BE,NL,FR,USA)


# Amend other data
GDPC2 <- GDPC %>% 
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "GDP per Capita") %>%
  filter(year >= 1900) 
GDPC2 <- GDPC2[-1,]
               
# Plots
All %>%
  filter(Age == 40) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() 

BENEFRUSA %>%
  filter(Age == 60) %>%
  filter(Year >= 1920) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + facet_wrap(~ country)
BENEFRUSA

GDPC2 %>%
  ggplot(aes(x = year, y = `GDP per Capita`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`) + scale_x_discrete(c1900,2000,100)
BENEFRUS



