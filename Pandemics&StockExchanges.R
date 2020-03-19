# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)

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

# Amend data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
WDE_DR$Year <- as.integer(WDE_DR$Year)
USA_DR$Year <- as.integer(DE_DR$Year)

BE <- BE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
NL <- NL_DR %>% select(1,2,5) %>% filter(Year >= 1900)
FR <- FR_DR %>% select(1,2,5) %>% filter(Year >= 1900)
DE <- DE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
WDE <- WDE_DR %>% select(1,2,5) %>% filter(Year >= 1900)
USA <- USA_DR %>% select(1,2,5) %>% filter(Year >= 1900)

# Join datasets
BENL <- left_join(BE,NL, by = "Year")
FRWDE <- left_join(WDE, FR, by = "Year")
EU <- left_join(BENL,FRWDE, by = "Year") ## ik begrijp niet waarom R hier crashed! Kan je dit bekijken?
ALL <- full_join(EU,USA, by = "Year")