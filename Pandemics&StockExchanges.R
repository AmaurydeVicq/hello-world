# Effects of Pandemics on Financial Crises

# Load packages
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)

# Load in data

BE_DR <-  read.csv("C:/Users/Amaur/Desktop/hello-world/Data/BE_DeathRate.txt", sep="")
NL_DR <- read_excel("C:/Users/Amaur/Desktop/hello-world/Data/NL_DR.xlsx")
FR_DR <-  read.csv("C:/Users/Amaur/Desktop/hello-world/Data/FR_DeathRate.txt", sep="")
DE_DR <-  read.csv("C:/Users/Amaur/Desktop/hello-world/Data/DE_DeathRate.txt", sep="")
USA_DR <-  read.csv("C:/Users/Amaur/Desktop/hello-world/Data/USA_DeathRate.txt", sep="")

# Amend data
BE_DR$Year <- as.integer(BE_DR$Year) 
NL_DR$Year <- as.integer(NL_DR$Year)
FR_DR$Year <- as.integer(FR_DR$Year)
DE_DR$Year <- as.integer(DE_DR$Year)
USA_DR$Year <- as.integer(DE_DR$Year)

BE <- BE_DR %>% select(1,2,5)
NL <- NL_DR %>% select(1,2,5)
FR <- FR_DR %>% select(1,2,5)
DE <- DE_DR %>% select(1,2,5)
USA <- USA_DR %>% select(1,2,5)

# Join datasets
BENL <- full_join(BE,NL, by = "Year")
FRDE <- full_join(DE, FR, by = "Year")
EU <- full_join(BENL,FRDE, by = "Year")
ALL <- full_join(EU,USA, by = "Year")