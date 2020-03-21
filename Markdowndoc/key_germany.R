library(fuzzyjoin)
library(tidyverse)
library(lubridate)

x <- c("ggmap", "rgdal", "rgeos", "maptools", "tmap")
lapply(x, library, character.only = TRUE)

Germany <- read.csv("../Data/DE_Confirmed.csv", sep = ";") %>%
  select(-4) %>%
  mutate(dag = dmy(as.character(dag)), region = toupper(as.character(region))) 

colnames(Germany) <- c("Date","Region","montant")
Europe <- readOGR(layer = "NUTS_RG_03M_2016_4326_LEVL_1", dsn = "../Data")


key <- Europe@data[10:33,c(2,4)]
key <- key[-c(1,5,9, 18:22),] %>%
  mutate(NUTS_NAME = as.character(NUTS_NAME))

key$NUTS_NAME[key$NUTS_NAME == "MECKLENBURG-VORPOMMERN"] <- "MECKLEN-VORPOMMERN"
#key$NUTS_NAME[key$NUTS_NAME == "THÜRINGEN"] <- "THURINGEN"

Germany <- merge(Germany, key, by.x = "Region", by.y = "NUTS_NAME")

Germany <- Germany %>%
  select(NUTS_ID, Date, montant)

colnames(Germany) <- c("NUTS_CODE","Date","montant")

write.csv(Germany, "../Data/Shiny_de.csv")
