#Covid evolution

#Load Libraries

# Load packages
library(dplyr)
library(SciViews)
# library(readxl)
# library(xlsx)
# library(tidyr)
library(ggplot2)
# library(tidyr)
# library(readr)
# library(lubridate)
# library(scales)
# library(lubridate)

setwd("../hello-world/Data")
# policies <- read.csv("policies.csv")
montantBE <- read.csv("Belgique.csv")
montantNL <- read.csv("Shiny_nl.csv")


montantBE[c("lnmontant")] <- ln(deathBE[c("montant")])
montantNL[c("lnmontant")] <- ln(montantNL[c("montant")])


ggplot(data=montantBE, aes(x=Date, y=lnmontant, group=NUTS_CODE)) +
  geom_line(aes(linetype=NUTS_CODE))+
  geom_point() 

ggplot(data=montantBE, aes(x=Date, y=montant, group=NUTS_CODE)) +
  geom_line(aes(linetype=NUTS_CODE))+
  geom_point() 

ggplot(data=montantNL, aes(x=Date, y=lnmontant, group=NUTS)) +
  geom_line(aes(linetype=NUTS))+
  geom_point() 

ggplot(data=montantNL, aes(x=Date, y=montant, group=NUTS)) +
  geom_line(aes(linetype=NUTS))+
  geom_point() 
