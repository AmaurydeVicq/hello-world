# What determines regional differences?

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
library(MASS)

# Load in data
temp <- list.files(path = "./Data2",pattern = "*.csv")
temp <- paste("./Data2/",temp,sep="")
data <- lapply(temp, read.csv)
CC <- read_excel("./Data/CountryCodes.xlsx")

as.data.frame(data[2])

#Amend Data
Clean <- function(x)
{
 x <- select(x,1,2,3,4,6:7)
 rename(x,LOCATION = ï..LOCATION)
}


d1 <- data[1]
d1 <- Clean(as.data.frame(d1))
d2 <- data[2]
d2 <- Clean(as.data.frame(d2))
d3 <- data[3]
d3 <- Clean(as.data.frame(d3))
d4 <- data[4]
d4 <- Clean(as.data.frame(d4))
d5 <- data[5]
d5 <- Clean(as.data.frame(d5))
d6 <- data[6]
d6 <- Clean(as.data.frame(d6))
d7 <- data[7]
d7 <- Clean(as.data.frame(d7))
d8 <- data[8]
d8 <- Clean(as.data.frame(d8))
d9 <- data[9]
d9 <- Clean(as.data.frame(d9))
d10 <- data[10]
d10 <- Clean(as.data.frame(d10))
d11 <- data[11]
d11 <- Clean(as.data.frame(d11))
d12 <- data[12]
d12 <- Clean(as.data.frame(d12))
d13 <- data[13]
d13 <- Clean(as.data.frame(d13))
d14 <- data[14]
d14 <- Clean(as.data.frame(d14))
d15 <- data[15]
d15 <- Clean(as.data.frame(d15))


D1 <- d1 %>% 
  filter(TIME == 2018) %>%
  pivot_wider(names_from = MEASURE, values_from = Value)
D2 <- d2 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = SUBJECT, values_from = Value)
D3 <- d3 %>% 
  filter(TIME == 2017) %>%
  rename(EldyPop = Value)
D4 <- d4 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = SUBJECT, values_from = Value)
D5 <- d5 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = SUBJECT, values_from = Value)
D6 <- d6 %>% 
  filter(TIME == 2017) %>%
  rename(PharmaExp = Value)
D7 <- d7 %>% 
  filter(TIME == 2017) %>%
  rename(FluVaccin = Value)
D8 <- d8 %>% 
  filter(TIME == 2017) %>%
  rename(GDP = Value) 
D9 <- d9 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = SUBJECT, values_from = Value)
D10 <- d10 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = MEASURE, values_from = Value)
D11 <- d11 %>% 
  filter(TIME == 2017) %>%
  pivot_wider(names_from = MEASURE, values_from = Value)
D11 <- D11 %>%
  pivot_wider(names_from = SUBJECT, values_from = Value)
D12 <- d12 %>% 
  filter(TIME == 2017) %>%
  rename(Smokers = Value)
D14 <- d14 %>% 
  filter(TIME == 2017) %>%
  rename(MedicalDoc = Value)
D15 <- d15 %>% 
  filter(TIME == 2017) %>%
  rename(Nurses = Value)

# Merge
D101 <- left_join(x=D10,y=D1, by = "LOCATION", "TIME")
D101 <- D101[-7]
D1012 <- left_join(x=D101,y=D2, by = "LOCATION", "TIME") 
D10123 <- left_join(x=D1012,y=D3, by = "LOCATION", "TIME") 
D101234 <- left_join(x=D10123,y=D4, by = "LOCATION", "TIME") 
D1012345 <- left_join(x=D101234,y=D5, by = "LOCATION", "TIME")
D10123456 <- left_join(x=D1012345,y=D6, by = "LOCATION", "TIME")
D101234567 <- left_join(x=D10123456,y=D7, by = "LOCATION", "TIME")
D1012345678 <- left_join(x=D101234567,y=D8, by = "LOCATION", "TIME")
D10123456789 <- left_join(x=D1012345678,y=D9, by = "LOCATION", "TIME")
D1012345678912 <- left_join(x=D10123456789,y=D12, by = "LOCATION", "TIME")
D101234567891214 <- left_join(x=D1012345678912,y=D14, by = "LOCATION", "TIME")
D10123456789121415 <- left_join(x=D101234567891214,y=D15, by = "LOCATION", "TIME")

DMerged <- D10123456789121415[c(-2,-7,-8,-11,-12,-13,-21,-22,-23,-24,-26,-27,-28,-32,-34,-38,-41,-43,-46)]
DMerged <- DMerged[c(-19,-24,-26,-27,-28,-29:-32,-34:-36,-40:-43,-45:-48,-50:-53)]
DMerged <- DMerged[c(-2,-22)]
DMerged <- rename(DMerged, Code = LOCATION)
PerCountry <- rename(PerCountry, Country = Country.Region)
DMerged <- DMerged %>% left_join(CC, by = NULL) %>% select(Country, everything())
DMerged2 <- left_join(DMerged, PerCountry, by = NULL)
DMerged2 <- rename(DMerged2, Date = name)
DMerged3 <- DMerged2 %>% filter(Date >= as.Date("2020-03-22"))
DMerged3 <- DMerged3[c(-1,-2,-30,-3)]
DMerged4 <- na.omit(DMerged3)


# Models
mod <- lm(Confirmed ~ Smokers + PharmaExp + EldyPop + AGE_17 + AGE_18 + AGE_19 + COMPULSORY + VOLUNTARY + GDP + TONNE_CAP + TOT.y, DMerged3)
summary(mod)
stepAIC(mod, direction = "both")
mod2 <- lm(Confirmed ~ Smokers + PharmaExp + EldyPop + AGE_17 + AGE_18 + 
  COMPULSORY + VOLUNTARY + TONNE_CAP + TOT.y, DMerged3)
mod3 <- lm(Deaths ~ Smokers + PharmaExp + EldyPop + AGE_17 + AGE_18 + AGE_19 + COMPULSORY + VOLUNTARY + GDP + TONNE_CAP + TOT.y, DMerged3) 
stepAIC(mod3, direction = "both")
mod4 <- lm(Deaths ~ Smokers + PharmaExp + EldyPop + AGE_18 + AGE_19 + COMPULSORY + 
             VOLUNTARY + TONNE_CAP + TOT.y, DMerged3)

DMerged3 %>% ggplot(aes(x=VOLUNTARY, y = Deaths)) + geom_point() + scale_y_log10()
summary(mod4)

# Plots
DMerged3 %>% ggplot(aes(x=Smokers, y = Deaths)) + geom_point() + scale_y_log10()
DMerged3 %>% ggplot(aes(x=EldyPop, y = Deaths)) + geom_point() + scale_y_log10()
DMerged3 %>% ggplot(aes(x=TONNE_CAP, y = Deaths)) + geom_point() + scale_y_log10()
DMerged3 %>% ggplot(aes(x=VOLUNTARY, y = Deaths)) + geom_point() + scale_y_log10()
