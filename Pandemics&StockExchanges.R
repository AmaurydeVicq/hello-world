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
MLife <- read_excel("MLife.xlsx")
TP <- read_excel("TP.xlsx")
BondYield <- read_excel("BondYield.xlsx")
RealWage <- read_excel("RealWage.xlsx")
Debt <- read_excel("Debt.xlsx")
CO2 <- read_excel("CO2.xlsx")

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
All <- na.omit(All)

ALLM <- All %>%
  group_by(country, Year) %>%
  summarise(Total=mean(Total))

BENEFRUSA <- rbind(BE,NL,FR,USA)


# Amend other data
GDPC2 <- GDPC %>% 
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States" | `country name` == "Italy" | `country name` == "United Kingdom"| `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "GDP per Capita") %>%
  filter(year >= 1900) 
GDPC2 <- GDPC2[-1,]


RealWage2 <- RealWage %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom"| `country name` == "China") %>%
  pivot_longer(-`country name`, names_to = "year", values_to = "Real Wage") %>%
  filter(year >= 1900)
RealWage2 <- RealWage2[-1,]  
RealWage2 <- na.omit(RealWage2)

Life2 <- Life %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom"| `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "LifeEx") %>%
  filter(year >= 1900)
Life2 <- Life2[-1,]  
Life2 <- na.omit(Life2)

MLife2 <- MLife %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom"| `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "LifeEx") %>%
  filter(year >= 1900)
MLife2 <- MLife2[-1,]  
MLife2 <- na.omit(MLife2)

BondYield2 <- BondYield %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom" | `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "Yield") %>%
  filter(year >= 1900)
BondYield2 <- BondYield2[-1,]
BondYield2 <- na.omit(BondYield2)

Debt2 <- Debt %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom" | `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "Debt") %>%
  filter(year >= 1900)
Debt2 <- Debt2[-1,] 
Debt2 <- na.omit(Debt2) 

TP2 <- TP %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "United States"  | `country name` == "Italy" | `country name` == "United Kingdom" | `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "TP") %>%
  filter(year >= 1900)
TP2 <- TP2[-1,] 
TP2 <- na.omit(TP2) 

CO2 <- CO2 %>%
  filter(`country name` == "Belgium" | `country name` == "France" | `country name` == "Netherlands" | `country name` == "Germany" | `country name` == "Italy" | `country name` == "United Kingdom" | `country name` == "China") %>%
  pivot_longer(-`country name`,names_to = "year", values_to = "CO2") %>%
  filter(year >= 1900)
CO2 <- CO2[-1,] 
CO2 <- na.omit(CO2)

SPC2 <- SPC%>%
  separate(Date, c("Year", "Month"))

# Flus & countries
SpanishFlu <- seq(1900,1930,1)
AsianFlu <- seq(1950,1965,1)
HongKongFlu <- seq(1960,1970,1)
RussianFlu <-seq(1970,1980,1)
SwineFlu <- seq(2000,2010,1)
countries <- c("Belgium","Netherlands","Italy","Germany","United Kingdom","United States")
Age <- seq(1,110,1)

# Plots - Overall
All %>%
  filter(Age == 55) %>%
  filter(Year >= 1900) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + ggtitle("Mortality Rates") 

All %>%
  filter(Age == 55) %>%
  filter(Year >= 1900) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + ggtitle("Mortality Rates") + facet_wrap(~ country, scales = "free")

# Plots - SpanishFlu
All %>%
  filter(Age == 55) %>%
  filter(Year %in% SpanishFlu) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + ggtitle("Mortality Rates") 

GDPC2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `GDP per Capita`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("GDP per Capita")

Life2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `LifeEx`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Life Expectancy at Birth")

BondYield2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `Yield`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Government Bond Yield")

RealWage2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `Real Wage`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Real Wages")

Debt2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `Debt`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total Gross Central Government Debt as % of GDP")

CO2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% SpanishFlu) %>%
  ggplot(aes(x = year, y = `CO2`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total CO2 emission")

SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% SpanishFlu) %>%
  ggplot(aes(x = date, y = `Real Price`)) + geom_line() +  scale_x_date(date_breaks = "3 year", date_labels = "%b %Y") + geom_vline(xintercept=as.numeric(as.Date(c("1917-01-01","1919-12-31")),linetype=4, colour="black")) + ggtitle("Effect of Spanish Flu on S&P Completion Index - dd. 01/01/1917 untill 31/12/1919")

# Plots - AsianFlu
BENEFRUSA %>%
  filter(Age == 60) %>%
  filter(Year >= 1920) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + facet_wrap(~ country)

GDPC2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `GDP per Capita`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("GDP per Capita")

Life2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `LifeEx`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Life Expectancy at Birth")

BondYield2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `Yield`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Government Bond Yield")

RealWage2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `Real Wage`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Real Wages")

Debt2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `Debt`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total Gross Central Government Debt as % of GDP")

CO2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% AsianFlu) %>%
  ggplot(aes(x = year, y = `CO2`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total CO2 emission")

SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% AsianFlu) %>%
  ggplot(aes(x = date, y = `Real Price`)) + geom_line() +  scale_x_date(date_breaks = "3 year", date_labels = "%b %Y") + ggtitle("Effect of Asian FLu on S&P Completion Index - dd. 06/1957 untill 12/1957") + geom_vline(xintercept=as.numeric(as.Date(c("1957-06-01","1957-12-01")),linetype=4, colour="black"))

#Plots - HongKong Flu
BENEFRUSA %>%
  filter(Age == 60) %>%
  filter(Year %in% HongKongFlu) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + facet_wrap(~ country)

GDPC2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `GDP per Capita`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("GDP per Capita")

Life2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `LifeEx`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Life Expectancy at Birth")

BondYield2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `Yield`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Government Bond Yield")

RealWage2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `Real Wage`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Real Wages")

Debt2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `Debt`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total Gross Central Government Debt as % of GDP")

CO2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% HongKongFlu) %>%
  ggplot(aes(x = year, y = `CO2`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total CO2 emission")

SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% HongKongFlu) %>%
  ggplot(aes(x = date, y = `Real Price`)) + geom_line() +  scale_x_date(date_breaks = "3 year", date_labels = "%b %Y") + ggtitle("Effect of Hongkong FLu on S&P Completion Index - dd. 07/1968 untill 01/1969") + geom_vline(xintercept=as.numeric(as.Date(c("1968-07-01","1969-01-01")),linetype=4, colour="black"))


#Plots - Russian Flu
BENEFRUSA %>%
  filter(Age == 60) %>%
  filter(Year %in% RussianFlu) %>%
  ggplot(aes(x = Year, y = Total, color = country, group = country)) + geom_line() + facet_wrap(~ country)

GDPC2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RussianFlu) %>%
  ggplot(aes(x = year, y = `GDP per Capita`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("GDP per Capita")

Life2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RusianFlu) %>%
  ggplot(aes(x = year, y = `LifeEx`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Life Expectancy at Birth")

BondYield2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RussianFlu) %>%
  ggplot(aes(x = year, y = `Yield`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Government Bond Yield")

RealWage2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RussianFlu) %>%
  ggplot(aes(x = year, y = `Real Wage`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Real Wages")

Debt2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RussianFlu) %>%
  ggplot(aes(x = year, y = `Debt`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total Gross Central Government Debt as % of GDP")

CO2 %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% RussianFlu) %>%
  ggplot(aes(x = year, y = `CO2`, color = `country name`, group = `country name`)) + geom_line() + facet_wrap(~ `country name`, scales = "free") + ggtitle("Total CO2 emission")

SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% RussianFlu) %>%
  ggplot(aes(x = date, y = `Real Price`)) + geom_line() +  scale_x_date(date_breaks = "3 year", date_labels = "%b %Y") + ggtitle("Effect of Russian FLu on S&P Completion Index - dd. 11/1977 untill 02/1978") + geom_vline(xintercept=as.numeric(as.Date(c("1977-11-01","1978-02-01")),linetype=4, colour="black"))

