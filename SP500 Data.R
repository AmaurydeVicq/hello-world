# Data inladen & beetje cleanen
SP500 <- read.csv2("C:/Users/Amaur/Desktop/hello-world/Data/S&P500.csv")
colnames(SP500)
names(SP500)[1] <- "Date"
SP500 <- transform(SP500, Year = substr(Date, 1, 4), Month = substr(Date, 5, 6))

# packages inladen
library(ggplot2)
library(dplyr)
library(tidyr)

# Figuur maken
SP500 %>%
  ggplot(aes(x= Year, y = Real.Price)) + geom_point() + scale_x_discrete(breaks=seq(1871,2020,5))