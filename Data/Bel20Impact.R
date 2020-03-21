#Effect of Covid-19 on Various Stock Market Indices

#Load Libraries

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
library(lubridate)

setwd("../hello-world/Data")
bel20 <- read.csv("Bel20.csv")


bel20$Close <- as.character(bel20$Close)
bel20$Close <- as.numeric(bel20$Close)


#Impact Corona Virus on SP Completion Index
SPC2 %>%
  mutate(date = ymd(paste(Year,Month,01, sep = "-"))) %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% Covid_19) %>%
  ggplot(aes(x = date, y = `Real Price`)) + geom_line() +  theme_light()+ theme(text = element_text(size = 15))+ scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") + 
  geom_vline(xintercept=as.numeric(as.Date(c("2018-01-01","2020-03-30")),linetype=4, colour="black")) + ggtitle("Effect of Covid-19 on S&P Completion Index")+
  annotate("rect", fill = "red", alpha = 0.5, 
           xmin = as.Date("2020-03-04", "%Y-%m-%d"), xmax = as.Date("1919-12-31", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)


#Impact on Bel20
bel20 %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x= Date, y= Close))+
        geom_line(size=1)+
        scale_x_date(labels = date_format("%b"))+
        xlab("2019-2020")+
        ggtitle("Effect of Covid-19 on the Bel20")+
        theme(text = element_text(size = 15))+
        geom_vline(xintercept=as.numeric(as.Date("2020-02-26")),linetype=1, colour="blue", size=1, )+
        geom_vline(xintercept=as.numeric(as.Date("2020-03-06")),linetype=1, colour="red", size=1)+
        geom_vline(xintercept=as.numeric(as.Date("2020-01-31")),linetype=1, colour="black", size=1)+
        annotate("text", x = as.Date("2020-03-12"), y = 4000, label = "100th Case BE", color ="red")+
        annotate("text", x = as.Date("2020-02-14"), y = 3200, label = "100th Case IT", color="blue")+
        annotate("text", x = as.Date("2020-01-20"), y = 3800, label = "1st Case IT")+
        theme_light()
  
scale_y_continuous(limits = c(2000, 4500), breaks = seq(2000, 4500, 6), name = "Points")+
