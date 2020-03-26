library(tidyr)
library(stringr)
library(readr)
library(rvest)
Netherlands <- read_html("https://www.rivm.nl/coronavirus-kaart-van-nederland") %>%
  html_nodes("#csvData") %>%
  html_text() %>%
  read_lines() %>%
  str_split(";") %>%
  unlist() 

Netherlands <- Netherlands[-1] %>%
  matrix(ncol = 5, byrow = TRUE)