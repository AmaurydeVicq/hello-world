#packages
library(readxl)
library(lubridate)

#urls
dates <- seq(from = ymd("2020-02-24"), to = today() -1, by = 1)
dates <- format(dates,"%Y%m%d")

#reading in the data
urls <- NULL
storage <- NULL
final <- NULL
for (i in dates) {
urls[i] <- paste("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-", 
                 i,
                 ".csv", sep = "")
storage <- read.csv(urls[i])
final <- rbind(final, storage)
}


#Correspondence table with NUTS-1 Regions

conversione_italia <- data.frame(NUTS_CODE = c(rep("ITC", 4), 
                         rep("ITD",4), 
                         rep("ITE", 4),
                         rep("ITF", 6), 
                         rep("ITG", 2)), 
           denominazione_regione = c("Piemonte", 
                                     "Valle d'Aosta",
                                     "Liguria",
                                     "Lombardia",
                                     "P.A. Trento",
                                     "Veneto",
                                     "Friuli Venezia Giulia",
                                     "Emilia Romagna",
                                     "Toscana",
                                     "Umbria",
                                     "Marche",
                                     "Lazio",
                                     "Abruzzo",
                                     "Molise",
                                     "Campania",
                                     "Puglia",
                                     "Basilicata",
                                     "Calabria",
                                     "Sicilia",
                                     "Sardegna"
                                    )
)

write.csv(conversione_italia, "../Data/conversiane_italia.csv")
