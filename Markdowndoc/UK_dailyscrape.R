library(dplyr)
library(stringr)

x <- c("ggmap", "rgdal", "rgeos", "maptools", "tmap")
lapply(x, library, character.only = TRUE)

UK <- read.csv("https://www.arcgis.com/sharing/rest/content/items/ca796627a2294c51926865748c4a56e8/data") 
  
conversionUK <- data.frame(NUTS_CODE = c("UKH", "UKI", "UKF", "UKG", "UKE","UKC","UKD","UKJ","UKK" ), 
                           NHSRNm = c("East of England", "London", "Midlands", "Midlands", "North East and Yorkshire",
                                      "North East and Yorkshire", "North West", "South East", "South West") )

write.csv(conversionUK,'../hello-world/Data/conversionUK.csv')

final <- UK %>%
  merge(conversionUK) %>%
  mutate(Date = Sys.Date(), NHSRNm = as.character(NHSRNm), TotalCases = as.numeric(str_squish(str_replace(TotalCases,",","")))) 

freq <- as.data.frame(table(final$NHSRNm))
final <- left_join(final, freq, by = c("NHSRNm" = "Var1")) 
  
final <- final %>%
  mutate(montant = TotalCases / Freq) %>%
  select(NUTS_CODE, Date, montant)

final
write.csv(final, paste("../hello-world/Data/UK_",Sys.Date(),".csv", sep = ""))
