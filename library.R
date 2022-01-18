library(tidyverse) 
library(shiny) 
library(shinydashboard)

##maps

library(tmap)
library(tmaptools)
library(sf)





# cool graphs
library(plotly)

#table

library("kableExtra")

library(rsconnect)


rsconnect::setAccountInfo(name='davidsegovia123',
                          token='7E3F32AEAA1425661EAC1C87E870A16A',
                          secret='oZCK4PLIQ8nt81C2LSmCgJGjMAeCg6bTuu2i0zGH')



library(here)




#here("~/Library/Mobile Documents/com~apple~CloudDocs/PA 446 Coding Civic Data Applications/FInal project/LifeExp2")

healthdata <- read.csv("healthdata2.csv")
chi_shape <- here("geo_export_d5e1bb4c-6641-4e21-a6cc-81750dfdbf75.shp") %>%
  st_read()


healthdata$GEOID <- as.character(healthdata$GEOID ) 
healthmap <- left_join(chi_shape, healthdata, by= c("area_num_1" = "GEOID"))

choices= c("bingedrinking_percent", "physicalinactivity_percent", "smoking_percent", "smoking_pregnancypercent", "accessFruitsVegetables_percent", "fruitVegetableServings_percent", "sodaconsumption_percent", "neighborhoodsafety_percent", "black_percent", "white_percent", "hispanic_latino_percent")
name= unique(healthdata$Name)







