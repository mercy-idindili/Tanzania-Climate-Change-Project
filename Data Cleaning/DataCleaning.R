library(tidyverse)
library(rnaturalearth)


#Cleaning rainfall data 

bc_meanrain <- read.csv("Data Cleaning/Original_Mean_Rain_Data.csv")

for ( col in 1:ncol(bc_meanrain)){
  colnames(bc_meanrain)[col] <-  sub("udel_precip_v501_sum.", " ", colnames(bc_meanrain)[col])
}

for ( col in 1:ncol(bc_meanrain)){
  colnames(bc_meanrain)[col] <-  sub(".mean", " ", colnames(bc_meanrain)[col])
}

ncol(bc_meanrain)

bc_meanrain<- bc_meanrain[,c(-24:-28)]

ncol(bc_meanrain)

bc_meanrain <- bc_meanrain[ , c(-20,-21,-23)]

ncol(bc_meanrain)

bc_meanrain <- bc_meanrain[ , c(-1)]

ac_meanrain <- bc_meanrain

mean_rain_longformatdata <- ac_meanrain  %>%
  pivot_longer(-name, names_to = "year", values_to = "rainfall")

#saving it as a csv to be uploaded when sharing the shiny app

write_csv(mean_rain_longformatdata, "Shiny App Creation/mean_rain_longformatdata.csv")




#Cleaning temp data 
bc_meantemp <- read.csv("Data Cleaning/Original_Mean_Temp_Data.csv")


for ( col in 1:ncol(bc_meantemp)){
  colnames(bc_meantemp)[col] <-  sub("cru_tmp_yearly_mean.", " ", colnames(bc_meantemp)[col])
}

for ( col in 1:ncol(bc_meantemp)){
  colnames(bc_meantemp)[col] <-  sub(" .mean", " ", colnames(bc_meantemp)[col])
}

ncol(bc_meantemp)

bc_meantemp<- bc_meantemp[,c(-22:-27)]

ncol(bc_meantemp)

bc_meantemp<- bc_meantemp[,c(-1, -20, -19)]

ac_mean_temp <- bc_meantemp

mean_temp_longformatdata <- ac_mean_temp  %>%
  pivot_longer(-name, names_to = "year", values_to = "temparature")

#save

write_csv(mean_temp_longformatdata, "Shiny App Creation/mean_temp_longformatdata")




#cleaning CO2 data 

bc_co2conc <- read.csv("Data Cleaning/Original_Mean_CO2Concentration_Data.csv")

ncol(bc_co2conc)

bc_co2conc<- bc_co2conc[,c(-1, -6, -7, -9,-10,-11,-12,-13,-14)]

for ( col in 1:ncol(bc_co2conc)){
  colnames(bc_co2conc)[col] <-  sub("oco2_xco2_yearly.", " ", colnames(bc_co2conc)[col])
}

for ( col in 1:ncol(bc_co2conc)){
  colnames(bc_co2conc)[col] <-  sub(".mean", " ", colnames(bc_co2conc)[col])
}

ac_meanco2conc <- bc_co2conc

mean_co2conc_longformatdata <- ac_meanco2conc  %>%
  pivot_longer(-name, names_to = "year", values_to = "carbonconc")

#save

write_csv(mean_co2conc_longformatdata, "Shiny App Creation/mean_co2conc_longformatdata.csv")




#Creating Map Data 

TanzaniaMAP <- ne_states(country = "united republic of tanzania", returnclass = c("sf"))

add_rain <- read.csv("Shiny App Creation/mean_rain_longformatdata.csv")

add_temp <- read.csv("Shiny App Creation/mean_temp_longformatdata.csv")

add_rain$name <- as.character(add_rain$name)

add_temp$name <- as.character(add_temp$name)

add_rain$rainfall <- as.factor(add_rain$rainfall)

add_temp$temparature <- as.factor(add_temp$temparature)

TZZ1 <- left_join(TanzaniaMAP, add_temp, by = "name")
 
Map_Data <- left_join(TZZ1, add_rain, by = "name")

#save
write_rds(Map_Data, "Shiny App Creation/Map_Data.rds")









