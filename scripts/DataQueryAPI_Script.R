#########################################################################
#########################################################################

###One-time aWhere R package install
##Run all three of the below commands once to install the package
##FMI on the aWhere R package, visit https://github.com/aWhereAPI/aWhere-R-Library/blob/master/documentation/complete-documentation.md
##For WBG staff, contact Caroline for assistance (csartoratosilvaf@worldbank.org)  

#########################################################################
#########################################################################

#install the package and its dependencies
install.packages('devtools')
install.packages(c('chron', 'magrittr', 'btcops', 'DBI', 'assertthat', 'Rcpp', 'tibble'))
devtools::install_github("aWhereAPI/aWhere-R-Library")

#########################################################################
#########################################################################

##Basic Commands in the aWhere R Package
#You may run these as many times as you would like, and update as needed

#########################################################################
#########################################################################

library(tidyr)
library(aWhereAPI)

###Set Working Directory
##update this to use the file path on your computer where you would like to save your data.
setwd("C:/YOUR WORKING DIRECTORY")

##Get Token - key and secret
#update the fields "key" and "secret" with your personal key and secret accessed 
#via the aWhere Developer Portal - http://developer.awhere.com/user/login
#alternatively, can load credentials stored in a .txt file

get_token("key", "secret")

load_credentials("C:./credentials.txt")

#set details of the query you want to make
lat <- -16.065050
lon <- 33.526264
day_start <- "2018-01-01"
day_end <- "2018-04-10"
monthday_start <- "01-01"
monthday_end <- "04-10"
year_start <- "2007"
year_end <- "2017"

#source helper function using your directory
source("./function/function_generateaWhereDataset.R")

####Weather data - input name/id of field or latitude/longitude points
#This pulls the data and creates a dataset in R titled "obs" that can be viewed later.
#The two lines below will perform an identical function - one calls location by field ID/name and the other by coordinates.

obs <- daily_observed_latlng(lat, lon, 
                             day_start = day_start, 
                             day_end = day_end)

View(obs)


###Forecast data - customize call as needed
#This pulls the data and creates a dataset in R titled "fcst" that can be viewed later.
#Note: Update day_start to be a day in the near future or today.
#Note: Forecasts are available in hourly increments, the block_size parameter allows you to set how many hours of the day 
##should be aggregated in the forecast.

fcst <- forecasts_latlng(lat, lon, 
                         day_start = as.character(Sys.Date()), 
                         day_end = as.character(Sys.Date()+7), block_size = 24)

View(fcst)


###Long-term norm data - norms determined based on month-day (MM-DD) spans, with default 
###as 10-year norms. Can customize years and exclude years.
#This pulls the data and creates a dataset in R titled "ltn" that can be viewed later.

##regular norms
ltn1 <- weather_norms_latlng(lat, lon, 
                             monthday_start = monthday_start, 
                             monthday_end = monthday_end, 
                             year_start = year_start, 
                             year_end = year_end)
  
##custom-year norms
ltn2 <- weather_norms_latlng(lat, lon, 
                             monthday_start = monthday_start, 
                             monthday_end = monthday_end, 
                             year_start = year_start, 
                             year_end = year_end, 
                             exclude_years = c("2011", "2016"))


###Agronomic data
#This pulls the data and creates a dataset in R titled "ag" or "ag_ltn" that can be viewed later.

agro <- agronomic_values_latlng(lat, lon, 
                                day_start = day_start, 
                                day_end = day_end)

###Agronomic Norms
##multiyear norms

agro_ltn <- agronomic_norms_latlng(lat, lon, 
                                   month_day_start = monthday_start, 
                                   month_day_end = monthday_end, 
                                   year_start = year_start, 
                                   year_end = year_end)


###Pull entire dataset
weather_df <- generateaWhereDataset(lat = lat, lon = lon, 
                                    day_start = day_start, 
                                    day_end = day_end, 
                                    year_start = year_start, 
                                    year_end = year_end)


###Save & export data into .csv file
#You can change which dataset you want to export - here we're exporting "obs".
write.csv(obs, file = "weather_dataset.csv")


#########################################################################
###GENERATE DATA FOR MULTIPLE POINTS
#########################################################################

locations <- read.csv("C:./data input/Punjab Coordinates.csv")

#construct for loop which iterates over each location
for (i in 5:(nrow(locations))) {
    
  #assign each latitude/longitude pair to the objects lat/lon in turn
  lat <- locations$latitude[i]
  lon <- locations$longitude[i]
  
  #generate a dataset for each individual lat/lon pair
  weather_temp <- generateaWhereDataset(lat, lon, 
                                        day_start = day_start, 
                                        day_end = day_end,
                                        year_start = year_start,
                                        year_end = year_end)
  
  #for all individual datasets, if they are not the first, bind them to the data already pulled
  if(i == 1) {
    weather_df <- weather_temp
  }   else {
    weather_df <- rbind(weather_df, weather_temp)
  }
}

#check the column names of the data
names(weather_df)

#summarize the data across the region by date
weather_df_summarized <- weather_df %>% 
  group_by(date) %>% 
  summarise(maxTemp.amount = mean(maxTemp.amount),
            maxTemp.average = mean(maxTemp.average),
            precipitation.amount = sum(precipitation.amount),
            precipitation.average = sum(precipitation.average),
            accumulatedPrecipitation.amount = sum(accumulatedPrecipitation.amount),
            accumulatedPrecipitation.average = sum(accumulatedPrecipitation.average),
            accumulatedPet.amount = mean(accumulatedPet.amount),
            accumulatedPet.average = mean(accumulatedPet.average),
            accumulatedPpet.amount = mean(accumulatedPpet.amount),
            accumulatedPpet.average = mean(accumulatedPpet.average))
