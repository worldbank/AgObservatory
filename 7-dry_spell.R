#-------------------------------------------------------------------------- 
# Ag Observatory R Tutorial: Dry Spell Analysis
#
# Purpose of script: It quantifies the logest dry spell over a rainy
# season (or specified period of time) as used used on the Crop and
# Livestock Assessment (pg 11)
# https://fscluster.org/zimbabwe/document/first-round-crop-and-livestock]
#
# It can be expanded to add other measure of dry spell frequency and severity
#
# Date updated: 2020-05-25
#--------------------------------------------------------------------------


# Install and load packages -----------------------------------------------

# Install the aWhere R packages, if you have not previously
devtools::install_github("aWhereAPI/aWhere-R-Library")
library(aWhereAPI)

# Load the packages needed for this script. 

#If they have not been installed yet on your computer, use this code to install
#them: install.packages("NAME OF PACKAGE")
library(seas)
library(dplyr)

# Load aWhere credentials -------------------------------------------------

# You will need to load your credentials file which includes your aWhere 
# key and secret, like a username and password. This gives you a token which 
# shows that you have access to the API and all of aWhere's data. Your 
# credentials should be kept in a location where you can easily find them. 
# Copy the pathfile name and paste it below over the phrase, 
# "YOUR CREDENTIALS HERE"

aWhereAPI::load_credentials("YOUR CREDENTIALS HERE")

# Set working & output directories ----------------------------------------

# The working directory is the location on your computer where R will 
# automatically save the output files created by this script.

# To set your working directory, find the folder on your computer where you 
# would like the outputs of this script to be saved, copy the pathfile name 
# and paste it over the phrase, "YOUR WD HERE"

working_dir <- "YOUR WD HERE" 
setwd(working_dir) # This sets your working directory to the working_dir path

#create output directory
csv_path <- "outputCSVs/"
dir.create(path = csv_path, showWarnings = FALSE, recursive = TRUE) 

# CHANGE THIS to your template file (a template file has the unique "locationid"
# for each of the aWhere "Virtual Weather Stations" (VWS), it often includes,
# locationid, latitude, longitide, admin levels, wkt, in addition to other data)
template_file <- "C:/AgObs/RunSet/TemplateZimbabwe.csv" 

# Read the locations from the template (i.e. each locationid/VWS has a
# coordinate associated with it,which is also the centroid of that VWS)
locations <- read.csv(template_file)

# Select area within the template you would like to pull data for
subarea_select <- dplyr::filter(locations, admin2_name == "Kwekwe")

# To pull data for the entire country use ' admin0_name == "Zimbabwe" ' or
# however you named your admin-level column


# Observed Data - API Pull ----------------------------------------------

# Warning: if you already have the data, jump to "Offline Longest Dry Spell"

# Here you will pull the historical data for your location of interest. 

# Set the starting and ending dates to a time period of interest
starting_date <- "2019-10-01"
ending_date <-  "2020-03-31"

#create empty data frame to store output data
ds <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c("locationid", "latitude", "longitude", "longest.dryspell", "shapewkt")
colnames(ds) <- x

# Pull the precipitation data via API
for (i in(1:nrow(subarea_select))) { 

  i=1 #use this to test whether data is being pulled from API and saved in working_dir
  #Extract from template latitude, longitude, locationid and shapewkt st
  lat <- subarea_select$latitude[i]
  lon <- subarea_select$longitude[i]
  locationid <- subarea_select$locationid[i]
  shapewkt <- subarea_select$shapewkt[i] 
  
  # Pull observed weather data from the aWhere API
  observed <- aWhereAPI::daily_observed_latlng(latitude = lat,
                                               longitude = lon,
                                               day_start = starting_date,
                                               day_end = ending_date)
  
  #before proceding save the data you pulled for further/future analyses (API
  #pulls take time, optimizing use still good practice for one time analyses) 
  #NOTE/Pull Request Challenge: this session can be improved by making use of ".RDATA"
  
  #adding locationid
  observed <- dplyr::mutate(observed, locationid = locationid)
  #add the WKT column
  observed <- dplyr::mutate(observed, shapewkt = shapewkt)
  #save CSV [Note to self: or use .RDATA instead?]
  write.csv(observed, file = paste0("outputCSVs/agmetData-",locationid,".csv"),
            row.names=F) 

  
# Longest Dry Spell -------------------------------------------------------

  # storing all data 
  precip_amount <- dplyr::select(observed, c(date, 
                                             precipitation.amount))
    
  precip_amount$date <- as.Date(precip_amount$date, )
    
    
  # Calculate the interarrivals (or spell periods), which are the number of days
  # between precipitation events (dry days), and the number of days of
  # continuous precipitation (wet days).
  temp_ds <- interarrival(precip_amount, 
                     var = "precipitation.amount", 
                     p.cut = 0.85, #change this to a different trace amount of rainfall if desired
                     inv = TRUE)
  
  #Select the longest dry spell
  longest_ds <- max(temp_ds$dry, na.rm=TRUE)
  #NOTE/Pull Request Challenge: add other measures that help understand better
  #frequency and severity of dry spells 
  
  #store new entry in a vector (row)
  ds_new_row <- c(locationid, lat, lon, longest_ds, shapewkt)
  #add row to dry spell data frame  
  ds <- rbind(ds, ds_new_row) 

}

temp <- ds[-c(1), ] 

#save dataframe
write.csv(temp, file = paste0("dry_spell",".csv"),
          row.names=F) 

#open saved file in QGIS and use the "shapewkt" column to map the
#"longest.dryspell"

#NOTE/Pull Request Challenge: replicate map of pg 11 in R
#https://fscluster.org/zimbabwe/document/first-round-crop-and-livestock]

#-------------------------------------------------------------------------- 
#-------------------------------------------------------------------------- 
#-------------------------------------------------------------------------- 
#-------------------------------------------------------------------------- 
#-------------------------------------------------------------------------- 
#-------------------------------------------------------------------------- 


# Offline Longest Dry Spell -----------------------------------------------

# In case you already have pulled the data needed

working_dir <- " YOUR WD HERE " 
setwd(working_dir) 

# list files 
file.names <- list.files(path = "C:/-- YOUR WD HERE -- /outputCSVs", pattern = "*.csv")
# outputCSVs is where you have all data pulled via API from previous session

#create empty data frame to store output data
ds <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c("locationid", "latitude", "longitude", "longest.dryspell", "shapewkt")
colnames(ds) <- x

# Extract the precipitation data 
for (i in (1:length(file.names))) { 
  
  #i=1 #you can use to test if is working
  locations <- read.csv(file.names[i])
  
  #you can reassign start and ending date here
  starting_date <- "2019-10-01"
  ending_date <-  "2020-03-31"
  
  #extracting variables you need from a line 
  extr <- locations[1,]
  #get the first latitude, longitude, and name of your location(s) of interest 
  lat <- extr$latitude
  lon <- extr$longitude
  locationid <- extr$locationid
  shapewkt <- extr$shapewkt
  
  #subsetting/selecting data from prev. assigned starting date until ending date
  observed <- dplyr::filter(locations, 
                            date >= as.Date(starting_date) 
                            & date <= as.Date(ending_date))
  
  
  precip_amount <- dplyr::select(observed, c(date, 
                                             precipitation.amount))
  
  precip_amount$date <- as.Date(precip_amount$date, )
  
  
  # Calculate the interarrivals (or spell periods), which are the number of days
  # between precipitation events (dry days), and the number of days of
  # continuous precipitation (wet days).
  temp_ds <- interarrival(precip_amount, 
                     var = "precipitation.amount", 
                     p.cut = 0.85, #change this to a different trace amount of rainfall if desired
                     inv = TRUE)
  
  #Select the longest dry spell
  longest_ds <- max(temp_ds$dry, na.rm=TRUE)
  #store new entry in a vector (row)  
  ds_new_row <- c(locationid, lat, lon, longest_ds, shapewkt)
  #add row to dry spell data frame  
  ds <- rbind(ds, ds_new_row) 
  
  
}

temp <- ds[-c(1), ]

#save dataframe
write.csv(temp, file = paste0("dry_spell_Zimbabwe_Oct1_Mar31",".csv"),
          row.names=F) 





