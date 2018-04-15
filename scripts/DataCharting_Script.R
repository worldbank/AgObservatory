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

###Generate graphics with aWhere data

#########################################################################
#########################################################################

###Set Working Directory
##update this to use the file path on your computer where you would like to save your data.
setwd("C:/YOUR WORKING DIRECTORY")

load_credentials("C:./credentials.txt")


library(ggplot2)
library(ggthemes)

##source helper functions
source("./function/function_generateaWhereDataset.R")
source("./function/function_generateaWhereStdDevChart.R")
source("./function/function_generateaWhereChart.R")
source("./function/function_multiplot.R")

#set details of the query you want to generate graphs
placename <- "Kapurthala"
lat <-   31.3927
lon <-   75.3557

day_start <- "2018-01-01" # start day of analysis
day_end <- as.character(Sys.Date()+7) # forecast
year_start <- 2008
year_end <- 2017

#build dataset based on coordinate indicated
weather_df <- generateaWhereDataset(lat = lat, lon = lon, 
                                    day_start = day_start, 
                                    day_end = day_end, 
                                    year_start = year_start, 
                                    year_end = year_end)


###Option to save & export data into .csv file
#write.csv(weather_df, file = "weather_dataset.csv")

##Generate a single graph
#At this time, variable can only be "precipitation", "accumulatedPrecipitation", "maxTemp", 
#"minTemp", "pet", "accumulatedPet", "ppet", or "rollingavgppet"

generateaWhereChart(data = weather_df,
                    variable = "accumulatedPrecipitation",
                    title = "Accumulated Precipitation")

generateaWhereChart(data = weather_df,
                    variable = "rollingavgppet",
                    title = "Rolling Average P/PET")




##The generateaWhereChart function also has a few options for customizing
##the rolling average window, and for adding a line for "effective" precipitation.

#Effective precip caps daily precip values (and therefore accumulations) at a certain
#threshold, demonstrating the fact that a plant can only take up so much water a day,
#even if more rain fell.

#To add a line for effective precip, add the parameter "e_precip = TRUE".
#The default threshold for effective precip is max 35mm per day. To customize this,
#add a parameter "e_threshold = X", where X equals the mm cap you want to use.

#To customize the rolling average window, add the parameter "rolling_window = X",
#where X is the number of days to include in the rolling average. The default is 30 days.

##The generateaWhereStdDevChart function works similarly to generateaWhereChart function
##while adding the standard deviation to the dataset and graph

generateaWhereChart(data = weather_df,
                    variable = "rollingavgppet",
                    title = "Rolling Average P/PET",
                    rolling_window = 25)

generateaWhereChart(data = weather_df,
                    variable = "rollingavgppet",
                    title = "Rolling Average P/PET",
                    rolling_window = 25,
                    e_precip = TRUE,
                    e_threshold = 30)

##generate multiple graphs and assemble into a multiplot
#plots need to be assigned to an object before they are assembled
# Example 1: 2 plots with costum title

precip <- generateaWhereChart(data = weather_df,
                              variable = "accumulatedPrecipitation",
                              title = "Accumulated Precipitation")

maxtemp <- generateaWhereChart(data = weather_df,
                               variable = "maxTemp",
                               title = "Daily Maximum Temperature")

multiplot(precip, maxtemp, fontsize = 20, cols = 2, 
          title = "Current vs Long-Term Norms (January 1 2018 - April 14 2018)")


#Example 2: 4 plots with automated title

accprecip <- generateaWhereStdDevChart(data = weather_df,
                                       variable = "accumulatedPrecipitation",
                                       title = "Accumulated Precipitation")

rollingavgppet <- generateaWhereChart(data = weather_df,
                                      variable = "rollingavgppet",
                                      title = "30 Day Rolling Average eP/PET and P/PET",
                                      e_precip = TRUE)

maxtemp <- generateaWhereStdDevChart(data = weather_df,
                                     variable = "maxTemp",
                                     title = "Maximum Temperature")

mintemp <- generateaWhereStdDevChart(data = weather_df,
                                     variable = "minTemp",
                                     title = "Minimum Temperature")


multiplot(accprecip, rollingavgppet, maxtemp, mintemp, fontsize = 15, cols = 2,
          title = paste0("Current vs Long-Term Normal in ", placename, " (", lat, ", ", 
                         lon, ")", " From ", head(sort(weather_df$date), n=1), " to ", 
                         tail(sort(weather_df$date), n=1)))


