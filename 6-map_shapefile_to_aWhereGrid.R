#--------------------------------------------------------------------------
# Ag Observatory R Tutorial: Mapping shapefile to aWhere grid
#
# Purpose of script:
# Maps any given shapefile to the aWhere's Virtual Weather Stations grid cell
#
# The tutorial describes one option to map any shapefile of interest to aWhere's
# weather data .csv files.  This can be applied to quickly generate analyses, 
# statistics, histograms, maps etc. at, for example, different administrative
# levels, watersheds, agro-ecological zones or other custom data of interest.

# aWhere data utilizes a unique key identifying each grid cell across space.
# This grid cell, or "locationid" is the minimum unit of analysis and represents
# a Virtual Weather Station (VWS). Currently, an aWhere VWS is of 5 arc-min
# resolution, or about 9 km at the equator.

#At the end of this tutorial, the outcome will be a template that combines both
#aWhere unique locationid as well as selected data from a given shapefile of
#your choice. This template can then be applied to the Tutorial "Histograms,
#Statistics, and Maps for Region and Selected Subregions".
#
#You will need to be connected to the internet to run this script.
#
#Date updated: 2020-02-09
#--------------------------------------------------------------------------

# Install and load packages -----------------------------------------------

# Clear your environment and remove all previous variables
rm(list = ls())

# Load the packages needed for this script
library(sp)
library(rgdal)
library(rangeMapper) 

# Set working & output directories ----------------------------------------

# The working directory is the location on your computer where R will 
# automatically save the output files created by this script.

# To set your working directory, find the folder on your computer where you 
# would like the outputs of this script to be saved, copy the pathfile name 
# and paste it over the phrase, "YOUR WD HERE"

working_dir <- "YOUR WD HERE" 
setwd(working_dir) # This sets your working directory to the working_dir path

# Set parameters ----------------------------------------------------------

#Set the template file you would like to have the spatial data extracted to.
#Below you see the example of the agroecological zones of Zimbabwe shapefile
#being mapped by grid cell

templateFile <- "YOUR PATHFILE HERE/TemplateZimbabwe.csv"

outputFile <-  "YOUR PATHFILE HERE/TemplateZimbabwe_AEZ.csv"

# Processing the shapefile data -------------------------------------------

# define folder where the shapefile files are stored
dsn.name <- paste0("YOUR PATHFILE HERE/zimagroecoregions")

# define the name of the shapefile files
layer.name <- paste0("Zimagroecoregions")

# define which fields of data in the shapefile you would like added to the template
# Can be explored before loading file using the command
#      rgdal::ogrInfo(dsn = dsn.name,layer = layer.name)

fieldsToKeep <- c('NATREG')


# Mapping the aWhere weather data grid cell -------------------------------

#load aWhere weather data file
df <- read.csv(templateFile,stringsAsFactors = FALSE)


#Use the WKT in the template file to convert the template to a spatial polygon
#data frame
df.sp <- rangeMapper::WKT2SpatialPolygonsDataFrame(dat = df
                                                   ,geom = 'shapewkt'
                                                   ,id = 'locationid')

#Check gridcell 
plot(df.sp)


#The above fxn converts the id's to factors.  
df.sp@data$locationid <- as.numeric(as.character(df.sp@data$locationid))


#Define the projection of this data.  It is Lat/Lon (WGS 84)
proj4string(df.sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#read the layer (shapefile)
shp <- rgdal::readOGR(dsn = dsn.name
                      ,layer = layer.name
                      ,stringsAsFactors = FALSE)

#Check shapefile
plot(shp)

#If CRS are not identical an error will be thrown. Convert CRS so both layers
#are WGS 84 and re-check if these are identical
if(identicalCRS(df.sp, shp) == FALSE){
  shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  message("Coordinate Reference System transformed to WGS 84")
  
  #re-check if CRS is the same
  if (identicalCRS(location, shp) == FALSE) {
    stop('Coordinate Reference Systems cannot be transformed to match')
  }
} 



#Do Spatial Extract -------------------------------------------------------

#Record vector of all locations that are to be output by spatial extract
uniqueLocations <- unique(df.sp@data$locationid)

# Combining both datasets -------------------------------------------------

# extract shp info from layer using the location matrix The use of returnList
# allows all of the entries found within a grid cell to be returned instead of
# simply the first one or a simple numerical aggregation
df.extract <- sp::over(df.sp
                       ,shp[fieldsToKeep]
                       ,returnList = TRUE)

#Because there can be multiple points from the shapefile within each aWhere grid
#cell, we need to aggregate the info. If the data we had been extracting was
#numerical, we could have passed a fn such as mean/min/max to the over fxn.
#Instead, because this data is categorical, we will choose the entry that
#appears most often (mode)

#We also need to add the locationid for each polygon to the output of the
#spatial extract so we can add this info back to the template

df.out <- list()
for (x in 1:length(uniqueLocations)) {
  
  df.out[[x]] <- data.frame('locationid' = uniqueLocations[x])
  
  for (y in 1:length(fieldsToKeep)) {
    temp <- names(sort(table(df.extract[[x]][fieldsToKeep[y]]),decreasing = TRUE)[1])
    
    df.out[[x]][fieldsToKeep[y]] <- temp
  }
}

#combine the list into a single dataset
df.out <- bind_rows(df.out)

# Combine aWhere weather dataframe and the shapefile data
df <- merge(df
            ,df.out
            ,by = 'locationid')

#This removes the locations that are outside of the region used in the shapefile
#(i.e. does not have region info in these setting)
df <- df[complete.cases(df),]

#save the file
write.csv(df, file = outputFile)


