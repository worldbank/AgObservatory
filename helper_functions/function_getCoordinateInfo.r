##Function takes a latitude/longitude pair as an input and returns the country in which it falls
#Parameters:
#points: (data.frame or matrix) input must have columns of point(s) with columns named "longitude" and "latitude" in that order
#attribute: (character) type of point information to return. Must be "country", "continent", or "continent classification"

#Note: point latitudes/longitudes must be in decimal degrees 
#required libraries: sp, rworldmap

list.of.packages = c("sp", "rworldmap")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(sp)
library(rworldmap)


getCoordinateInfo <- function(points, attribute){  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  if(attribute == "country") {
    # return the ADMIN names of each country
    indices$ADMIN
  } else if(attribute == "continent") {
    indices$REGION         # returns the continent (7 continent model)  
  } else if(attribute == "continent classification") {
    indices$GBD            # returns the continent and classification    
  } else {
    stop("Your attribute parameter is not allowed. Please choose either 'country', 'continent', or 'continent classification'.")
  }
  #indices$POP_EST        # returns the estimated population of the country
  #indices$ISO3           # returns the ISO3 code of country
  #indices$continent      # returns the continent (6 continent model)
}