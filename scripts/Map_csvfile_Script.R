#########################################################################
#########################################################################

###Generate maps with aWhere data from daily FTP tranfer
##No instalation of aWhere R package necessary
##For WBG staff, contact Caroline for assistance (csartoratosilvaf@worldbank.org)  

#########################################################################
#########################################################################

library(sf)
library(dplyr)
library(tmap)

#set country/countries
ReportCountries <- c("Malawi")

#load data from aWhere .csv file (make sure you have aWhere .csv sample files)
past30_all <- read.csv("C:/Users/wb525266/OneDrive - WBG/2_AgObservatory/aWhere/aWhere CSV data/20180405_AFR/global_files/AFR/180405_AFR_past30.csv", stringsAsFactors = FALSE) 
past30_all <- mutate(past30_all, country = getCoordinateInfo(points = past30_all[,c(3,2)], attribute = "country"))

#filter by report country and convert to sf object
past30_all$shapewkt <- as.character(past30_all$shapewkt)
past30_country <- st_as_sf(filter(past30_all, country %in% ReportCountries), wkt = "shapewkt") 

#set CRS for sf objects
st_crs(past30_country) = 4326

#set mapping mode
tmap_mode("view")

#map in tmap
tmap::tm_shape(past30_country) +
  tmap::tm_fill("CSUMPRE", palette = RColorBrewer::brewer.pal(4, "RdYlBu"), 
                id = "locationid", title = "Precipitation ", auto.palette.mapping = FALSE) +
  tmap::tm_style_gray()

