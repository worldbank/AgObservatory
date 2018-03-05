#inputs to function:
##lat: latitude, numeric
##lon: longitude, numeric
##day_start: character "YYYY-MM-DD"
##day_end: character "YYYY-MM-DD"
##year_start: numeric "YYYY"
##year_end: numeric "YYYY"

generateaWhereDataset <- function(lat, lon, day_start, day_end, year_start, year_end) {
  
  #set month-day combos to pull LTN for. If more than one year of data is requested, then
  #Jan 1-December 31 is set. Otherwise, the span is set based on the date span of the query.
  if((as.numeric(as.Date(day_end)) - as.numeric(as.Date(day_start))) < 365) {
    monthday_start <- substr(day_start, 6, 10)
    monthday_end <- substr(day_end, 6, 10)
  } else {
    monthday_start <- "01-01"
    monthday_end <- "12-31"
  }
  
  #if forecast data is requested, set an interim day_end for obs/ag queries
  if((day_end <= (Sys.Date()-1)) == TRUE) {
    interim_day_end <- day_end
  } else {
    interim_day_end <- as.character((Sys.Date()-1))
  }
  
  #pull daily weather data for determined time period
  obs <- daily_observed_latlng(lat, lon, day_start, day_end = interim_day_end) %>% 
    cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
    mutate(day = paste0(X2, "-", X3))%>% 
    dplyr::select(-c(X1,X2,X3))
  
  #simplify column names
  names(obs)[grep("temperatures.max", names(obs))] <- "maxTemp"
  names(obs)[grep("temperatures.min", names(obs))] <- "minTemp"
  names(obs)[grep("precipitation.amount", names(obs))] <- "precip"
  
  #pull agronomic data for time period
  ag <- agronomic_values_latlng(lat, lon, day_start, interim_day_end) %>% 
    cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
    mutate(day = paste0(X2, "-", X3)) %>% 
    dplyr::select(-c(X1,X2,X3))
  
  #pull LTN observed weather
  obs_ltn <- weather_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
  
  #pull LTN agronomic
  ag_ltn <- agronomic_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
  
  
  if((day_end <= (Sys.Date()-1)) == TRUE) {
    
    #create final dataset
      weather_full <- merge(obs, ag, by = c("date", "day")) %>% 
        merge(., obs_ltn,            by = "day") %>% 
        merge(., ag_ltn,             by = "day")   %>% 
        .[order(.$date),] %>%
        dplyr::mutate(latitude = lat,
                      longitude = lon,
                      accumulatedPrecipitation.average = cumsum(precipitation.average),
                      accumulatedPet.average = cumsum(pet.average),
                      accumulatedPpet.average = cumsum(ppet.average)) %>%
        dplyr::select(day, date, latitude, longitude, maxTemp, maxTemp.average, maxTemp.stdDev, minTemp, 
                      minTemp.average, minTemp.stdDev, precip, precipitation.average, precipitation.stdDev,
                      accumulatedPrecipitation.amount, accumulatedPrecipitation.average, accumulatedPrecipitation.stdDev,
                      gdd, gdd.average, gdd.stdDev, pet.amount, pet.average, pet.stdDev, accumulatedPet.amount, 
                      accumulatedPet.average, accumulatedPet.stdDev, ppet, ppet.average, ppet.stdDev,
                      accumulatedPpet, accumulatedPpet.average, accumulatedPpet.stdDev)
      
      names(weather_full)[grep("latitude.x", names(weather_full))] <- "latitude"
      names(weather_full)[grep("longitude.x", names(weather_full))] <- "longitude"
    
    } else {
      
      #pull forecasted data for determined time period
      forecast <- forecasts_latlng(lat, lon, day_end = day_end, block_size = 24) %>% 
        mutate(date = substr(.$startTime, 1, 10)) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        #create month-day combo column
        mutate(day = paste0(X2, "-", X3))%>% 
        dplyr::select(-c(X1,X2,X3)) 
      
      #create final dataset
      
      weather_full <- merge(obs, ag, by = c("date", 'day')) %>%
        merge(., forecast,       by = c("date", "day"), all = TRUE) %>% 
        merge(., obs_ltn,    by = "day")  %>%  
        merge(., ag_ltn,  by = "day") %>%
        .[order(.$date),] %>% 
        dplyr::mutate(latitude = lat,
                      longitude = lon,
                      maxTemp = ifelse(!is.na(maxTemp), maxTemp, temperatures.max),
                      minTemp = ifelse(!is.na(minTemp), minTemp, temperatures.min),
                      precip = ifelse(!is.na(precip), precip, precipitation.amount),
                      accumulatedPrecipitation.amount = cumsum(precip),
                      accumulatedPrecipitation.average = cumsum(precipitation.average),
                      accumulatedPet.average = cumsum(pet.average),
                      accumulatedPpet.average = cumsum(ppet.average)) %>% 
        dplyr::select(day, date, latitude, longitude, maxTemp, maxTemp.average, maxTemp.stdDev, minTemp, 
                      minTemp.average, minTemp.stdDev, precip, precipitation.average, precipitation.stdDev, 
                      accumulatedPrecipitation.amount, accumulatedPrecipitation.average, accumulatedPrecipitation.stdDev,
                      gdd, gdd.average, gdd.stdDev, pet.amount, pet.average, pet.stdDev, accumulatedPet.amount, 
                      accumulatedPet.average, accumulatedPet.stdDev, ppet, ppet.average, ppet.stdDev,
                      accumulatedPpet, accumulatedPpet.average, accumulatedPpet.stdDev)
      
      
    }
  return(weather_full)
}