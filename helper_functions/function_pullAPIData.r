#inputs to function:
##lat: latitude, numeric
##lon: longitude, numeric
##day_start: character "YYYY-MM-DD"
##day_end: character "YYYY-MM-DD"
##year_start: numeric "YYYY"
##year_end: numeric "YYYY"

generateaWhereDataset <- function(lat, lon, day_start, day_end, year_start, year_end) {
  if((day_end <= (Sys.Date()-1)) == TRUE) {
      monthday_start <- substr(day_start, 6, 10)
      monthday_end <- substr(day_end, 6, 10)
      
      #pull daily weather data for determined time period
      obs <- daily_observed_latlng(lat, lon, day_start, day_end) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        mutate(day = paste0(X2, "-", X3))%>% 
        select(-c(X1,X2,X3))
      
      #simplify column names
      names(obs)[grep("temperatures.max", names(obs))] <- "maxTemp"
      names(obs)[grep("temperatures.min", names(obs))] <- "minTemp"
      names(obs)[grep("precipitation.amount", names(obs))] <- "precip"
  
      #pull agronomic datafor time period
      ag <- agronomic_values_latlng(lat, lon, day_start, day_end) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        mutate(day = paste0(X2, "-", X3)) %>% 
        select(-c(X1,X2,X3))
      
      #pull LTN observed weather
      obs_ltn <- weather_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
      
      #pull LTN agronomic
      ag_ltn <- agronomic_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
  
      #create final dataset
      
      weather_full <- merge(obs, ag, by = "day") %>% 
        merge(., obs_ltn,            by = "day") %>% 
        merge(., ag_ltn,             by = "day")  %>%  
        select(day, date.x, latitude.x, longitude.x, maxTemp, maxTemp.average, minTemp, minTemp.average, 
               precip, precipitation.average, accumulatedPrecipitation.amount, accumulatedPrecipitation.average,
               gdd, gdd.average, pet.amount, pet.average, accumulatedPet.amount, accumulatedPet.average,
               ppet, ppet.average, accumulatedPpet, accumulatedPpet.average)
      
      names(weather_full)[grep("date.x", names(weather_full))] <- "date"
      names(weather_full)[grep("latitude.x", names(weather_full))] <- "latitude"
      names(weather_full)[grep("longitude.x", names(weather_full))] <- "longitude"
    
    } else {
      monthday_start <- substr(day_start, 6, 10)
      monthday_end <- substr(day_end, 6, 10)

      #pull daily weather data for determined time period
      obs <- daily_observed_latlng(lat, lon, day_start, as.character(Sys.Date()-1)) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        mutate(day = paste0(X2, "-", X3))%>% 
        select(-c(X1,X2,X3))
      
      #simplify column names
      names(obs)[grep("temperatures.max", names(obs))] <- "maxTemp"
      names(obs)[grep("temperatures.min", names(obs))] <- "minTemp"
      names(obs)[grep("precipitation.amount", names(obs))] <- "precip"
      
      #pull forecasted data for determined time period
      forecast <- forecasts_latlng(lat, lon, day_end = day_end, block_size = 24) %>% 
        mutate(date = substr(.$startTime, 1, 10)) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        #create month-day combo column
        mutate(day = paste0(X2, "-", X3))%>% 
        select(-c(X1,X2,X3)) %>% 
        #create "date" column with year
        mutate(date.y = substr(.$startTime, 1, 10))
      
      names(forecast)[grep("temperatures.max", names(forecast))] <- "maxTemp.forecast"
      names(forecast)[grep("temperatures.min", names(forecast))] <- "minTemp.forecast"
      names(forecast)[grep("precipitation.amount", names(forecast))] <- "precip.forecast"
      
      #pull agronomic data for time period
      ag <- agronomic_values_latlng(lat, lon, day_start, as.character(Sys.Date()-1)) %>% 
        cbind(., data.frame(do.call(rbind, strsplit(.$date, "-")))) %>% 
        mutate(day = paste0(X2, "-", X3)) %>% 
        select(-c(X1,X2,X3))
      
      #pull LTN observed weather
      obs_ltn <- weather_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
      
      #pull LTN agronomic
      ag_ltn <- agronomic_norms_latlng(lat, lon, monthday_start, monthday_end, year_start, year_end)
      
      #create final dataset
      
      weather_full <- merge(obs_ltn, ag_ltn, by = "day") %>%
        left_join(., obs,       by = "day") %>% 
        left_join(., ag,        by = "day")  %>%  
        left_join(., forecast,  by = "day") %>% 
        mutate(date = ifelse(!is.na(date.x), date.x, date.y.y)) %>% 
        select(day, date, latitude.x, longitude.x, maxTemp, maxTemp.average, maxTemp.forecast, minTemp, minTemp.average, 
               minTemp.forecast, precip, precipitation.average, precip.forecast, accumulatedPrecipitation.amount, 
               accumulatedPrecipitation.average, gdd, gdd.average, pet.amount, pet.average, accumulatedPet.amount, 
               accumulatedPet.average, ppet, ppet.average, accumulatedPpet, accumulatedPpet.average)
      
      names(weather_full)[grep("latitude.x", names(weather_full))] <- "latitude"
      names(weather_full)[grep("longitude.x", names(weather_full))] <- "longitude"
    }
  return(weather_full)
}