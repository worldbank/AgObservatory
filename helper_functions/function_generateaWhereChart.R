##inputs to function:
#data: (data frame) must be in format like that resulting from aWhere API query
#variable: (character string) Acceptable values are precipitation, accumulatedPrecipitation, maxTemp, minTemp, pet, accumulatedPet, ppet, or rollingavgppet
#title: (character string) Title to assign to the plot (optional)
#e_precip: (logical) If True, effective precipitation will be calculated and charted based on e_threshold
#e_threshold: (numeric) The daily cap used to calculate effective precip if e_precip is set to TRUE. (mm)
#rolling_window: (numeric) Number of days to use in rolling average calculation
###dependencies: tidyr, zoo

list.of.packages = c("tidyr", "zoo")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  stop(paste0("This function requires package ", new.packages))
}

library(tidyr)
library(zoo)

#generate charts
generateaWhereChart <- function(data, variable, 
                                title = NULL, 
                                e_precip = FALSE, 
                                e_threshold = 35, 
                                rolling_window = 30) {

    ##determine vars to chart
    if(variable %in% c("precipitation", "accumulatedPrecipitation",
                       "maxTemp", "minTemp", "pet", "accumulatedPet",
                       "ppet")) {
      
      varsToChart <- c(paste0(variable,'.amount'), paste0(variable,'.average'))    
      
    } else if(variable == "rollingavgppet") {
      
      varsToChart <- c("ppet.amount", "ppet.average")    
      
    } else {
      
      stop("Input Variable is not from allowed list. Please use precipitation, accumulatedPrecipitation, 
           maxTemp, minTemp, pet, accumulatedPet, ppet, or rollingavgppet.")
      
    }
    
    ##set ylabel
    if(variable %in% c("precipitation", "accumulatedPrecipitation",
                       "pet", "accumulatedPet")) {
      
      ylabel = "mm"
      
    } else if(variable %in% c("maxTemp", "minTemp")) {
      
      ylabel <- "Celsius"  
      
    } else {
      
      ylabel = "Index"
      
    }

    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0(variable, " from ", max(data$date), " to ", min(data$date))
    }
    
    #filter out relevant data
    chart_data <- data[, c("date", varsToChart)]
  
    #set common names of columns
    chart_data <- setNames(chart_data, c("date", "Current", "LTN"))

    #if e_precip is set to true, bring in daily precip data and calculate accumulated
    #daily precipitation using either default or user-defined threshold
    
    if(variable == "accumulatedPrecipitation" & e_precip == TRUE) {
      temp <- data[, c("date", "precipitation.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precipitation.amount[chart_data$precipitation.amount > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = cumsum(chart_data$precipitation.amount))
      chart_data <- dplyr::select(chart_data, -precipitation.amount)
    }
    
    #if variable is set to "rollingavgppet", bring in daily precip and pet data 
    #and calculate rolling averages. If e_precip = TRUE, add effective precip
    
    if(variable == "rollingavgppet") {
      if(e_precip == TRUE) {
      temp <- data[, c("date", "precipitation.amount", "pet.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precipitation.amount[chart_data$precipitation.amount > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = precipitation.amount/pet.amount)
      chart_data <- dplyr::select(chart_data, -precipitation.amount, -pet.amount)
      
      chart_data$EffectiveCurrent <- zoo::rollapply(chart_data$EffectiveCurrent, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
      
      }
      
      chart_data$Current <- zoo::rollapply(chart_data$Current, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
      
      chart_data$LTN <-     zoo::rollapply(chart_data$LTN, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
    }
    
    #convert character date column to Date
    chart_data$date <- as.Date(chart_data$date)
    
    #change data format from wide to long
    chart_data <- tidyr::gather(chart_data, 
                                key = Variable, 
                                value = measure, 
                                2:ncol(chart_data))
    
    #set color scale based on # of vars to chart
    if(length(unique(chart_data$Variable)) == 2) {
      colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#FF810E")) 
    } else {
      colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
    } 
    
    #make chart
    chart <- ggplot() + theme_igray() + colorScaleToUse +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5,
                na.rm = TRUE) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = ylabel) +
      #the next two lines may be commented out if the vertical current date line is not desired
      geom_vline(aes(xintercept = Sys.Date()),
                 linetype = "dashed") +
      ggtitle(title)
    
    return(chart)
}