##inputs to function:
#data: (data frame) must be in format like that resulting from aWhere API query
#variable: (character string) Variable to chart - accepted values are precip, accprecip, maxTemp, minTemp, pet, accpet, ppet, or rollingavgppet
#title: (character string) Title to assign to the plot (optional)
#e_precip: (logical) If True, effective precipitation will be calculated and charted based on e_threshold
#e_threshold: (numeric) The daily cap used to calculate effective precip if e_precip is set to TRUE. (mm)
#rolling_window: (numeric) Number of days to use in rolling average calculation
###dependencies: tidyr, zoo

#generate charts
generateaWhereStdDevChart <- function(data, variable, 
                                title = NULL, 
                                e_precip = FALSE, 
                                e_threshold = 35, 
                                rolling_window = 30) {
  
  ##determine vars to chart
  if(variable == "precip") {
    varsToChart <- c("precip", "precipitation.average", "precipitation.stdDev")  
    ylabel <- "mm"
  } else if(variable == "accprecip") {
    varsToChart <- c("accumulatedPrecipitation.amount", "accumulatedPrecipitation.average", "accumulatedPrecipitation.stdDev")    
    ylabel <- "mm"
  } else if(variable == "maxTemp") {
    varsToChart <- c("maxTemp", "maxTemp.average", "maxTemp.stdDev")    
    ylabel <- "Celsius"
  } else if(variable == "minTemp") {
    varsToChart <- c("minTemp", "minTemp.average", "minTemp.stdDev")    
    ylabel <- "Celsius"
  } else if(variable == "pet") {
    varsToChart <- c("pet.amount", "pet.average", "pet.stdDev")   
    ylabel <- "mm"
  } else if(variable == "accpet") {
    varsToChart <- c("accumulatedPet.amount", "accumulatedPet.average", "accumulatedPet.stdDev")   
    ylabel <- "mm"
  } else if(variable == "ppet") {
    varsToChart <- c("ppet", "ppet.average", "ppet.stdDev")   
    ylabel <- "mm"
  } else if(variable == "rollingavgppet") {
    varsToChart <- c("ppet", "ppet.average", "ppet.stdDev")    
    ylabel <- "mm"
  } else {
    stop("Input Variable is not from allowed list. Please use precip, accprecip, 
         maxTemp, minTemp, pet, accpet, ppet, or rollingavgppet.")
  }
  
  #filter out relevant data
  chart_data <- data[, c("date", varsToChart)]
  
  #set common names of columns
  chart_data <- setNames(chart_data, c("date", "Current", "LTN", "LTNstddev"))
  
  #separate out stdDev data into a separate dataframe to calculate ymax and ymin
  chart_data_temp <- chart_data[, c("date", "LTN", "LTNstddev")] %>% 
    dplyr::mutate(ymax = LTN + LTNstddev,
           ymin = LTN - LTNstddev,
           Variable = "LTN") %>% 
    dplyr::select(-LTN, -LTNstddev)
  
  #ensure ymin in precip or accprecip never goes below 0
  if(variable == "accprecip" | variable == "precip") {
    chart_data_temp$ymin[chart_data_temp$ymin < 0] <- 0
  }
  
  chart_data <- dplyr::select(chart_data, -LTNstddev)
  
  
  #if e_precip is set to true, bring in daily precip data and calculate accumulated
  #daily precipitation using either default or user-defined threshold
  
  if(variable == "accprecip" & e_precip == TRUE) {
    temp <- data[, c("date", "precip")]
    chart_data <- merge(chart_data, temp, by = "date")
    chart_data$precip[chart_data$precip > e_threshold ] <- e_threshold
    chart_data <- dplyr::mutate(chart_data,
                                EffectiveCurrent = cumsum(chart_data$precip))
    chart_data <- dplyr::select(chart_data, -precip)
  }
  
  #if variable is set to "rollingavgppet", bring in daily precip and pet data 
  #and calculate rolling averages. If e_precip = TRUE, add effective precip
  
  if(variable == "rollingavgppet") {
    if(e_precip == TRUE) {
      temp <- data[, c("date", "precip", "pet.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precip[chart_data$precip > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = precip/pet.amount)
      chart_data <- dplyr::select(chart_data, -precip, -pet.amount)
      
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
    chart_data_temp$ymax <-     zoo::rollapply(chart_data_temp$ymax, 
                                          width = rolling_window, 
                                          align = "right",
                                          FUN = mean, 
                                          fill = NA)
    chart_data_temp$ymin <-     zoo::rollapply(chart_data_temp$ymin, 
                                          width = rolling_window, 
                                          align = "right",
                                          FUN = mean, 
                                          fill = NA)
  }
  
  #change data format from wide to long
  chart_data <- tidyr::gather(chart_data, 
                              key = Variable, 
                              value = measure, 
                              2:ncol(chart_data))
  
  #merge in ymax/ymin data frame to master chart_data
  chart_data <- left_join(chart_data, chart_data_temp, by = c("date", "Variable"))
  
  #convert character date column to Date
  chart_data$date <- as.Date(chart_data$date)
  
  #if title is not given by user, set it to date range + variable
  if (is.null(title)) {
    title <- paste0(variable, " from ", min(data$date), " to ", max(data$date))
  }
  
  
  #set color scale based on # of vars to chart
  if(length(unique(chart_data$Variable)) == 2) {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
  } else {
    colorScaleToUse <- scale_colour_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
    colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#18A188", "#FF810E")) 
  } 
  
  #make chart
  chart <- ggplot(data = chart_data, 
                  aes(x = date, 
                      y = measure, 
                      ymax = ymax,
                      ymin = ymin,
                      group = Variable,
                      color = Variable,
                      fill = Variable),
                  na.rm = TRUE) + 
    theme_igray() + 
    colorScaleToUse +
    colorFillToUse +
    geom_line(size = 1.5) +
    geom_ribbon(alpha = 0.3, linetype = "blank") +
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