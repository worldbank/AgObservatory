##inputs to function:
#data: (data frame) must be in format like that resulting from aWhere API query
#variable: (character string) Variable to chart - accepted values are precip, accprecip, maxTemp, minTemp, pet, accpet, ppet, or rollingavgppet
#title: (character string) Title to assign to the plot (optional)
#e_precip: (logical) If True, effective precipitation will be calculated and charted based on e_threshold
#e_threshold: (numeric) The daily cap used to calculate effective precip if e_precip is set to TRUE. (mm)
#rolling_window: (numeric) Number of days to use in rolling average calculation
###dependencies: tidyr, zoo

#generate charts
generateaWhereChart <- function(data, variable, 
                                title = NULL, 
                                e_precip = FALSE, 
                                e_threshold = 35, 
                                rolling_window = 30) {

    ##determine vars to chart
    if(variable == "precip") {
      varsToChart <- c("precip", "precipitation.average")  
      ylabel <- "mm"
    } else if(variable == "accprecip") {
      varsToChart <- c("accumulatedPrecipitation.amount", "accumulatedPrecipitation.average")    
      ylabel <- "mm"
    } else if(variable == "maxTemp") {
      varsToChart <- c("maxTemp", "maxTemp.average")    
      ylabel <- "Celsius"
    } else if(variable == "minTemp") {
      varsToChart <- c("minTemp", "minTemp.average")    
      ylabel <- "Celsius"
    } else if(variable == "pet") {
      varsToChart <- c("pet.amount", "pet.average")   
      ylabel <- "mm"
    } else if(variable == "accpet") {
      varsToChart <- c("accumulatedPet.amount", "accumulatedPet.average")   
      ylabel <- "mm"
    } else if(variable == "ppet") {
      varsToChart <- c("ppet", "ppet.average")   
      ylabel <- "mm"
    } else if(variable == "rollingavgppet") {
      varsToChart <- c("ppet", "ppet.average")    
      ylabel <- "mm"
    } else {
      stop("Input Variable is not from allowed list. Please use precip, accprecip, 
           maxTemp, minTemp, pet, accpet, ppet, or rollingavgppet.")
    }

    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0(variable, "from ", max(data$date), " to ", min(data$date))
    }
    
    #filter out relevant data
    chart_data <- data[, c("date", varsToChart)]
  
    #set common names of columns
    chart_data <- setNames(chart_data, c("date", "Current", "LTN"))

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
    
    if(variable == "rollingavgppet" & e_precip == TRUE) {
      temp <- data[, c("date", "precip", "pet.amount")]
      chart_data <- merge(chart_data, temp, by = "date")
      chart_data$precip[chart_data$precip > e_threshold ] <- e_threshold
      chart_data <- dplyr::mutate(chart_data,
                                  EffectiveCurrent = precip/pet.amount)
      chart_data <- dplyr::select(chart_data, -precip, -pet.amount)
      chart_data$Current <- zoo::rollapply(chart_data$Current, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
      chart_data$EffectiveCurrent <- zoo::rollapply(chart_data$EffectiveCurrent, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
      chart_data$LTN <-     zoo::rollapply(chart_data$LTN, 
                                      width = rolling_window, 
                                      align = "right",
                                      FUN = mean, 
                                      fill = NA)
    } else if(variable == "rollingavgppet" & e_precip == FALSE) {
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