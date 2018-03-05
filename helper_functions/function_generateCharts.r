#inputs to function:
##dataset: data frame/data table
##title: character string


#define mutiplot helper function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL, title) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout)+1, ncol(layout), heights = unit(c(1, 4, 4), "null"))))
    grid.text(title, gp = gpar(fontsize=18), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2, gp = gpar(fill="gray")))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row +1,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#generate charts
generateCharts <- function(df) {
#it is controling for not generate chart with forecast
  if((day_end <= (Sys.Date()-1)) == TRUE) {
    
    #make precip chart
    chart_data <- select(weather_df, date, accumulatedPrecipitation.amount, accumulatedPrecipitation.average) %>%
    setNames(., c("date", "Current", "LTN")) %>% 
    gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    precip <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "mm \n") +
      ggtitle(paste("Accumulated Precipitation"))
    
    #make PET chart
    chart_data <- select(weather_df, date, accumulatedPet.amount, accumulatedPet.average) %>%
      setNames(., c("date", "Current", "LTN")) %>% 
      gather(key = date, value = measure) 
    names(chart_data)[2] <- "Variable"
    
    pet <- ggplot() + theme_igray() + scale_colour_manual(values = c("#1F83B4", "#29A03C")) +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "mm \n") +
      ggtitle(paste("PET"))
    
    #make maxTemp chart
    chart_data <- select(weather_df, date, maxTemp, maxTemp.average) %>%
      setNames(., c("date", "Current", "LTN")) %>% 
      gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    maxtemp <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "Celsius \n") +
      ggtitle(paste("Max Temperature"))
    
    #make minTemp chart
    chart_data <- select(weather_df, date, minTemp, minTemp.average) %>%
      setNames(., c("date", "Current", "LTN")) %>% 
      gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    mintemp <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "Celsius \n") +
      ggtitle(paste("Min Temperature"))
    
    multiplot(precip, pet, maxtemp, mintemp, cols = 2, 
              title = paste0("Current vs Long-Term Norms in ", placename, " (", lat, ", ", 
                             lon, ")", " From ", head(sort(weather_df$date), n=1), " to ", 
                             tail(sort(weather_df$date), n=1)))
    
  } else {
    
    #forecast charts
    #make precip chart
    chart_data <- select(weather_df, date, accumulatedPrecipitation.amount, accumulatedPrecipitation.average, precip.forecast)
    
    miss <- is.na(chart_data$precip.forecast)
    chart_data$precip.forecast[miss] <- 0
    chart_data$precip.forecast<- cumsum(chart_data$precip.forecast)
    chart_data$precip.forecast[miss] <- NA
    
    chart_data <- chart_data %>% 
      mutate(forecast = max(.$accumulatedPrecipitation.amount, na.rm = TRUE) + precip.forecast) %>% 
      select(-precip.forecast) %>% 
      setNames(., c("date", "Current", "LTN", "Forecasted")) %>% 
      gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    precip <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "mm \n") +
      ggtitle(paste("Accumulated Precipitation"))
    
    
    #make PET chart
    chart_data <- select(weather_df, date, accumulatedPet.amount, accumulatedPet.average) %>%
      setNames(., c("date", "Current", "LTN")) %>% 
      gather(key = date, value = measure) 
    names(chart_data)[2] <- "Variable"
    
    pet <- ggplot() + theme_igray() + scale_colour_manual(values = c("#1F83B4", "#29A03C")) +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "mm \n") +
      ggtitle(paste("PET"))
    
    
    #make maxTemp chart
    chart_data <- select(weather_df, date, maxTemp, maxTemp.average, maxTemp.forecast) %>%
      setNames(., c("date", "Current", "LTN", "Forecasted")) %>% 
      gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    maxtemp <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "Celsius \n") +
      ggtitle(paste("Max Temperature"))
    
    #make minTemp chart
    chart_data <- select(weather_df, date, minTemp, minTemp.average, minTemp.forecast) %>%
      setNames(., c("date", "Current", "LTN", "Forecasted")) %>% 
      gather(key = date, value = measure) 
    
    names(chart_data)[2] <- "Variable"
    
    mintemp <- ggplot() + theme_igray() + scale_colour_tableau() +
      geom_line(data = chart_data, 
                aes(x = date, 
                    y = measure, 
                    group = Variable,
                    color = Variable),
                size = 1.5) + 
      scale_x_discrete(breaks = unique(chart_data$date)[seq(1,length(chart_data$date),10)],
                       labels = unique(substr(chart_data$date, 6, 10))[seq(1,length(chart_data$date),10)]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      labs(x="Date", y = "Celsius \n") +
      ggtitle(paste("Min Temperature"))
    
    multiplot(precip, pet, maxtemp, mintemp, cols = 2, 
              title = paste0("Current vs Long-Term Norms in ", placename, " (", lat, ", ", 
                             lon, ")", " From ", head(sort(weather_df$date), n=1), " to ", 
                             tail(sort(weather_df$date), n=1)))
  }
}