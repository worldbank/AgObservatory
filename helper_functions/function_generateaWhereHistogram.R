##inputs to function:
#data: (data frame) takes a dataset that contains multiple observations of a variable as an input - observations can be across time or locations
#variable: (character string) Variable to chart - must be spelled exactly the same as in the data
#compare: (logical) Set to true to chart a second variable (compare_var)
#compare_var: (character string) Name of the second variable to chart
#xlabel: (character string) Label to give the the x axis of the chart (optional)
#title: (character string) Title to assign to the plot - default is the date span for the Current data (optional)
###dependencies: ggplot2, ggthemes, tidyr, zoo

list.of.packages = c("ggplot2", "ggthemes", "tidyr", "zoo")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  stop(paste0("This function requires package ", new.packages))
}

## load necessary libraries
library(ggplot2)
library(ggthemes)
library(tidyr)
library(zoo)


#generate charts
generateaWhereHistogram <- function(data, variable, 
                                    compare = FALSE,
                                    compare_var = NULL,
                                    xlabel = NULL,
                                    title = NULL) {
  
  
    #if title is not given by user, set it to date range + variable
    if (is.null(title)) {
      title <- paste0("Aggregated aWhere Data - ", variable)
    }
  
    #filter out relevant data
  
    if (compare == TRUE) {
      chart_data <- data[, c("latitude", "longitude", variable, compare_var)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable, compare_var)]
      chart_data <- setNames(chart_data, c("place", "Current", "LTN"))
    } else {
      chart_data <- data[, c("latitude", "longitude", variable)]
      chart_data$place <- paste0(chart_data$latitude, ", ", chart_data$longitude)
      chart_data <- chart_data[, c("place", variable)]
      chart_data <- setNames(chart_data, c("place", "Current"))
    }
  
    #set data format as long
    chart_data <- tidyr::gather(chart_data, 
                                key = Variable, 
                                value = measure, 
                                2:ncol(chart_data))
    
    #set common names of columns and xlabel (if not specified)
    if (is.null(xlabel)) {
      xlabel <- variable
    }

    
  
    #set color scale based on # of vars to chart
    if(length(unique(chart_data$Variable)) == 2) {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC", "#FF810E")) 
    } else {
      colorScaleToUse <- scale_colour_manual(values = c("#1696AC")) 
    } 
  
    if(length(unique(chart_data$Variable)) == 2) {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4", "#FF810E")) 
    } else {
      colorFillToUse <- scale_fill_manual(values = c("#1F83B4")) 
    } 
  
    #set x axis scale
    xScale <- scale_x_continuous(breaks = seq(from = min(chart_data$measure), 
                                              to = max(chart_data$measure),
                                              by = as.integer((as.integer(max(chart_data$measure)) 
                                                               - as.integer(min(chart_data$measure)))/10)))
    
    #make chart

    chart <- ggplot() + 
      theme_igray() + 
      geom_histogram(data = chart_data, 
                     aes(measure,
                         col = Variable, 
                         fill = Variable),
                     position = 'identity',
                     bins = 40,
                     alpha = 0.4) +
      xScale +
      theme(legend.position="bottom", legend.direction="horizontal",
             legend.title = element_blank()) +
      labs(x=xlabel, y = "Count of Locations") +
      ggtitle(title)
    
    return(chart)
}