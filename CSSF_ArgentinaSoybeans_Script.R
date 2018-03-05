#########################################################################
#########################################################################

### GENERATE HISTOGRAMS WITH aWHERE DATA
## Consult Hanna for assistance if errors occur
## Modified by Caroline SS Franca

#########################################################################
#########################################################################

#install.packages('devtools')
#install.packages(c('chron', 'magrittr', 'btcops', 'DBI', 'assertthat', 'Rcpp', 'tibble'))
#devtools::install_github("aWhereAPI/aWhere-R-Library")


#one-time install if you do not already have the zoo package
#install.packages("zoo")

library(ggplot2)
library(ggthemes)
library(zoo)
library(aWhereAPI)

#set working environment
setwd('your directory')

#user autentication
load_credentials("./credentials.txt") # if credential.txt file is in your directory

#source helper functions
source("./function_generateaWhereHistogram.R") # if function_generateaWhereHistogram.R is in your directory


Arg_past30 <- read.csv(" (add path to the Argentina Case Study folder) /Argentina_past30_180302.csv")

generateaWhereHistogram(data = Arg_past30, variable = "CSUMPRE",
                        compare = TRUE, compare_var = "LTNSUMP", xlabel = "mm",
                        title = "Total Precipitation by Location (41185) for Argentina - February,2018")


ArgSoy_past30 <- read.csv("(add path to the Argentina Case Study folder)/Arg_Soy_past30_180302.csv")

generateaWhereHistogram(data = ArgSoy_past30, variable = "CSUMPRE",
                        compare = TRUE, compare_var = "LTNSUMP", xlabel = "mm",
                        title = "Total Precipitation by Location (14974)  for Argentina Soybeans (all areas) - February, 2018")


ArgSoy80_past30 <- read.csv("(add path to the Argentina Case Study folder)/Arg_Soy80_past30_180302.csv")

generateaWhereHistogram(data = ArgSoy80_past30, variable = "CSUMPRE",
                        compare = TRUE, compare_var = "LTNSUMP", xlabel = "mm",
                        title = "Total Precipitation by Location (4493) for Argentina Soybeans (>80% production) - February, 2018")


ArgSoy80Cor_past30 <- read.csv("(add path to the Argentina Case Study folder) /Arg_Cordoba_Soy80_past30_180302.csv")

generateaWhereHistogram(data = ArgSoy80Cor_past30, variable = "CSUMPRE",
                        compare = TRUE, compare_var = "LTNSUMP", xlabel = "mm",
                        title = "Total Precipitation by Location (1322) for Cordoba Soybeans (>80% production) - February, 2018")


ArgSoy80SF_past30 <- read.csv("(add path to the Argentina Case Study folder)/Arg_SantaFe_Soy80_past30_180302.csv")

generateaWhereHistogram(data = ArgSoy80SF_past30, variable = "CSUMPRE",
                        compare = TRUE, compare_var = "LTNSUMP", xlabel = "mm",
                        title = "Total Precipitation by Location (865) for Santa Fe Soybeans (>80% production) - February, 2018")


#########################################################################
# END ###################################################################
#########################################################################