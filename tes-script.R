## Test script to learn git with R Studio 
library(ggplot2)
Fr.data <- read.csv('C:/Users/david/OneDrive/Desktop/EPA/EPA/6 PROJECT 1 KENTUCKY HEADWATER STREAMS/3 DATA/TIME_SERIES/Table_4_FR.csv')

zero.flow.days <- sum(ifelse(na.omit(Fr.data$meandaily_CFS)==0,1,0))

length.record <- length(na.omit(Fr.data$meandaily_CFS))

percent.flowing <- zero.flow.days/length.record*100

ggplot()
