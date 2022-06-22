## Test script to learn git with R Studio 
library(ggplot2)
library(tidyverse) 


## Creating data in R
# First, lets create some pseudo sets of data

a <- c(1,2,3,4,5)                                    # The 'c' function combines values in to a vector or list 
b <- c('d','e','f','g','h')                          # you can combine letters, numbers
c <- c(T,F,T,F,T)                                    # Or booleans can be put into a vector
?c                                                   # If you don't know what something does

# We can actually combine this data in a really convenient way using data frames
test.df <- data.frame('numbers'= a,'letters' = b,'Boolean' = c)                 # Notice that I'm giving the columns whatever name I want 
View(test.df)                                                                   # We can View the data using View()
test.df.2 <- data.frame('letters'= a, 'Boolean' = b, 'numbers' = c)             # Note that this obviously still works but doesn't make sense
View(test.df.2) 

# BUT this type of data is somewhat limited... because the number of rows that you have must be equal (regardless of the data type)
x <- seq(from=1,to=9,by=2)
x
?seq


# Think of this like an excel spreadsheet. We can even view the data that we have in here! 
View(test.df)

which(test.df$letters == 'f')

test.df$letters[3]

which(test.df$numbers>1)

test.df$thresh <- NA

test.df$thresh <- ifelse(test.df$numbers > 2,1,0)

Fr.data <- read.csv('Table_4_FR.csv')

sum(test.df$thresh ==1 )



zero.flow.days <- sum(ifelse(na.omit(Fr.data$meandaily_CFS)==0,1,0))

length.record <- length(na.omit(Fr.data$meandaily_CFS))

percent.flowing <- zero.flow.days/length.record*100

ggplot()

# Where to find some cool packages: https://cran.r-project.org/web/views/Hydrology.html

# Downloading hydrology data:
install.packages('waterData')
??waterData

# For waterData I need an 8 digit identifier - the id for middle fork of beargrass creek at old cannons is 03293000
# we also need numbers which identify the parameter we want to download 00060 - discharge 00095 - sp cond 00400 - pH 
# We also want the daily mean which is found from statistics code 00003
library(waterData)

middle.fork.discharge <- importDVs('03293000', code ='00060', stat = '00003', sdate='2020-01-01', edate='2022-01-01')
View(middle.fork.discharge)

# I don't like these column names 
names(middle.fork.discharge) <- c('ID', 'flow_cfs','date', 'acceptance')

middle.fork.cond <- importDVs('03293000', code ='00095', stat = '00003', sdate='2020-01-01', edate='2022-01-01')
View(middle.fork.cond)
names(middle.fork.discharge) <- c('ID_cond', 'cond','date_cond', 'acceptance_cond')

# Can I combine the two? 
middle.fork <- cbind(middle.fork.cond,middle.fork.discharge)
