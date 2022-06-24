#Output boxplots of monthly precip totals 

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)


#read in precip quantity data

precip <- read.csv("PrecipQuantity.csv")

#add variable compiling Date from yr, mon, day

precip$Date <- ymd(paste(precip$yr, precip$mon, precip$day, sep = "-"))

#add rows for missing days

precip_c <- complete(precip, Date = seq.Date(as.Date("1971-01-01"), 
	as.Date("2018-12-31"), by = "day"))

#"yr"    "mon"   "day"   "LM"    "FR"    "CAMP"  "RIDGE" "HFR"   "MSC"

LM_mean <- function(){

#aggregate data by month, permits calculation of monthly means/sums, or
#setting up as time series 

LM_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_mean = mean(LM))

#extract month from "%Y-%m" data to set up for boxplots

LM_values$months <- as.yearmon(LM_values$ym)
LM_months <- LM_values %>% mutate(months = month(LM_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Mean Precip","LM", sep = "_"))

boxPlot(LM_months$p_mean, group = LM_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Mean Monthly Precipitation (in)", 
	caption = paste("Mean Monthly Precipitation","LM", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}

FR_sum <- function(){

#aggregate data by month, to set up for regular time series 

FR_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_sum = sum(FR))

#extract month from "%Y-%m" data to set up for boxplots

FR_values$months <- as.yearmon(FR_values$ym)
FR_months <- FR_values %>% mutate(months = month(FR_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Precip Total","FR", sep = "_"))

boxPlot(FR_months$p_sum, group = FR_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Total Monthly Precipitation (in)", 
	caption = paste("Total Monthly Precipitation","FR", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}

CAMP_sum <- function(){

#aggregate data by month, to set up for regular time series 

CAMP_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_sum = sum(LM))

#extract month from "%Y-%m" data to set up for boxplots

CAMP_values$months <- as.yearmon(CAMP_values$ym)
CAMP_months <- LM_values %>% mutate(months = month(LM_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Precip Total","CAMP", sep = "_"))

boxPlot(CAMP_months$p_sum, group = CAMP_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Total Monthly Precipitation (in)", 
	caption = paste("Total Monthly Precipitation","CAMP", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}

RIDGE_sum <- function(){

#aggregate data by month, to set up for regular time series 

RIDGE_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_sum = sum(RIDGE))

#extract month from "%Y-%m" data to set up for boxplots

RIDGE_values$months <- as.yearmon(RIDGE_values$ym)
RIDGE_months <- RIDGE_values %>% mutate(months = month(RIDGE_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Precip Total","RIDGE", sep = "_"))

boxPlot(RIDGE_months$p_sum, group = RIDGE_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Total Monthly Precipitation (in)", 
	caption = paste("Total Monthly Precipitation","LM", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}

RIDGE_sum <- function(){

#aggregate data by month, to set up for regular time series 

HFR_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_sum = sum(HFR))

#extract month from "%Y-%m" data to set up for boxplots

HFR_values$months <- as.yearmon(HFR_values$ym)
HFR_months <- HFR_values %>% mutate(months = month(HFR_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Precip Total","HFR", sep = "_"))

boxPlot(HFR_months$p_sum, group = HFR_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Total Monthly Precipitation (in)", 
	caption = paste("Total Monthly Precipitation","HFR", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}

MSC_sum <- function(){

#aggregate data by month, to set up for regular time series 

MSC_values <<- precip_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(p_sum = sum(MSC))

#extract month from "%Y-%m" data to set up for boxplots

MSC_values$months <- as.yearmon(MSC_values$ym)
MSC_months <- MSC_values %>% mutate(months = month(MSC_values$months))

#plot monthly data

setPDF(list(width = 2, height = 3), basename = paste("Precip Total","MSC", sep = "_"))

boxPlot(MSC_months$p_sum, group = MSC_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Total Monthly Precipitation (in)", 
	caption = paste("Total Monthly Precipitation","MSC", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()
}


