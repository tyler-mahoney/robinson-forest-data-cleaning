library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)

#read in precip data

precip <- read.csv("PrecipitationChemistry_master.csv")

#add variable compiling Date 

precip$Date <- ymd(paste(precip$YR, precip$MO, precip$DAY, sep = "-"))

#add rows for missing days

precip_c <- complete(precip, Date = seq.Date(as.Date("1971-01-01"), 
	as.Date("2018-12-31"), by = "day"), SITE)

precip_1 <- subset(precip_c, subset = precip_c$SITE == 1)


NO3TS <- function(site){

#aggregate data by month, to set up for regular time series 

precip_site <- subset(precip_c, subset = precip_c$SITE == site)

NO3_values <- precip_site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(NO3_med = median(NO3, na.rm = TRUE))

#extract month from "%Y-%m" data to set up for boxplots

NO3_values$months <- as.yearmon(NO3_values$ym)
NO3_months <- NO3_values %>% mutate(months = month(NO3_values$months))

#plot monthly data to check against Kendall test results

setPDF(list(width = 2, height = 3), basename = paste("Precip", site, "NO3", sep = "_"))

boxPlot(NO3_months$NO3_med, group = NO3_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Nitrate", 
	caption = paste("Avg Nitrate by Month",site, sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()

#set up as Time Series 

NO3_values$Date <- as.Date(as.yearmon(NO3_values$ym))
NO3_TS <- ts(data = NO3_values$NO3_med, start = min(NO3_values$Date), 
	end = max(NO3_values$Date), frequency = 12)

#run seasonal Kendall test

res <- SeasonalMannKendall(NO3_TS)
print(res)

}


SO4TS <- function(site){

precip_site <- subset(precip_c, subset = precip_c$SITE == site)

SO4_values <- precip_site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(SO4_med = median(SO4, na.rm = TRUE))


SO4_values$months <- as.yearmon(SO4_values$ym)
SO4_months <- SO4_values %>% mutate(months = month(SO4_values$months))

setPDF(list(width = 2, height = 3), basename = paste("Precip", site, "SO4", sep = "_"))

boxPlot(SO4_months$SO4_med, group = SO4_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Sulfate", 
	caption = paste("Avg Sulfate by Month",site, sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()

SO4_values$Date <- as.Date(as.yearmon(SO4_values$ym))
SO4_TS <- ts(data = SO4_values$SO4_med, start = min(SO4_values$Date), 
	end = max(SO4_values$Date), frequency = 12)

res <- SeasonalMannKendall(SO4_TS)

print(res)

}

pHTS <- function(site){

precip_site <- subset(precip_c, subset = precip_c$SITE == site)

pH_values <- precip_site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(pH_med = median(pH, na.rm = TRUE))


pH_values$months <- as.yearmon(pH_values$ym)
pH_months <- pH_values %>% mutate(months = month(pH_values$months))

setPDF(list(width = 2, height = 3), basename = paste("Precip", site, "pH", sep = "_"))

boxPlot(pH_months$pH_med, group = pH_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "pH", 
	caption = paste("pH by Month",site, sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()

pH_values$Date <- as.Date(as.yearmon(pH_values$ym))
pH_TS <- ts(data = pH_values$pH_med, start = min(pH_values$Date), 
	end = max(pH_values$Date), frequency = 12)

res <- SeasonalMannKendall(pH_TS)

print(res)

}