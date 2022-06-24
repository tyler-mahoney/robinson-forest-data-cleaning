#to visualize sulfate/nitrate/pH in precipitation over time
#use timePlot in smwrGraphs

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)
library(naniar)


#read in precip data for given site

site <- read.csv("Table_2_CAMP_precip.csv")
site <- read.csv("Table_3_FR_Bottom_precip.csv")
site <- read.csv("Table_4_HFR_precip.csv")
site <- read.csv("Table_5_LM_precip.csv")
site <- read.csv("Table_6_MSC_precip.csv")
site <- read.csv("Table_7_RIDGE_precip.csv")

#identify date range

site_dates <- as.Date(c("1980-01-01", "2019-12-31"))

#calculate monthly median, and add column formatted as date

NO3 <- site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(NO3_med = median(NO3, na.rm = TRUE))
NO3$Date <- as.Date(as.yearmon(NO3$ym))

SO4 <- site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(SO4_med = median(SO4, na.rm = TRUE))
SO4$Date <- as.Date(as.yearmon(SO4$ym))

pH <- site %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(pH_med = median(pH, na.rm = TRUE))
pH$Date <- as.Date(as.yearmon(pH$ym))

setPDF(layout = "landscape", basename = "Precip_TimePlots_Camp")

## Plot monthly median nitrates over time
timePlot(NO3$Date, NO3$NO3_med, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Monthly Median Nitrate Concentration", 
  caption = "Nitrates in Precipitation, Camp Station", margin = c(NA, NA, NA, NA))

#Plot monthly median sulfates over time
timePlot(SO4$Date, SO4$SO4_med, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Monthly Median Sulfate Concentration", 
  caption = "Sulfates in Precipitation, Camp Station", margin = c(NA, NA, NA, NA))

#Plot monthly median pH over time
timePlot(pH$Date, pH$pH_med, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Monthly Median precipitation pH", 
  caption = "pH of Precipitation, Camp Station", margin = c(NA, NA, NA, NA))

dev.off()

#also plot NADP data

site <- read.csv("NADP_NTN-KY22-w.csv")

NO3 <- site %>%
	mutate(Date = as.Date(dateoff)) %>%
	replace_with_na(replace = list(NO3 = -9))

SO4 <- site %>%
	mutate(Date = as.Date(dateoff)) %>%
	replace_with_na(replace = list(SO4 = -9))

pH <- site %>%
	mutate(Date = as.Date(dateoff)) %>%
	replace_with_na(replace = list(ph = -9))

setPDF(layout = "landscape", basename = "Precip_TimePlots_NADP")

timePlot(NO3$Date, NO3$NO3, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Nitrate Concentration", 
  caption = "Nitrates in Precipitation, NADP Jackson Station", margin = c(NA, NA, NA, NA))

timePlot(SO4$Date, SO4$SO4, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Sulfate Concentration", 
  caption = "Sulfates in Precipitation, NADP Jackson Station", margin = c(NA, NA, NA, NA))

timePlot(pH$Date, pH$ph, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "pH", 
  caption = "pH of Precipitation, NADP Jackson Station", margin = c(NA, NA, NA, NA))

dev.off()