library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)
library(naniar)

#linear regression of stream chemistry to precip chemistry

#Nitrates
site <- read.csv("FRChem.csv")
streamNO3 <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(streamNO3 = median(NO3, na.rm = TRUE))
streamNO3$Date <- as.Date(as.yearmon(streamNO3$ym))

precip <- read.csv("Table_2_CAMP_precip.csv")
precipNO3 <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(precipNO3 = median(NO3, na.rm = TRUE))
precipNO3$Date <- as.Date(as.yearmon(precipNO3$ym))

NO3_all <- left_join(streamNO3, precipNO3, by = c("ym"))

NO3_c <- NO3_all %>%
	replace_with_na(replace = list(streamNO3 = 0)) %>%
	replace_with_na(replace = list(precipNO3 = 0))

NO3_c <- NO3_c %>%
	mutate(logprecipNO3 = log(precipNO3)) %>%
	mutate(logstreamNO3 = log(streamNO3))

xyPlot(NO3_c$logprecipNO3, NO3_c$logstreamNO3, Plot = list(name = "", what =
  "points", type = "solid", width = "standard", symbol = "circle", filled =
  TRUE, size = 0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(NA, NA), xaxis.log = FALSE, xaxis.range = c(NA, NA),
  ylabels = 7, xlabels = 7, xtitle = "Nitrates in Precipitation, Camp Station",
  ytitle = "Nitrates in Streamwater, Falling Rock", caption = "Simple Regression, Stream vs. Precip", margin = c(NA, NA, NA,
  NA))

#Sulfates
site <- read.csv("FRChem.csv")
streamSO4 <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(streamSO4 = median(SO4, na.rm = TRUE))
streamSO4$Date <- as.Date(as.yearmon(streamSO4$ym))

precip <- read.csv("Table_2_CAMP_precip.csv")
precipSO4 <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(precipSO4 = median(SO4, na.rm = TRUE))
precipSO4$Date <- as.Date(as.yearmon(precipSO4$ym))

SO4_all <- left_join(streamSO4, precipSO4, by = c("ym"))

SO4_c <- SO4_all %>%
	replace_with_na(replace = list(streamSO4 = 0)) %>%
	replace_with_na(replace = list(precipSO4 = 0))

SO4_c <- SO4_c %>%
	mutate(logprecipSO4 = log(precipSO4)) %>%
	mutate(logstreamSO4 = log(streamSO4))

xyPlot(SO4_c$logprecipSO4, SO4_c$streamSO4, Plot = list(name = "", what =
  "points", type = "solid", width = "standard", symbol = "circle", filled =
  TRUE, size = 0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(NA, NA), xaxis.log = FALSE, xaxis.range = c(NA, NA),
  ylabels = 7, xlabels = 7, xtitle = "Sulfates in Precipitation, Camp Station",
  ytitle = "Sulfates in Streamwater, Falling Rock", caption = "Simple Regression, Stream vs. Precip", margin = c(NA, NA, NA,
  NA))

#pH
site <- read.csv("FRChem.csv")
streampH <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(streampH = median(SO4, na.rm = TRUE))
streampH$Date <- as.Date(as.yearmon(streampH$ym))

precip <- read.csv("Table_2_CAMP_precip.csv")
precippH <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(precippH = median(pH, na.rm = TRUE))
precippH$Date <- as.Date(as.yearmon(precippH$ym))

pH_all <- left_join(streampH, precippH, by = c("ym"))

xyPlot(pH_all$precippH, pH_all$streampH, Plot = list(name = "", what =
  "points", type = "solid", width = "standard", symbol = "circle", filled =
  TRUE, size = 0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(NA, NA), xaxis.log = FALSE, xaxis.range = c(NA, NA),
  ylabels = 7, xlabels = 7, xtitle = "pH of Precipitation, Camp Station",
  ytitle = "pH of Streamwater, Falling Rock", caption = "Simple Regression, Stream vs. Precip", margin = c(NA, NA, NA,
  NA))



#Try with NADP data

NADP <- read.csv("NADP_NTN-KY22-w.csv")

NADP <- NADP %>%
	mutate(Date = as.Date(dateoff)) %>%
	replace_with_na(replace = list(SO4 = -9)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(NADPSO4 = median(SO4, na.rm = TRUE))
	NADP$Date <- as.Date(as.yearmon(NADP$ym))

SO4_c <- left_join(SO4_c, NADP, by = c("ym"))

SO4_c <- SO4_c %>%
	mutate(logprecipSO4 = log(precipSO4)) %>%
	mutate(logstreamSO4 = log(streamSO4)) %>%
	mutate(logNADPSO4 = log(NADPSO4))


xyPlot(SO4_c$logNADPSO4, SO4_c$logstreamSO4, Plot = list(name = "", what =
  "points", type = "solid", width = "standard", symbol = "circle", filled =
  TRUE, size = 0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(NA, NA), xaxis.log = FALSE, xaxis.range = c(NA, NA),
  ylabels = 7, xlabels = 7, xtitle = "Sulfates in Precipitation, NADP",
  ytitle = "Sulfates in Streamwater, Falling Rock", caption = "Simple Regression, Stream vs. Precip", margin = c(NA, NA, NA,
  NA))
