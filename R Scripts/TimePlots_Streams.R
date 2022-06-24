library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)
library(naniar)

#visualize stream sulfate/nitrate/pH 
#read in stream data for given site

site <- read.csv("FRChem.csv")
site <- read.csv("LMChem.csv")
site <- read.csv("ColesChem.csv")
site <- read.csv("ClemonsChem.csv")


#identify date range

site_dates <- as.Date(c("1970-01-01", "2019-12-31"))


site <- site %>%
	mutate(Date = as.Date(DATE))


setPDF(layout = "landscape", basename = "Stream_TimePlots_Clemons")

timePlot(site$Date, site$NO3, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Nitrate Concentration", 
  caption = "Nitrates in streamwater, Clemons", margin = c(NA, NA, NA, NA))

timePlot(site$Date, site$SO4, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Sulfate Concentration", 
  caption = "Sulfates in streamwater, Clemons", margin = c(NA, NA, NA, NA))

timePlot(site$Date, site$pH, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "pH", 
  caption = "pH of streamwater, Clemons", margin = c(NA, NA, NA, NA))

dev.off()

#plotting base cations

setPDF(layout = "landscape", basename = "Stream_TimePlots_Cations_Coles")

timePlot(site$Date, site$Mg, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Magnesium Concentration", 
  caption = "Magnesium in streamwater, Coles", margin = c(NA, NA, NA, NA))

timePlot(site$Date, site$Ca, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Calcium Concentration", 
  caption = "Calcium in streamwater, Coles", margin = c(NA, NA, NA, NA))

timePlot(site$Date, site$K, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Potassium Concentration", 
  caption = "Potassium in streamwater, Coles", margin = c(NA, NA, NA, NA))

timePlot(site$Date, site$Na, Plot = list(name = "",
  what = "lines", type = "solid", width = "standard", symbol = "circle", 
  filled = TRUE, size =  0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(0, NA), xaxis.range = range(site_dates),
  ylabels = 7, xlabels = "Auto", xtitle = "Year", ytitle = "Sodium Concentration", 
  caption = "Sodium in streamwater, Coles", margin = c(NA, NA, NA, NA))

dev.off()

