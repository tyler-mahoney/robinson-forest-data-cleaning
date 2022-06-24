library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)

#in Excel, edit date to 4-digit year
#Read in streamflow data

streamflow <- read.csv("LMStreamflowCompiled.csv")

#change datetime to datetime format in R
streamflow$Time=as.POSIXct(strptime(streamflow$Time, "%m/%d/%Y %H:%M"))
streamflow <- streamflow %>% mutate(Date = as.Date(Time))

#add rows for missing days

sf_c <- complete(streamflow, Date = seq.Date
	(as.Date(min(streamflow$Date, na.rm = TRUE)), 
	as.Date(max(streamflow$Date, na.rm = TRUE)), by = "day"))

#aggregate by day; calculate daily max, min, and mean flows

d_mean <- sf_c %>% 
	group_by(Date) %>%
	summarize(dailymean = mean(Streamflow_CFS, na.rm = TRUE))
	
d_min <- sf_c %>% 
	group_by(Date) %>%
	summarize(dailymin = min(Streamflow_CFS, na.rm = TRUE))

d_max <- sf_c %>%
	group_by(Date) %>%
	summarize(dailymax = max(Streamflow_CFS, na.rm = TRUE))

sf_1 <- left_join(d_mean, d_min, by = c("Date"))
daily_sf <- left_join(sf_1, d_max, by = c("Date"))

#calculate mean stage height and corrected stage height

StageHt <- sf_c %>%
	group_by(Date) %>%
	summarize(stageht = mean(StageHt, na.rm = TRUE))

StageHtCorr <- sf_c %>%
	group_by(Date) %>%
	summarize(stagehtcorr = mean(StageHtCorr, na.rm = TRUE))

#merge 
stageht <- left_join(StageHt, StageHtCorr, by = c("Date"))

#calculate min, max, and mean temperature
TempMin <- sf_c %>%
	group_by(Date) %>%
	summarize(tempmin = min(Temperature, na.rm = FALSE))

TempMax <- sf_c %>%
	group_by(Date) %>%
	summarize(tempmax = max(Temperature, na.rm = FALSE))

TempMean <- sf_c %>%
	group_by(Date) %>%
	summarize(tempmean = mean(Temperature, na.rm = FALSE))

temp_1 <- left_join(TempMin, TempMax, by = c("Date"))
temp <- left_join(TempMean, temp_1, by = c("Date"))

#read in stream chemistry data
streamchem <- read.csv("LMChem.csv")

#add variable formatted as Date
streamchem <- streamchem %>% 
	mutate(Date = as.Date(DATE))

#add rows for missing days
daily_chem <- complete(streamchem, Date = seq.Date
	(as.Date(min(streamchem$Date, na.rm = TRUE)), 
	as.Date(max(streamchem$Date, na.rm = TRUE)), by = "day"))

#merge streamflow and stream chemistry datasets 
stream <- left_join(daily_sf, daily_chem, by = c("Date"))

#export as .csv file

write.csv(stream, "LM_sf_chem")

#merge stage height and temperature files
update <- left_join(temp, stageht, by = c("Date"))

#read in streamflow/chemistry datafile 
#merge with update datafile
Compiled <- read.csv("Table_5_LM.csv")%>%
	mutate(Date = as.Date(Date))
Updated <- left_join(Compiled, update, by = c("Date"))

#export updated 
write.csv(Updated, "LM_updated.csv")

#aggregate by month; calculate monthly max, min, and average for plotting

sf_monmax <- sf_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(sf_monthmax = max(DailyMax, na.rm = TRUE))

sf_monmin <- sf_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(sf_monthmin = min(DailyMin, na.rm = TRUE))

sf_monavg <- sf_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(sf_monthavg = mean(DailyAvg, na.rm = TRUE))

sf_maxmin <- left_join(sf_monmax, sf_monmin, by = c("ym"))
sf_summaries <- left_join(sf_maxmin, sf_monavg, by = c("ym"))

#extract month from "%Y-%m" data to set up for boxplots

sf_summaries$months <- as.yearmon(sf_summaries$ym)
sf_months <- sf_summaries %>% mutate(months = month(sf_summaries$months))


setPDF(list(width = 2, height = 3), basename = paste("Streamflow", "Clemons", "Summaries", sep = "_"))

boxPlot(sf_months$sf_monthmin, group = sf_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(0,3.5),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Monthly Minimum Streamflow", 
	caption = paste("Monthly Minimum Streamflow","Clemons", sep = "_"), margin = c(NA, NA, NA, NA))

boxPlot(sf_months$sf_monthmax, group = sf_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(0, 100),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Monthly Maximum Streamflow", 
	caption = paste("Monthly Maximum Streamflow","Clemons", sep = "_"), margin = c(NA, NA, NA, NA))

boxPlot(sf_months$sf_monthavg, group = sf_months$months, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 20, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(0, 7),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Month", ytitle = "Monthly Average Streamflow", 
	caption = paste("Monthly Average Streamflow","Clemons", sep = "_"), margin = c(NA, NA, NA, NA))

dev.off()


