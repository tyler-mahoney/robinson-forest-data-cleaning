library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)


#Read in temperature data

temp <- read.csv("RFTempData.csv")

#trim to actual data
temp <- temp[,1:6]

#add variable compiling Date
temp$Date <- ymd(paste(temp$Year, temp$Month, 
	temp$Day, sep = "-"))

#add rows for missing days

temp_c <- complete(temp, Date = seq.Date
	(as.Date(min(temp$Date, na.rm = TRUE)), 
	as.Date(max(temp$Date, na.rm = TRUE)), by = "day"))


#aggregate by month; calculate monthly max, min, and average for plotting

temp_monmax <- temp_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(temp_monthmax = max(DailyMax, na.rm = TRUE))

temp_monmin <- temp_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(temp_monthmin = min(DailyMin, na.rm = TRUE))

temp_monavg <- temp_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(temp_monthavg = mean(DailyMean, na.rm = TRUE))

temp_monminavg <- temp_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(temp_monthminavg = mean(DailyMin, na.rm = TRUE))

temp_monmaxavg <- temp_c %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(temp_monthmaxavg = mean(DailyMax, na.rm = TRUE))

temp_maxmin <- left_join(temp_monmax, temp_monmin, by = c("ym"))
temp_maxminavg <- left_join(temp_maxmin, temp_monavg, by = c("ym"))
temp_maxminnext <- left_join(temp_maxminavg, temp_monminavg, by = c("ym"))
temp_summaries <- left_join(temp_maxminnext, temp_monmaxavg, by = c("ym"))

write.csv(temp_summaries, "TempSummaries")