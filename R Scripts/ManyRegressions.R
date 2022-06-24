#perform many linear regressions to identify relationships between
#precip and stream data

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(smwrGraphs)
library(Kendall)
library(naniar)

#build single df with a column for values of a given species (e.g., NO3)
#from each stream  


#import stream data and adjust to be monthly median values of NO3
site <- read.csv("FRChem.csv")
FR_s <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(FR_s = median(NO3, na.rm = TRUE))
FR_s$Date <- as.Date(as.yearmon(FR_s$ym))

site <- read.csv("LMChem.csv")
LM_s <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(LM_s = median(NO3, na.rm = TRUE))
LM_s$Date <- as.Date(as.yearmon(LM_s$ym))

site <- read.csv("ClemonsChem.csv")
Clemons_s <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(Clemons_s = median(NO3, na.rm = TRUE))
Clemons_s$Date <- as.Date(as.yearmon(Clemons_s$ym))

site <- read.csv("ColesChem.csv")
Coles_s <- site %>%
	mutate(Date = as.Date(DATE)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(Coles_s = median(NO3, na.rm = TRUE))
Coles_s$Date <- as.Date(as.yearmon(Coles_s$ym))

#Merge datasets into single dataframe of NO3 values, with each column representing
#data from single stream

stream1 <- left_join(FR_s, LM_s, by = c("ym"))
stream2 <- left_join(stream1, Clemons_s, by = c("ym"))
stream <- left_join(stream2, Coles_s, by = c("ym"))

#remove extra Date columns 
stream <- subset(stream, select = -c(Date.y, Date.x.x, Date.y.y))

#import and process precip data 

precip <- read.csv("Table_2_CAMP_precip.csv")
Camp_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(Camp_p = median(NO3, na.rm = TRUE))
Camp_p$Date <- as.Date(as.yearmon(Camp_p$ym))

precip <- read.csv("Table_3_FR_Bottom_precip.csv")
FR_B_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(FR_B_p = median(NO3, na.rm = TRUE))
FR_B_p$Date <- as.Date(as.yearmon(FR_B_p$ym))

precip <- read.csv("Table_4_HFR_precip.csv")
HFR_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(HFR_p = median(NO3, na.rm = TRUE))
HFR_p$Date <- as.Date(as.yearmon(HFR_p$ym))

precip <- read.csv("Table_5_LM_precip.csv")
LM_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(LM_p = median(NO3, na.rm = TRUE))
LM_p$Date <- as.Date(as.yearmon(LM_p$ym))

precip <- read.csv("Table_6_MSC_precip.csv")
MSC_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(MSC_p = median(NO3, na.rm = TRUE))
MSC_p$Date <- as.Date(as.yearmon(MSC_p$ym))

precip <- read.csv("Table_7_RIDGE_precip.csv")
RIDGE_p <- precip %>%
	mutate(Date = as.Date(Date)) %>%
	mutate(ym = format(Date, "%Y-%m")) %>%
	group_by(ym) %>%
	summarize(RIDGE_p = median(NO3, na.rm = TRUE))
RIDGE_p$Date <- as.Date(as.yearmon(RIDGE_p$ym))

#merge precip datasets
precip1 <- left_join(Camp_p, FR_B_p, by = c("ym"))
precip2 <- left_join(precip1, HFR_p, by = c("ym"))
precip3 <- left_join(precip2, LM_p, by = c("ym"))
precip4 <- left_join(precip3, MSC_p, by = c("ym"))
precip <- left_join(precip4, RIDGE_p, by = c("ym"))

precip <- subset(precip, select = -c(Date.y, Date.x.x, Date.y.y, Date.x.x.x,
	Date.y.y.y))


#merge precip and stream datasets
NO3 <- left_join(precip, stream, by = c("ym"))
NO3 <- subset(NO3, select = -c(Date.x.y))


#create subset to test regressions of stream chemistry against precip data 

CampComp <- subset(NO3, select = c(ym, Camp_p, FR_s, LM_s, Clemons_s, Coles_s))
FR_Bcomp <- subset(NO3, select = c(ym, FR_B_p, FR_s, LM_s, Clemons_s, Coles_s))
HFRcomp <- subset(NO3, select = c(ym, HFR_p, FR_s, LM_s, Clemons_s, Coles_s))
LMcomp <- subset(NO3, select = c(ym, LM_p, FR_s, LM_s, Clemons_s, Coles_s))
MSCcomp <- subset(NO3, select = c(ym, MSC_p, FR_s, LM_s, Clemons_s, Coles_s))
RIDGEcomp <- subset(NO3, select = c(ym, RIDGE_p, FR_s, LM_s, Clemons_s, Coles_s))

#log(x +1) transform precip data

CampComp <- CampComp %>% mutate(logp = log1p(Camp_p))
FR_Bcomp <- FR_Bcomp %>% mutate(logp = log1p(FR_B_p))
HFRcomp <- HFRcomp %>% mutate(logp = log1p(HFR_p))
LMcomp <- LMcomp %>% mutate(logp = log1p(LM_p))
MSCcomp <- MSCcomp %>% mutate(logp = log1p(MSC_p))
RIDGEcomp <- RIDGEcomp %>% mutate(logp = log1p(RIDGE_p))

#plot all stream values against precip values

plot(CampComp[,2:7])
plot(FR_Bcomp[,2:7])
plot(HFRcomp[,2:7])
plot(LMcomp[,2:7])
plot(MSCcomp[,2:7])
plot(RIDGEcomp[,2:7])

#use bestNormalize function
#select best transformation 

norm_p <- orderNorm(CampComp$Camp_p)
norm_p <- as.data.frame(norm_p[1])
plot(x = norm_p$x.t, y = CampComp$LM_s)


norm_p <- orderNorm(FR_Bcomp$FR_B_p)
norm_p <- as.data.frame(norm_p[1])
plot(x = norm_p$x.t, y = CampComp$LM_s)


