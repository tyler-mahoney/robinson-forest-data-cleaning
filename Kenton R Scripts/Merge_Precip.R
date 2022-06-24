#merge precip + precip chemistry by site and export one CSV per site 

#read in precip quantity data

precip <- read.csv("PrecipQuantity.csv")

#add variable compiling Date from yr, mon, day

precip$Date <- ymd(paste(precip$yr, precip$mon, precip$day, sep = "-"))

#add rows for missing days

precip_c <- complete(precip, Date = seq.Date(as.Date("1971-01-01"), 
	as.Date("2018-12-31"), by = "day"))

# Precip = Chemistry
#"LM" = 4P(Little Millseat Bottom) 
#"FR" = 8P (Booker Bottom)
#"CAMP" = site 13 (Camp)
#"RIDGE"  = 1P (Tower Ridge)
#"HFR" = 12P (Falling Rock) 
#"MSC"  = 5P (Midslope Clearcut)

#extract only date column and column of selected site 
MSC_quantity <- precip_c[,c("Date","MSC")]

#read in chemistry data

precip_chem <- read.csv("PrecipitationChemistry_master.csv")

#add variable compiling Date 

precip_chem$Date <- ymd(paste(precip_chem$YR, precip_chem$MO, 
	precip_chem$DAY, sep = "-"))

#add rows for missing days

precip_chem_c <- complete(precip_chem, Date = seq.Date(as.Date("1971-01-01"), 
	as.Date("2018-12-31"), by = "day"), SITE)

#extract chem data for selected site

MSC_chem <- subset(precip_chem_c, subset = precip_chem_c$SITE == 5)

#merge chem data with precip data
precip_MSC_all <- left_join(MSC_quantity, MSC_chem, by = c("Date"))

write.csv(precip_MSC_all, "MSC_precip")


