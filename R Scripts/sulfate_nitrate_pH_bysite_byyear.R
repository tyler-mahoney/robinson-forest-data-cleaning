precip <- read.csv("PrecipitationChemistry_master.csv")
site1 <- subset(precip, subset = precip$SITE == 1)

season <- vector()
season <- "fall"
cbind(precip, season) 

precip$season <- ifelse(precip$MO %in% c(1:3), precip$season <- "winter",
   ifelse(precip$MO %in% c(4:6), precip$season <- "spring",
      ifelse(precip$MO %in% c(7:9), precip$season <- "summer",
         precip$season <- "fall")))


boxP <- function(site, year){

setPDF(list(width = 2, height = 3), basename = paste("Precip",site,year[1],tail(year, n=1),sep="_"))

bysite <- subset(precip, subset = precip$SITE == site)
byyear <- subset(bysite, subset = bysite$YR %in% year)

if(!is.na(mean(byyear$NO3, na.rm = TRUE))){
boxPlot(byyear$NO3, group = byyear$season, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Season", ytitle = "Nitrate", 
	caption = paste("Site", site, "NO3",year[1],tail(year, n = 1),sep = "_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$SO4, na.rm = TRUE))){
boxPlot(byyear$SO4, group = byyear$season, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Season", ytitle = "Sulfate", 
	caption = paste("Site", site, "SO4",year[1],tail(year, n = 1), sep = "_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$pH, na.rm = TRUE))){
boxPlot(byyear$pH, group = byyear$season, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Season", ytitle = "pH", 
	caption = paste("Site", site, "pH",year[1],tail(year, n = 1), sep = "_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

dev.off()
}


sites <- c(1:13)
for(i in sites){
boxP(site = i, year = 2011:2018)
}



	