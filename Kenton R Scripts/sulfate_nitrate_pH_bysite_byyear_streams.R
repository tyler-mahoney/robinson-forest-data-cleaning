 [1] "WS"         "MO"         "DAY"        "YR"         "DATE"      
 [6] "Cond"       "JTU"        "SO4"        "Mg"         "Ca"        
[11] "K"          "Na"         "Alkalinity" "pH"         "NO3"       
[16] "PO4"        "Cl"         "TOC"        "NH4"        "Notes"    

stream <- read.csv("StreamwaterChemistry_fourstreams.csv")
season <- vector()
season <- "fall"
cbind(stream, season) 

stream$season <- ifelse(stream$MO %in% c(1:3), stream$season <- "winter",
   ifelse(stream$MO %in% c(4:6), stream$season <- "spring",
      ifelse(stream$MO %in% c(7:9), stream$season <- "summer",
         stream$season <- "fall")))


boxP <- function(watershed, year){

rm(byyear)

setPDF(list(width = 2, height = 3), basename = paste("Stream",watershed,year[1],tail(year, n=1),sep="_"))

bysite <- subset(stream, subset = stream$WS == watershed)
byyear <- subset(bysite, subset = bysite$YR %in% year)

if(!is.na(mean(byyear$NO3, na.rm = TRUE))){
boxPlot(byyear$NO3, group = byyear$season, Box = list(type = "tukey",
	show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Season", ytitle = "Nitrate", 
	caption = paste("Watershed", watershed, "NO3",year[1],tail(year, n = 1),sep = "_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

dev.off()
}

watersheds <- c(1,2,4,5)
for(i in watersheds){
boxP(watershed = i, year = 2011:2018)
}

