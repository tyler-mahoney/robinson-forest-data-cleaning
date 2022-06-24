[1] "DATE"    "MO"      "DAY"     "YR"      "SITE"    "Cl"      "NO3"    
 [8] "SO4"     "NH4"     "pH"      "ALK"     "Ca"      "Mg"      "K"      
[15] "Na"      "Cond"    "TOC"     "PO4"     "Notes"   "Notes.1" "Notes.2"




precip <- read.csv("PrecipitationChemistry_master.csv")

byyear <- subset(precip, subset = precip$YR %in% year)



boxP <- function(year){

setPDF(list(width = 2, height = 3), basename = paste("Precip",year[1],tail(year, n=1),sep="_"))

byyear <- subset(precip, subset = precip$YR %in% year)


if(!is.na(mean(byyear$Cl, na.rm = TRUE))){
boxPlot(byyear$Cl, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Chloride", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$NO3, na.rm = TRUE))){
boxPlot(byyear$NO3, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Nitrate", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$SO4, na.rm = TRUE))){
boxPlot(byyear$SO4, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Sulfate", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$NH4, na.rm = TRUE))){
	boxPlot(byyear$NH4, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Ammonium", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$pH, na.rm = TRUE))){
boxPlot(byyear$pH, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "pH", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$ALK, na.rm = TRUE))){
boxPlot(byyear$ALK, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Alkalinity", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$Ca, na.rm = TRUE))){
boxPlot(byyear$Ca, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Calcium", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$Mg, na.rm = TRUE))){
boxPlot(byyear$Mg, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Magnesium", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$K, na.rm = TRUE))){
boxPlot(byyear$K, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Potassium", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$Na, na.rm = TRUE))){
boxPlot(byyear$Na, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Sodium", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$Cond, na.rm = TRUE))){
boxPlot(byyear$Cond, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Conductivity", 
	caption = paste("Precip",year[1],tail(year, n=1),sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$TOC, na.rm = TRUE))){
boxPlot(byyear$TOC, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Total Organic Carbon", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

if(!is.na(mean(byyear$PO4, na.rm = TRUE))){
boxPlot(byyear$PO4, group = byyear$SITE, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "SITE", ytitle = "Phosphate", 
	caption = paste("Precip",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))
	} else {
	print("NA")}

dev.off()
}
