#[1] "WS"         "MO"         "DAY"        "YR"         "DATE"      
#[6] "Cond"       "JTU"        "SO4"        "Mg"         "Ca"        
#[11] "K"          "Na"         "Alkalinity" "pH"         "NO3"       
#[16] "PO4"        "Cl"         "TOC"        "NH4"        "Notes"  

#1971 - 1976 species include Cond, JTU, SO4, Mg, Ca, K, Na, Alkalinity, pH, NO3
#1977 add PO4
#1995 add TOC
#1996 rm JTU, add Cl
#2002 rm PO4, add NH4

#updated to use CSV of only 4 streams

fourstreams <- read.csv("StreamwaterChemistry_fourstreams.csv")


boxP <- function(year){

setPDF(list(width = 2, height = 3), basename = paste("Stream",year[1],year[5],sep="_"))

byyear <- subset(fourstreams, subset = fourstreams$YR %in% year)

boxPlot(byyear$Cond, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Conductivity", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

#boxPlot(byyear$JTU, group = byyear$WS, Box = list(type =
#	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
#	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
#	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
#	xtitle = "Watershed", ytitle = "Turbidity", 
#	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$SO4, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Sulfate", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$Mg, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Magnesium", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$Ca, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Calcium", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$K, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Potassium", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$Na, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Sodium", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$Alkalinity, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Alkalinity", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$pH, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "pH", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$NO3, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Nitrate", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$NH4, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Ammonium", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$TOC, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Total Organic Carbon", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

#boxPlot(byyear$PO4, group = byyear$WS, Box = list(type =
#	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
#	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
#	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
#	xtitle = "Watershed", ytitle = "Phosphate", 
#	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

boxPlot(byyear$Cl, group = byyear$WS, Box = list(type =
	"tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
	truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA, NA),
	ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
	xtitle = "Watershed", ytitle = "Chloride", 
	caption = paste("Stream",year[1],year[5],sep="_"), margin = c(NA, NA, NA, NA))

dev.off()
}



