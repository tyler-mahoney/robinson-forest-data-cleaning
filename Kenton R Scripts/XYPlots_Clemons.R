## S4 method for signature 'numeric,numeric'
xyPlot(x, y, Plot = list(name = "", what =
  "points", type = "solid", width = "standard", symbol = "circle", filled =
  TRUE, size = 0.09, color = "black"), yaxis.log = FALSE, yaxis.rev = FALSE,
  yaxis.range = c(NA, NA), xaxis.log = FALSE, xaxis.range = c(NA, NA),
  ylabels = 7, xlabels = 7, xtitle = deparse(substitute(x)),
  ytitle = deparse(substitute(y)), caption = "", margin = c(NA, NA, NA,
  NA), ...)

setPDF(list(width=2,height=3), basename="Clemons_2016_2018")

xyPlot(Clemons$Cond, Clemons$JTU, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "JTU", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$Mg, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Mg", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$SO4, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Sulfate", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$Ca, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Calcium", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$K, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Potassium", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$Na, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Sodium", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$Alkalinity, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Alkalinity", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$pH, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "pH", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$NO3, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Nitrate", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$PO4, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Phosphate", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$Cl, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Chloride", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$TOC, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Total Organic Carbon", caption = "Clemons")

xyPlot(Clemons$Cond, Clemons$NH4, yaxis.range = c(NA, NA), 
	xaxis.range = c(NA, NA), xtitle = "Conductivity", 
	ytitle = "Ammonium", caption = "Clemons")

dev.off()

