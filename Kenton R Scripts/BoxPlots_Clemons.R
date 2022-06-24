boxPlot(data$Cond, group = data$WS, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(0, 500),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = FALSE,
  xtitle = "", ytitle = "", caption = "", margin = c(NA, NA, NA, NA))

#species include: Cond, JTU, SO4, Mg, Ca, K, Na, Alkalinity, pH, NO3, PO4, Cl, TOC, NH4

data1975 <- subset(data, subset = data$YR %in% c(1971, 1972, 1973, 1974, 1975))
data1980 <- subset(data, subset = data$YR %in% c(1976, 1977, 1978, 1979, 1980))
data1985 <- subset(data, subset = data$YR %in% c(1981, 1982, 1983, 1984, 1985))
data1990 <- subset(data, subset = data$YR %in% c(1986, 1987, 1988, 1989, 1990))
data1995 <- subset(data, subset = data$YR %in% c(1991, 1992, 1993, 1994, 1995))
data2000 <- subset(data, subset = data$YR %in% c(1996, 1997, 1998, 1999, 2000))
data2005 <- subset(data, subset = data$YR %in% c(2001, 2002, 2003, 2004, 2005))
data2010 <- subset(data, subset = data$YR %in% c(2006, 2007, 2008, 2009, 2010))
data2015 <- subset(data, subset = data$YR %in% c(2011, 2012, 2013, 2014, 2015))
data2018 <- subset(data, subset = data$YR %in% c(2016, 2017, 2018))

Clemons <- subset(data, WS == 4)
Coles <- subset(data, WS == 5) 
FR <- subset(data, WS == 1) 
LM <- subset(data, WS == 2) 

setPDF(list(width=2,height=3), basename="Clemons_2016_2018")

boxPlot(Clemons$Cond, group = Clemons$YR, Box = list(type =
  "tukey", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Conductivity", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$JTU, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Turbidity", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$SO4, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Sulfate", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$Mg, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Magnesium", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$Ca, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Calcium", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$K, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Potassium", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$Na, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Sodium", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$Alkalinity, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Alkalinity", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$pH, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "pH", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$NO3, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Nitrate", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$PO4, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Phosphate", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$Cl, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Chloride", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$TOC, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Total Organic Carbon", caption = "Clemons", margin = c(NA, NA, NA, NA))

boxPlot(Clemons$NH4, group = Clemons$YR, Box = list(type =
  "simple", show.counts = TRUE, nobox = 30, width = "Auto", fill = "none",
  truncated = c(0, 100)), yaxis.log = FALSE, yaxis.range = c(NA,NA),
  ylabels = "Auto", xlabels = "Auto", xlabels.rotate = T,
  xtitle = "Year", ytitle = "Ammonium", caption = "Clemons", margin = c(NA, NA, NA, NA))

dev.off()






