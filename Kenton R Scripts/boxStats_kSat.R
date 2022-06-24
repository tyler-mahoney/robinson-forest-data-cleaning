
# get Ksat data 
dir <- "C:/WORK/RobinsonForest/UKosmre/" #"C:/PROJECTS/UKmining/2017_TNW_UK_OSM_KSAT/Data/fieldData/"
DATA <- read.table(paste(dir,"KsatData_2019jun14.txt",sep=""),sep="\t",header=T)   #KsatData_2018jan24
#data <- na.omit(DATA)
data <- subset(DATA, Ksat_cmHR>0)

sites <- c("Little Millseat","Guy Cove", "Williams Branch","Starfire","vanBooven")
treatments <- c("Control", "Forest Reclamation", "Traditional", "Ripped FRA")
positions <- c("Summit", "SideSlope", "Toe", "Reconstructed")
depth <- c("0-16 cm","20-40 cm")

#data <- subset(data, Treatment==treatments)

nSites <- length(sites)#

today <- Sys.Date()
end <- today
txt <- as.character(end)
yr <- substr(txt,1,4)   #2017      2017
mo <- substr(txt,6,7)    #10      12
dy <- substr(txt,9,10)    #18     22

surface <- subset(data,Depth==depth[1])
sub <- subset(data,Depth==depth[2])

#surface$Position <- ifelse(surface[,6]=="Reconstructed","Summit",surface[,6])

surf_Sum <- subset(surface,Position==positions[1])
surf_Side <- subset(surface,Position==positions[2])
surf_Toe <- subset(surface,Position==positions[3])
surf_TOP <- subset(surface,Position==positions[4])

sub_Sum <- subset(sub,Position==positions[1])
sub_Side <- subset(sub,Position==positions[2])
sub_Toe <- subset(sub,Position==positions[3])
sub_TOP <- subset(sub,Position==positions[4])

high <- max(data$Ksat_cmHR)

surf_SumR <- rbind(surf_Sum,surf_TOP)
sub_SumR <- rbind(sub_Sum,sub_TOP)

library(smwrGraphs)
 #by depth and position graphs
setPDF(list(width=2,height=3), basename=paste("surface_",positions[1],sep=""))
boxPlot(surf_SumR$Ksat_cmHR,
        group = surf_SumR$Treatment, Box = list(type = "simple", show.counts = TRUE, 
        nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
        #xlabels = treatments[1:3], 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("surface_",positions[1],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("surface_",positions[2],sep=""))
boxPlot(surf_Side$Ksat_cmHR,
        group = surf_Side$Treatment, Box = list(type = "simple", show.counts = TRUE, 
                                               nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
       # xlabels = treatments, 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("surface_",positions[2],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("surface_",positions[3],sep=""))
boxPlot(surf_Toe$Ksat_cmHR,
        group = surf_Toe$Treatment, Box = list(type = "simple", show.counts = TRUE, 
                                               nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
       # xlabels = treatments, 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("surface_",positions[3],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("sub_",positions[1],sep=""))
boxPlot(sub_SumR$Ksat_cmHR,
        group = sub_SumR$Treatment, Box = list(type = "simple", show.counts = TRUE, 
                                               nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
       # xlabels = treatments, 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("sub_",positions[1],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("sub_",positions[2],sep=""))
boxPlot(sub_Side$Ksat_cmHR,
        group = sub_Side$Treatment, Box = list(type = "simple", show.counts = TRUE, 
                                               nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
      #  xlabels = treatments, 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("sub_",positions[2],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("sub_",positions[3],sep=""))
boxPlot(sub_Toe$Ksat_cmHR,
        group = sub_Toe$Treatment, Box = list(type = "simple", show.counts = TRUE, 
                                               nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
        #xlabels = treatments, 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("sub_",positions[3],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

#_________________________________________ Just Reconstructed Tops

setPDF(list(width=2,height=3), basename=paste("surface_",positions[4],sep=""))
boxPlot(surf_TOP$Ksat_cmHR,
        group = surf_TOP$Basin, Box = list(type = "simple", show.counts = TRUE, 
                 nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
        #xlabels = treatments[1:3], 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("surface_",positions[4],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

setPDF(list(width=2,height=3), basename=paste("sub_",positions[4],sep=""))
boxPlot(sub_TOP$Ksat_cmHR,
        group = sub_TOP$Basin, Box = list(type = "simple", show.counts = TRUE, 
                                                nobox = 30, width = "Auto", fill="none", truncated = c(0, 100)), yaxis.log = F, 
        yaxis.range = c(0, high), ylabels = "Auto", 
        #xlabels = treatments[1:3], 
        xlabels.rotate = T, 
        xtitle = "Treatment", ytitle = "Field-Measured Ksat (cm hr-1)", 
        caption = paste("sub_",positions[4],sep=""), margin = c(NA, NA, NA, NA))
dev.off()

