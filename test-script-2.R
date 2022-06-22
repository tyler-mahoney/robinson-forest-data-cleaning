## Developing data types in R 
a <- c(1,2,3,4,5)
b <- c('d','e','f','g','h')
c <- c(T,F,T,F,T)
d <- c(1,2,3,4,5,6)
# I can combine these with a data frame 
data.df <- data.frame('numbers'=a,'letters'=b,'boolean'=c)
View(data.df)
data.df$numbers
data.df$letters

names(data.df) <- c('num','let','bool')

data.df$num2 <- a
data.df$num3 <- d[1:length(a)]

which(data.df ==3)
x=2
for (i in 1:10) {
  x=x+i
  print(x)
}

data.df$thresh <- ifelse(data.df$num>3,1,0)
data.df$num[5] <- NA
sum(na.omit(data.df$thresh))


### Bringing in data to R 
install.packages('waterData')
library(waterData)

middle.fork.discharge <- importDVs('03293000',code='00060',stat='00003',
                                   sdate='2020-01-01',edate='2022-01-01')
View(middle.fork.discharge)
names(middle.fork.discharge) <- c('ID_discharge','Q_cfs','date_discharge','status_discharge')

middle.fork.cond <- importDVs('03293000',code='00095',stat='00003',
                                   sdate='2020-01-01',edate='2021-01-01')
names(middle.fork.cond) <- c('ID_cond','cond_us/cm','date_cond','status_cond')

middle.fork <- cbind(middle.fork.discharge[1:length(middle.fork.cond$staid),],middle.fork.cond)
View(middle.fork)

plot(x=middle.fork$date_discharge,y=middle.fork$Q_cfs,type='l',xlab='date',ylab='Q (cfs)',ylim=c(0,1000))
lines(x=middle.fork$dates,y=middle.fork$val)

library(ggplot2)

ggplot(data=middle.fork, aes(x=dates,y=Q_cfs)) + geom_line()
library(hydroTSM)


plot.data <- function(stationID) {
  middle.fork.test <- importDVs(stationID,code='00060',stat='00003',
                                     sdate='2020-01-01',edate='2022-01-01')
  
  flow <- middle.fork.test$val
  
  x11()
  plot(middle.fork.test$val,type='l')
  
  return(flow)
}

flow.data <- plot.data(stationID='03293000')
