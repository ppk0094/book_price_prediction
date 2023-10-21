##Libraries used

#install.packages("mongolite")
#install.packages("fUnitRoots")
library(data.table)
library("fUnitRoots")
library(mongolite)
library(plyr)
library(ggplot2)
library(dplyr)
library(forcats)
library("forecast")
library(FitAR)
#library(leaflet)
#library(rworldmap)


#Pedestrian traffic system, getting the collections from fit5141 database
pedestrian_traffic = mongo(collection = "peddata", db = "fit5141")
pedestrian_sensors = mongo(collection = "pedsensors", db = "fit5141")
buildings = mongo(collection = "buildings", db = "fit5141")

#Getting the values of the collections
ped_traffic = pedestrian_traffic$find()
ped_sensors = pedestrian_sensors$find()
buildings = buildings$find()

##Converting date time to only date column
new_data  <- ped_traffic
new_data$actual_date <- as.Date(mdy_hms(new_data$Date_Time))

##Aggregating the date to get day wise trend
total_traffic = aggregate(new_data[,10],by = list(new_data$Month,new_data$Year),FUN = mean)
names(total_traffic) <- c("Month","Year","count")


traffic.ts <- ts(total_traffic$count,start=c(2009,5),frequency=12)
autoplot(traffic.ts,main = "Pedestrian traffic in city across years")


## Exploratory data analysis
components.ts = decompose(traffic.ts)
plot(components.ts)

##Unit root test
urkpssTest(traffic.ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(traffic.ts, differences=1)
plot(tsstationary)

##Auto correlation function
acf(traffic.ts,lag.max=34)

#Seasonal component is removed from the time series data
timeseriesseasonallyadjusted <- traffic.ts- components.ts$seasonal
plot(timeseriesseasonallyadjusted)
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)

#ACF and PACF function to get p, q values
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)


## fitting Arima model to the time series converted data
fitARIMA<-arima(traffic.ts, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
fitARIMA

coeftest(fitARIMA)
par(mfrow=c(1,1))
acf(fitARIMA$residuals)

boxresult<-LjungBoxTest (fitARIMA$residuals,k=2,StartLag=1) # residual?? or the original series?
par(mfrow=c(2,1))
plot(boxresult[,3],main="Ljung-Box Q Test", ylab="P-values", xlab="Lag")
qqnorm(fitARIMA$residuals)
qqline(fitARIMA$residuals)

par(mfrow=c(1,1))
predict(fitARIMA,n.ahead = 10)
futurVal <- forecast(fitARIMA,h=24, level=c(99.5))
plot(futurVal)


futur_df <- as.data.frame(futurVal)


formatted_df <- setDT(futur_df, keep.rownames=TRUE)[]

names(formatted_df) <- c("Time","count","low","high")
colnames(formatted_df)

forecast_df <- formatted_df %>% select(Time,count)
colnames(forecast_df)
write.csv(forecast_df,"forecast.csv")
