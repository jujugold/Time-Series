
install.packages ("plotly")
install.packages ("xts")
install.packages ("forecast")
install.packages ("TSstudio")
install.packages ("tseries")
install.packages("ggfortify")

library(plotly)
library(xts)
library(TSstudio)
library(tseries)
library(forecast)
library(ggfortify)


sweets <- read.csv("US_sweets_production.csv")
colnames(sweets)<-c("date", "production")

sweets$date<-as.Date(sweets$date, format="%Y-%m-%d")

SweetsXTS<- xts(sweets[-1], sweets[[1]])
Sweets_TS<- ts(sweets$production, start=c(1972,1),end=c(2017,8), frequency=12 )

#Plot time series
ts_plot(SweetsXTS, title="Sweet Production Time Series")
plot(Sweets_TS, main = "Monthly US Sweet Production - 1972 to 2017",
     ylab = "Production as % of 2012 Production",
     xlab = "Date")


ts_decompose(Sweets_TS, "multiplicative")
ts_decompose(Sweets_TS, "additive")

#Here are two more methods to obtain information on seasonality in the data. 
#It is clear in the months of October, November, and December sweets are produced more
ts_heatmap(Sweets_TS, title="Sweet Production Time Series")
  
ts_seasonal(Sweets_TS, type="box")

#Using forecast library to plot autocorrelation function (ACF). 
#This shows evidence of non-stationary data. 

ggAcf(Sweets_TS, main = "                Autocorrelation Function - Sweets TS ")

#Stationarity testing using ADF
adf.test(Sweets_TS) # reject the null of non stationary data; however, the adf test doesn't pick up on seasonality, so it's a bad test here.

#Let's do an arima model via the Auto.Arima command and plot the standardized residuals, 
#the ACF of residuals, and pvalues for the Ljung-Box statistic. Standardized residuals should be centered around 0.
#ACF of residuals should be within blue line. And we want low pvalues for the LB lags.
#Ljung-Box statisitic tests autocorrelation for groups of lags in time series analysis. High pvalues state there is autocorrelation at that lag

arima_sweets <- auto.arima(Sweets_TS,stepwise = FALSE)
arima_sweets
ggtsdiag(arima_sweets)

#Now we forecast using the standard forecast command in R, with a 95% CI and three years
forecast_candyts <- forecast(arima_sweets, level = c(95), h = 24)
plot(forecast_candyts, 
     ylab = "Production as % of 2012 Production",
     xlab = "Year")

autoplot(forecast_candyts,
         main = "Arima (1,0,2)(0,1,2)[12] Forecast") +
  ylab("Production as % of 2012 Production") +
  xlab("Year")

ts_plot(forecast_candyts)
ggplot(forecast_candyts, aes(x,y)) +
  geom_point(forecast_candyts)



