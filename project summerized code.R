#Run this entire block of code to set up time series analysis
library(TSA)
library(uroot)
data <- read.table("salesdata.txt")
temp <-ts(data)
temp<-rev(temp)
BadTicketSales <- ts(temp, start=c(1925,1), end=c(2014,1), frequency=1)
BadTicketSales
TicketSales=BadTicketSales
TicketSales30=window(BadTicketSales, start=1985, end=2014, frequency=1)
TicketSales30No1994=TicketSales30[-9]
LogTicketSales30= log(TicketSales30)
DiffTicketSales30 = diff(TicketSales30)
DiffLogTicketSales30 = diff(log(TicketSales30))
LogTicketSales= log(TicketSales)
DiffTicketSales = diff(TicketSales)
DiffLogTicketSales = diff(log(TicketSales))
plot(TicketSales)

#Looking for stationarity
plot(DiffLogTicketSales30)
ADF.test(DiffLogTicketSales30, selectlags=list(mode=0), itsd=c(1,0,0))

plot(TicketSales30)
plot(TicketSales30No1994)
#Model Specification
#Looking for MA process
acf(DiffLogTicketSales30[-9])
#Looking for AR process
pacf(DiffLogTicketSales30[-9])
#Looking for ARMA Process
eacf(DiffLogTicketSales30[-9], ar.max=6, ma.max=7)
eacf(DiffLogTicketSales30, ar.max=7, ma.max=6)
#Looking for Subsetted ARMA Series
res=armasubsets((DiffLogTicketSales30[-9]), nar=10, nma=10,
	y.name='test', ar.method='ols')
plot(res)
#Last 30 years - minus 1994 = normal distribution mean, st
mean(DiffLogTicketSales30)
#Possible Models
#IMA(1,1)
arima(DiffLogTicketSales30, order=c(0,0,1), method='ML')
#ARI(1,1)
arima(DiffLogTicketSales30, order=c(1,0,0), method='ML')
#IMA(1,6) only w/lags1,5,6 and intercept
arima(DiffLogTicketSales30, order=c(0,0,6), fixed=c(NA,0,0,0,NA,NA,NA), method='ML')
#ARIMA (9,1,2) only w/AR lag 9 and MA lag 2 and intercept
arima(DiffLogTicketSales30, order=c(0,0,0), seasonal=list(order=c(1,0,1), period=3), method='ML')

#IMA(1,2) only w/lag 2 and intercept
arima(DiffLogTicketSales30, order=c(0,0,2), fixed=c(0,NA,NA), method='ML')
hist(DiffLogTicketSales30[-9])
shapiro.test(DiffLogTicketSales30[-9])
qqnorm(Dif)
#use DiffTicketSales30 ARIMA (1,1,0) & ARIMA(0,1,1)
#IMA(1,1) - AIC=-45.69, only intercept insignificant
#ARI(1,1) - AIC=-45.78, only intercept insignificant
#IMA(1,6) - AIC=-43.35, Lag6 and Intercept insignificant, overfit?
#ARIMA (9,1,2) - AIC=-40.16, Everything insignificant, overfit?
#IMA (1,2) - AIC=-41.91, everything insignificant, overfit?

#Code Block for Residual analysis for IMA(1,1)
a=arima(DiffLogTicketSales30[-9], order=c(0,0,1), method='ML')
tsdiag(a)
plot(rstandard(a), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(a)); qqline(residuals(a))
acf(residuals(a))
hist(residuals(a))
shapiro.test(residuals(a))

#Code Block for Ploting Observed series vs. model
plot(DiffLogTicketSales30[-9], col="red", type='b'); lines(fitted(a), col='blue')

#Code Block for Forecasting
plot(a)
predict(a, n.ahead=10)
plot(forecast(DiffLogTicketSales30))
#Code Block for Residual analysis for ARI(1,1)
b=arima(DiffLogTicketSales30[-9], order=c(1,0,0), method='ML')
tsdiag(b)
plot(rstandard(b), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(b)); qqline(residuals(b))
acf(residuals(b))
hist(residuals(b))
shapiro.test(residuals(b))

#Code Block for Ploting Observed series vs. model
plot(DiffLogTicketSales30, col="red"); lines(fitted(b), col='blue')

#Code Block for Forecasting
plot(b)
predict(b, n.ahead=10)
