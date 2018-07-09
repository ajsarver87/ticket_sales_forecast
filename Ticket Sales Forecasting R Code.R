#Run this entire block of code to set up time series analysis
library(TSA)
library(uroot)
library(tseries)
data <- read.table("salesdata.txt")
temp <-ts(data)
temp<-rev(temp)
BadTicketSales <- ts(temp, start=c(1925,1), end=c(2014,1), frequency=1)
TicketSales=BadTicketSales
TicketSales30=window(BadTicketSales, start=1985, end=2014, frequency=1)
TicketSales30No94=TicketSales30[-10]
TicketSales30No9495=TicketSales30[-(10:11)]

#set which time series you want to model
x=TicketSales30No9495

#Data Transformation
LogData=log(x)
DiffData=diff(x)
DiffLogData=diff(log(x))

#Turn Data into Time Series Objects
DiffData.ts=ts(DiffData)
DiffLogData.ts=ts(DiffLogData)

#Normality Tests for Data Set
hist(x)
qqnorm(x); qqline(x)
shapiro.test(x)

#Normality Tests for Diff
hist(DiffData)
qqnorm(DiffData);qqline(DiffData)
shapiro.test(DiffData)

#Normality Tests for DiffLog
hist(DiffLogData)
qqnorm(DiffLogData);qqline(DiffLogData)
shapiro.test(DiffLogData)

#Looking for stationarity of Diff
plot(DiffData.ts, type='l')
adf.test(DiffData.ts)

#Looking for stationarity of DiffLog
plot(DiffLogData.ts, type='l')
adf.test(DiffLogData.ts)

#Model Specification for DiffData.ts
#Looking for MA process
acf(DiffData.ts)
#Looking for AR process
pacf(DiffData.ts)
#Looking for ARMA Process
eacf(DiffData.ts, ar.max=6, ma.max=6)
#Looking for Subsetted ARMA Series
res=armasubsets((DiffData.ts), nar=6, nma=6,
	y.name='test', ar.method='ols')
plot(res)

#Model Specification for DiffLogData.ts
#Looking for MA process
acf(DiffLogData.ts)
#Looking for AR process
pacf(DiffLogData.ts)
#Looking for ARMA Process
eacf(DiffLogData.ts, ar.max=6, ma.max=6)
#Looking for Subsetted ARMA Series
res=armasubsets((DiffLogData.ts), nar=6, nma=6,
	y.name='test', ar.method='ols')
plot(res)

#ModelsforDiffData
#SeasonalIMA(1,1) with period 4
arima(DiffData.ts, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=4), method='ML')
#Results - No Insignificants, Test Residuals

#ARIMA(5,1,1)
arima(DiffData.ts, order=c(5,0,1), method='ML')
#Results - Insignificance, Throw Out

#SubsetARIMA(6,1,5) with only AR1,3,4,6 MA1,5
arima(DiffLogData.ts, order=c(6,0,5), fixed=c(NA,0,NA,NA,0,NA,NA,0,0,0,NA,NA), method='ML')
#Results - Insignificance, Throw Out

#Models for DiffLogData
#SeasonalIMA(1,1) with period 4
arima(DiffLogData.ts, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=4), method='ML')
#Results - OK, Test Residauls

#ARI(6,1), Reduce overfitting if insig coeff
arima(DiffLogData.ts, order=c(6,0,0), method='ML')
arima(DiffLogData.ts, order=c(5,0,0), method='ML')
arima(DiffLogData.ts, order=c(4,0,0), method='ML')
arima(DiffLogData.ts, order=c(3,0,0), method='ML')
#Results - ARI(3,1) is OK, Test Residuals

#Set Up Models
a=arima(DiffData.ts, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=4), method='ML')
b=arima(DiffLogData.ts, order=c(0,0,0), seasonal=list(order=c(0,0,1), period=4), method='ML')
c=arima(DiffLogData.ts, order=c(3,0,0), method='ML')

#Residual Analysis for DiffData.ts SeasonalIMA(1,1)
tsdiag(a)
plot(rstandard(a), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(a)); qqline(residuals(a))
acf(residuals(a))
hist(residuals(a))
shapiro.test(residuals(a))
#Residuals Normal - Can Use to Forcast (pvalue=.5977)

#Residual Analysis for DiffLogData.ts SeasonalIMA(1,1)
tsdiag(b)
plot(rstandard(b), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(b)); qqline(residuals(b))
acf(residuals(b))
hist(residuals(b))
shapiro.test(residuals(b))
#Residual Normal - Can Use to Forcast (pvalue=.3748)

#Residual Analysis for ARI(3,1)
tsdiag(c)
plot(rstandard(c), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(c)); qqline(residuals(c))
acf(residuals(c))
hist(residuals(c))
shapiro.test(residuals(c))
#Residual Normal - Can Use to Forcast (pvalue=.7331)

#Will Use ARI(3,1) to forcast since Shapiro-Wilk Normality Test Yeilded the highest P-Value and it has the 2nd Lowest AIC, but isn't far off of the lowest

#Compare ARI(3,1) With Original
plot(DiffLogData.ts, col="red"); lines(fitted(c), col='blue')

#Code Block for Forcasting Plot and 10 Years into the Future
plot(c)
predict <- predict(c, n.ahead=10)
predict

#Backtransform the forecasted time series to ticket sales
previous <- x[28]

for (i in 1:10){
  trans[i] <- previous*exp(predict$pred[i])
  previous <- trans[i]
}

trans
plot(trans, type="l")

#You are going to Have to use a recursive formula to backtransform the Predictions  which is:
#
#    X*e^(P)=T
#
#     X = the last year of data/last year's prediction
#     P = Prediction in transformed Scale
#     T = prediction in normal ticket sales numbers (what we want)
#
#NOTE: This is done by the code above.



#Plots for Paper
plot(TicketSales)
plot(TicketSales30)
plot(TicketSales30No9495, type='l')
plot(log(TicketSales30No9495), type='l')
y=ts(TicketSales30No9495)
adf.test(y)