library(TSA)
library(uroot)

data <- read.table("/Users/Alex/Desktop/salesdata.txt")
temp <-ts(data)
temp<-rev(temp)
BadTicketSales <- ts(temp, start=c(1925,1), end=c(2014,1), frequency=1)
BadTicketSales

TicketSales=BadTicketSales
TicketSales30=window(BadTicketSales, start=1985, end=2014, frequency=1)
TicketSales
TicketSales30

LogTicketSales30= log(TicketSales30)
DiffTicketSales30 = diff(TicketSales30)
DiffLogTicketSales30 = diff(log(TicketSales30))
LogTicketSales= log(TicketSales)
DiffTicketSales = diff(TicketSales)
DiffLogTicketSales = diff(log(TicketSales))

ADF.test(TicketSales, selectlags=list(mode=0), itsd=c(1,0,0))
ADF.test(DiffTicketSales, selectlags=list(mode=0), itsd=c(1,0,0))
ADF.test(DiffLogTicketSales, selectlags=list(mode=0), itsd=c(1,0,0))
ADF.test(TicketSales30, selectlags=list(mode=0), itsd=c(1,0,0))
ADF.test(DiffTicketSales30, selectlags=list(mode=0), itsd=c(1,0,0))
ADF.test(DiffLogTicketSales30, selectlags=list(mode=0), itsd=c(1,0,0))

plot(TicketSales)
plot(LogTicketSales)
plot(DiffTicketSales)
plot(DiffLogTicketSales)
plot(TicketSales30)
plot(LogTicketSales30)
plot(DiffTicketSales30)
plot(DiffLogTicketSales30)

acf(DiffTicketSales)
acf(DiffLogTicketSales)
acf(DiffTicketSales30)
acf(DiffLogTicketSales30)

pacf(DiffTicketSales)
pacf(DiffLogTicketSales)
pacf(DiffTicketSales30)
pacf(DiffLogTicketSales30)

eacf(DiffTicketSales)
eacf(DiffLogTicketSales)

eacf(DiffTicketSales30, ar.max=1, ma.max=12)
eacf(DiffTicketSales30, ar.max=2, ma.max=11)
eacf(DiffTicketSales30, ar.max=3, ma.max=10)
eacf(DiffTicketSales30, ar.max=4, ma.max=9)
eacf(DiffTicketSales30, ar.max=5, ma.max=8)
eacf(DiffTicketSales30, ar.max=6, ma.max=7)
eacf(DiffTicketSales30, ar.max=7, ma.max=6)
eacf(DiffTicketSales30, ar.max=8, ma.max=5)
eacf(DiffTicketSales30, ar.max=9, ma.max=4)
eacf(DiffTicketSales30, ar.max=10, ma.max=3)
eacf(DiffTicketSales30, ar.max=11, ma.max=2)
eacf(DiffTicketSales30, ar.max=12, ma.max=1)

eacf(DiffLogTicketSales30, ar.max=1, ma.max=12)
eacf(DiffLogTicketSales30, ar.max=2, ma.max=11)
eacf(DiffLogTicketSales30, ar.max=3, ma.max=10)
eacf(DiffLogTicketSales30, ar.max=4, ma.max=9)
eacf(DiffLogTicketSales30, ar.max=5, ma.max=8)
eacf(DiffLogTicketSales30, ar.max=6, ma.max=7)
eacf(DiffLogTicketSales30, ar.max=7, ma.max=6)
eacf(DiffLogTicketSales30, ar.max=8, ma.max=5)
eacf(DiffLogTicketSales30, ar.max=9, ma.max=4)
eacf(DiffLogTicketSales30, ar.max=10, ma.max=3)
eacf(DiffLogTicketSales30, ar.max=11, ma.max=2)
eacf(DiffLogTicketSales30, ar.max=12, ma.max=1)

res=armasubsets((DiffTicketSales), nar=15, nma=15,
	y.name='test', ar.method='ols')
plot(res)

res=armasubsets((DiffLogTicketSales), nar=15, nma=15,
	y.name='test', ar.method='ols')
plot(res)

res=armasubsets((DiffTicketSales30), nar=10, nma=10,
	y.name='test', ar.method='ols')
plot(res)

res=armasubsets((DiffLogTicketSales30), nar=10, nma=10,
	y.name='test', ar.method='ols')
plot(res)

arima(DiffTicketSales, order=c(0,0,12), method='ML')
arima(DiffTicketSales, order=c(3,0,12), method='ML')
arima(DiffTicketSales, order=c(14,0,0), method='ML')

arima(DiffLogTicketSales, order=c(0,0,12), method='ML')

arima(DiffTicketSales30, order=c(7,0,8), method='ML')
arima(DiffTicketSales30, order=c(5,0,4), method='ML')
arima(DiffTicketSales30, order=c(0,0,3), method='ML')

arima(DiffLogTicketSales30, order=c(0,0,5), fixed=c(NA,0,0,0,NA,NA), method='ML')
arima(DiffLogTicketSales30, order=c(9,0,2), method='ML')
arima(DiffLogTicketSales30, order=c(0,0,2), method='ML')
arima(DiffLogTicketSales30, order=c(1,0,1), method='ML')
arima(DiffLogTicketSales30, order=c(0,0,1), method='ML')
arima(DiffLogTicketSales30, order=c(1,0,0), method='ML')

a=arima(DiffLogTicketSales30, order=c(0,0,2), method='ML')
tsdiag(a)
plot(rstandard(a), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(a)); qqline(residuals(a))
acf(residuals(a))
hist(residuals(a))
shapiro.test(residuals(a))
plot(DiffLogTicketSales30, col="red"); lines(fitted(a), col='blue')

b=arima(DiffLogTicketSales30, order=c(0,0,6), method='ML')
tsdiag(b)
plot(rstandard(b), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(b)); qqline(residuals(b))
acf(residuals(b))
hist(residuals(b))
shapiro.test(residuals(b))
plot(DiffLogTicketSales30, col="red"); lines(fitted(b), col='blue')

c=arima(DiffLogTicketSales30, order=c(9,0,2), method='ML')
tsdiag(c)
plot(rstandard(c), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(c)); qqline(residuals(c))
acf(residuals(c))
hist(residuals(c))
shapiro.test(residuals(c))
plot(DiffLogTicketSales30, col="red"); lines(fitted(c), col='blue')

d=arima(DiffLogTicketSales30, order=c(1,0,1), method='ML')
tsdiag(d)
plot(rstandard(d), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(d)); qqline(residuals(d))
acf(residuals(d))
hist(residuals(d))
shapiro.test(residuals(d))
plot(DiffLogTicketSales30, col="red"); lines(fitted(d), col='blue')

e=arima(DiffLogTicketSales, order=c(0,0,12), method='ML')
tsdiag(e)
plot(rstandard(e), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(e)); qqline(residuals(e))
acf(residuals(e))
hist(residuals(e))
shapiro.test(residuals(e))
plot(DiffLogTicketSales, col="red"); lines(fitted(e), col='blue')

f=arima(DiffLogTicketSales30, order=c(1,0,0), method='ML')
tsdiag(f)
plot(rstandard(f), ylab='Stanardized Residuals', type='o'); abline(h=0)
qqnorm(residuals(f)); qqline(residuals(f))
acf(residuals(f))
hist(residuals(f))
shapiro.test(residuals(f))
plot(DiffLogTicketSales30, col="red"); lines(fitted(f), col='blue')

plot(f)
predict(f, n.ahead=10)
