par(ask=FALSE)
sales <- c(18, 33, 41,  7, 34, 35, 24, 25, 24, 21, 25, 20, 
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)
class(sales)
tsales <- ts(sales, start=c(2003, 1), frequency=12) 
tsales
plot(tsales)

start(tsales) 
end(tsales)
frequency(tsales)

library(forecast)
#opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw time series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 35), main="Simple Moving Averages (k=15)", ylim=ylim)
Nile
class(Nile)
par(opar)
class(AirPassengers)
AirPassengers
plot(AirPassengers)                                               
lAirPassengers <- log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers)")
fit <- stl(lAirPassengers, s.window="period")           
plot(fit)
fit$time.series                                 
exp(fit$time.series)

library(forecast) 
fit <- ets(nhtemp,model="ANN")      
fit
forecast(fit , 4)

fit<-ets(JohnsonJohnson)
fit
plot(forecast(fit), main="Johnson and Johnson Forecasts", 
     ylab="Quarterly Earnings (Dollars)", xlab="Time", flty=2)


library(astsa)
plot(gtemp, type="o", ylab="Global Temperature Deviations")
start(gtemp)
end(gtemp)
frequency(gtemp)
gtemp.subset <- window(gtemp,start=c(1910,1),end=c(1960,1))
gtemp.subset
plot(gtemp.subset)
jj
plot(jj, type="o", ylab="Quarterly Earnings per Share")
fit<-stl(jj,s.window = "period")
plot(fit)

fit$time.series
par(mfrow=c(2,1))
monthplot(jj)
seasonplot(jj,year.labels = "true",main = "")

fit<-ets(jj)
fit
forecast(fit,1)
gtemp
gtemp[1:129]
fit<-ets(gtemp)
fit
forecast(gtemp,1)
