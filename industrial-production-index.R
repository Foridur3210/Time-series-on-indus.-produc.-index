getwd()
setwd("C:/Users/admin/Desktop/desktop/sem 3/seema maam")
ind_prod<-read.csv("IPG2211A2N (1).csv")
library(fpp2)

head(ind_prod)

class(ind_prod)

str(ind_prod)

timeseries1<-ts(ind_prod$IPG2211A2N,start=c(1985,1),end = c(2018,8),frequency = 12)

timeseries1

str(timeseries1)

View(timeseries1)
View(ind_prod)

plot(timeseries1)

#spliting into train & test data
prod_train<-window(timeseries1,end=c(2011,12))
prod_test<-window(timeseries1,start=c(2012,1))

#plot train & test
plot(prod_train)
plot(prod_test)

#seasonal difference
prod_train_diff1<-diff(prod_train,lag=12)

plot(prod_train_diff1)

#kppss test
train_prod<-ur.kpss(prod_train_diff1)
summary(train_prod)

#Plot ACF &PACF
par(mfrow=c(1,2))
pacf(prod_train_diff1,main="PACF",xlab="lag",ylab=" PARTIAL ACF")
Acf(prod_train_diff1,main="ACF",xlab="lag")

#Looking at the plot we can see that as ACF is sinosidual so q=0; p=1; d=0; P=2;Q=1;D=2
fit1<-Arima(prod_train, order = c(1,0,0),seasonal=list(order=c(2,1,1),period=12))
summary(fit1)

ARIMAFIT1=auto.arima(prod_train,approximation = FALSE,stepwise = FALSE,trace = TRUE)
summary(ARIMAFIT1)

checkresiduals(ARIMAFIT1)


ARIMA_FORECAST<-forecast(ARIMAFIT1,h=80,level = 95)
summary(ARIMA_FORECAST)

par(mfrow=c(1,1))
plot(ARIMA_FORECAST)
lines(prod_test,col="red")

#checkin accuracy
accuracy(ARIMA_FORECAST,prod_test)
