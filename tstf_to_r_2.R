Paths = c("C:/Users/pf/Documents/2013")
names(Paths) = c("pf@rff.dk")
setwd(Paths[Sys.info()[7]])

#install.packages(c("usethis"),dep=TRUE)
#install.packages('tseries',dep=TRUE)
#install.packages('rio',dep=TRUE)


rm(list=ls())

library(usethis)
library(stats)
library(foreign)
library(TSA) #Cryer & Chen 
library(car)
library(aod)
library(forecast)
library(ggplot2)
library(tseries)
library(haven)
library(rio)
mydata <- read.dta("data_tstf_old.dta")

#outliers


marriage<-import("marriages.csv")
marriage$year<-marriage$V1



#intervention

mydata$pulse <- as.numeric(mydata$time == 166)

mydata$step <- as.numeric(mydata$time >= 166)

mydata$year<-floor((mydata$time-1)/12)+2000

total <- merge(mydata,marriage,by="year",all=TRUE)

##Limit sample to January, 2010 -- December, 2017

mydata<-total[110:229,]

##Clean up
rm(list=c("marriage","total"))

write.csv2(mydata,file="sample.csv")

#divorce rate measured as monthly divorce per 100,000 married couples mid year
mydata$y<-(mydata$log_incident/mydata$V2)*100000

#Log and time series set the data
y<-ts(mydata$y, f=12)
logy <- log(mydata$y)
logy <- ts(logy, frequency = 12, start = c(2009, 1))
y <- ts(y, frequency = 12, start = c(2009, 1))
mydata$logy <- ts(log((mydata$log_incident/mydata$V2)*100000),  f=12, start = c(2009, 1))

newdata<-ts(mydata,start=1,f=12)

traindata<-newdata[1:54,9:9]
traindata<-ts(traindata,f=12,start = c(2009, 1))

ggAcf(traindata)




#plot the time series

png(filename="timeseries.png", width = 10, height = 7, units = "in", pointsize = 14,
    bg = "white",  res = 250,  type = c("windows"))
  plot(y,xlab = "Year", ylab = "Monthly divorces per 100,000 marriages",lwd=2)
  abline(v=2013.5,lwd=2,lty="dashed")
  abline(v=2013.75,lwd=2,lty="dotted")
dev.off()
pacf(logy)
ggAcf(logy)
plot(y=logy,x=zlag(logy),type='p')



res <- residuals(naive(logy))
autoplot(res) + xlab("Month") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of naive residuals")

t1 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             io=c(55,73,108),
             xtransf = mydata[,c("pulse")], transfer = list(c(1,0)))

summary(t1)

tsdiag(t1, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
shapiro.test(t1$residuals)
runs(t1$residuals)
checkresiduals(t1)


t5 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             io=c(55,73,108),
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t5)

png(filename="diagnostic.png", width = 7, height = 7, units = "in", pointsize = 18,
    bg = "white",  res = 500,  type = c("windows"))
tsdiag(t5, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
dev.off()
ggAcf(t5$residuals)+ggtitle("ACF, seasonal AR(1), IOs, pulse AR(1) and step-function")
gghistogram(t5$residuals) + ggtitle("Histogram of residuals")
##test for patterns in residuals
shapiro.test(t5$residuals)
runs(t5$residuals)
checkresiduals(t5)
detectIO(t5)
detectAO(t5)


dev.off()
plot(logy,lwd=2)
points(fitted(t5),pch=19,cex=1)




#play

##Forecasting off cleaned data-set pre-pulse

train<- Arima(traindata,order = c(0,0,0),seasonal = c(1,0,0))
summary(train)
checkresiduals(train)
shapiro.test(train$residuals)
runs(train$residuals)

dev.off()
fcast<-forecast(Arima(traindata,order = c(0,0,0),seasonal = c(1,0,0)),h=66)
summary(fcast)
plot(fcast,xlab = "Year", ylab = "Monthly divorce per 100,000 marriages", ylim=c(3.7,5.7),lwd=2)
points(fitted(t5),pch=19,cex=1.5)



##Assuming no step

txc <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
              xreg = mydata[,c("out1", "out2")],
              xtransf = mydata[,c("pulse")], 
              transfer = list(c(1,0)))
summary(txc)
shapiro.test(txc$residuals)
runs(txc$residuals)
checkresiduals(txc,gof=24)
detectIO(txc)
detectAO(txc)

plot(logy,lwd=3)
points(fitted(txc),pch=19,cex=1.5)
points(fitted(t5),pch=19,cex=1.5,col="blue")


##Assuming AR(1) step

tx2 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
              xreg = mydata[,c("out1", "out2")],
              xtransf = mydata[,c("pulse")], 
              transfer = list(c(1,0)))
summary(tx2)
shapiro.test(txc$residuals)
runs(txc$residuals)
checkresiduals(txc,gof=24)
detectIO(txc)
detectAO(txc)

plot(logy,lwd=3)
points(fitted(txc),pch=19,cex=1.5)
points(fitted(t5),pch=19,cex=1.5,col="blue")

