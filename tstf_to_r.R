Paths = c("C:/Users/pf/Documents/2013")
names(Paths) = c("pf")
setwd(Paths[Sys.info()[7]])

#install.packages(c("usethis"),dep=TRUE)
#install.packages('tseries',dep=TRUE)



rm(list=ls())

library(usethis)
library(stats)
library(foreign)
library(TSA) #Cryer & Chen 
library(car)
library(aod)
library(forecast)
library(ggplot2)
mydata <- read.dta("data_tstf_old.dta")

#outliers

mydata$out1 <- as.numeric(mydata$time == 163)
mydata$out2 <- as.numeric(mydata$time == 181)
mydata$out3 <- as.numeric(mydata$time == 164)
mydata$out4 <- as.numeric(mydata$time == 165)


#intervention

mydata$pulse <- as.numeric(mydata$time == 166)

mydata$step <- as.numeric(mydata$time >= 166)


#Log and time series set the data
y<-ts(mydata$log_incident, f=12)
logy <- log(mydata$log_incident)
logy <- ts(logy, frequency = 12)
mydata$logy <- log(mydata$log_incident)

newdata<-ts(mydata,start=1,f=12)

traindata<-newdata[1:165,]
traindata<-ts(traindata,f=12)

clean<- tsclean(logy)

#plot the time series
plot(logy)
plot(clean)
pacf(clean)



plot(y=clean,x=zlag(clean),type='p')

autoplot(logy) +
  xlab("Month") + ylab("log(Number of Divorces)") +
  ggtitle("Log of divorce frequency (Monthly ending December, 2017)")

res <- residuals(naive(logy))
autoplot(res) + xlab("Month") + ylab("") +
  ggtitle("Residuals from na?ve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of naive residuals")

f1 <- arima(logy, order = c(1,0,0))
ggAcf(f1$residuals) + ggtitle("ACF of AR(1)")

f2 <- arima(logy, order = c(0,1,0))
ggAcf(f2$residuals) + ggtitle("ACF of Random Walk")

f3 <- arima(logy, order = c(1,1,0))
ggAcf(f3$residuals) + ggtitle("ACF of diff. AR(1)")

f4 <- arima(logy, order = c(1,1,1))
ggAcf(f4$residuals) + ggtitle("ACF of diff. AR(1),MA(1)")


f5 <- arima(logy, order = c(1,0,2), seasonal = c(1,0,0))
ggAcf(f5$residuals) + ggtitle("ACF of AR(1), MA(2) with seasonal AR(1)")

auto.arima(logy)
arimax(logy, order = c(1,1,1), seasonal = c(0,0,2))

t1 <- arimax(logy, order = c(1,1,1), seasonal = c(0,0,2),
             xreg = mydata[,c("out1", "out2","out4")])
summary(t1)
acf(t1$residuals)
shapiro.test(t1$residuals)

t2 <- arimax(logy, order = c(0,0,3), seasonal = c(0,1,1),
             xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t2)
tsdiag(t2, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
ggAcf(t2$residuals)+ggtitle("ACF")
gghistogram(t2$residuals) + ggtitle("Histogram of residuals")
shapiro.test(t2$residuals)



t3 <- arimax(logy, order = c(3,0,0), seasonal = c(1,1,1),
             xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t3)
tsdiag(t3, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
ggAcf(t3$residuals)+ggtitle("ACF")
gghistogram(t3$residuals) + ggtitle("Histogram of residuals")
shapiro.test(t3$residuals)

t4 <- arimax(logy, order = c(0,0,3), seasonal = c(1,0,0),
             xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t4)
tsdiag(t4, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
ggAcf(t4$residuals)+ggtitle("ACF with MA(3), seasonal AR(1), IOs, pulse AR(1) and step")
gghistogram(t4$residuals) + ggtitle("Histogram of residuals")
##test for patterns in residuals
shapiro.test(t4$residuals)
runs(t4$residuals)


t5 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t5)
tsdiag(t5, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
ggAcf(t5$residuals)+ggtitle("ACF, seasonal AR(1), IOs, pulse AR(1) and step")
gghistogram(t5$residuals) + ggtitle("Histogram of residuals")
##test for patterns in residuals
shapiro.test(t5$residuals)
runs(t5$residuals)

t5c <- arimax(clean, order = c(0,0,0), seasonal = c(1,0,0),
             xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))
dev.off()
plot(logy,lw=1)
points(fitted(t5),pch=19)
lines(clean,col="red",lw=1)
points(fitted(t5c),pch=24)

#play
out<-c("out1")
test<-auto.arima(traindata[,"logy"],io=163)
summary(test)
tsdiag(test, gof=24, tol = 0.1, col = "red", omit.initial = TRUE)
runs(test$residuals)
shapiro.test(test$residuals)
detectIO(test)

fcast<-forecast(test,h=50)

tx <- arimax(clean, order = c(0,0,0),seasonal = c(1,0,0),
           #xreg = mydata[,c("out1", "out2")],
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

detectIO(tx)

summary(tx)
##test for patterns in residuals
shapiro.test(tx$residuals)
runs(tx$residuals)

tsdiag(tx, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
ggAcf(tx$residuals)+ggtitle("ACF")
ggPacf(tx$residuals)+ggtitle("ACF")
gghistogram(tx$residuals) + ggtitle("Histogram of residuals")

plot(logy)
points(fitted(tx))
