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
library(zoo)
library(statsDK)
library(tidyverse)
library(ggfortify)


#Pull in monthly divorces through API for Statistics Denmark's public database

bevc3 <- retrieve_data("BEV3C",lang="en")
attach(bevc3)
bevc3$year<-substr(TID,1,4)
bevc3$month<-substr(TID,6,7)

##Limit sample to January, 2007 -- December, 2018

data<-subset(bevc3,BEVÆGELSEV=="Divorces" & year>2006 & year < 2019,select=c("INDHOLD","year","month"))
detach(bevc3)

data<-data[order(data$year,data$month),]
mydata<-subset(data,select=c("year","month"))

##Data now include year, month, and number of divorces
mydata$incident<-data$INDHOLD

###Pull in mean number of married couples per year through API for Statistics Denmark's public database

#Reads meta information on data set
info<-retrieve_metadata("FAM44N")
glimpse(info)
variables <- get_variables(info)
glimpse(variables)

variable_overview <- variables %>% 
  group_by(param) %>%
  slice(c(1,round(n()/2), n())) %>%
  ungroup()

variable_overview

##Retrieve number of mariages at start of year 2007-2019
data<-retrieve_data("FAM44N",Tid = "2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019",
                    OMRÅDE="000",
                    FAMTYP="PARF")

##Aggregate across number of children/individuals living in household
marr<-aggregate(data$INDHOLD, by = list(trt = data$TID), FUN=sum)

##Build rolling mean of number of marriages using two adjacent years
mean<-rollmean(marr$x, 2, align="left")
year <-2007:2018
marriages<-data.frame(year,mean)


#intervention designs

mydata$pulse <- as.numeric(mydata$year == 2013 & mydata$month==10)

mydata$step <- as.numeric((mydata$year >= 2013 & mydata$month>=10) |(mydata$year >= 2014) )


#Combine TS-data with denominator dataset
mydata <- merge(mydata,marriages,by="year",all=TRUE)


##Clean up aux. data sets
rm(list=c("marriages","info","bevc3","data","marr","variable_overview","variables"))

#saves a copy of the estimation sample
write.csv2(mydata,file="sample.csv")
mydata<-read.csv2(file="sample.csv")

#divorce rate measured as monthly divorce per 100,000 married couples mid year
mydata$y<-(mydata$incident/mydata$mean)*100000

#Log and time series set the data
y<-ts(mydata$y, f=12)
logy <- log(mydata$y)
logy <- ts(logy, frequency = 12, start = c(2007, 1))
y <- ts(y, frequency = 12, start = c(2007, 1))
mydata$logy <- ts(log(mydata$y),  f=12, start = c(2007, 1))

newdata<-ts(mydata,start=1,f=12)

##Build training set used for forcasting
traindata<-newdata[1:78,9:9]
traindata<-ts(traindata,f=12,start = c(2007, 1))

#Examine nature of training set (Seasonal AR[1])
ggAcf(traindata)




#plot the time series

png(filename="timeseries.png", width = 10, height = 8, units = "in", pointsize = 14,
    bg = "white",  res = 250,  type = c("windows"))
  plot(y,xlab = "Year", ylab = "Monthly divorces per 100,000 marriages",lwd=2, ylim=c(40,320))
  abline(v=2013.5,lwd=2,lty="dashed")
  abline(v=2013.75,lwd=2,lty="dotted")
dev.off()

#AUX ACF and PACF plots
ggPacf(logy)
ggAcf(logy)

##Examine likely shape of AR-term
plot(y=logy,x=zlag(logy),type='p')


##Rune naïve model for comparison
res <- residuals(naive(logy))
autoplot(res) + xlab("Month") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of naive residuals")


#Run ITSD model without step-increase in divorce risk
t1 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             io=c(79,97,132),
             xtransf = mydata[,c("pulse")], transfer = list(c(1,0)))

summary(t1)

png(filename="diagnostic_t1.png", width = 5, height = 10, units = "in", pointsize = 18,
    bg = "white",  res = 250,  type = c("windows"))
tsdiag(t1, gof=24, tol = 0.1, col = "red", omit.initial = FALSE)
dev.off()
shapiro.test(t1$residuals)
runs(t1$residuals)

#Additional residual check outside scope of Box-Ljung
checkresiduals(t1)


t5 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             io=c(79,97,132),
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))

summary(t5)

png(filename="diagnostic_t5.png", width = 5, height = 10, units = "in", pointsize = 18,
    bg = "white",  res = 250,  type = c("windows"))
tsdiag(t5, gof=24, tol = 0.1, col = "red", omit.initial = TRUE)
dev.off()
ggAcf(t5$residuals)+ggtitle("ACF, seasonal AR(1), IOs, pulse AR(1) and step-function")
gghistogram(t5$residuals) + ggtitle("Histogram of residuals")
##test for patterns in residuals
shapiro.test(t5$residuals)
runs(t5$residuals)

#Additional residual check outside scope of Box-Ljung
checkresiduals(t5,lag=24)



dev.off()
plot(logy,lwd=2)
points(fitted(t5),pch=19,cex=1)
points(fitted(t1),pch=17,cex=1)




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
plot(logy,xlab = "Year", ylab = "Monthly divorce per 100,000 marriages", ylim=c(3.7,5.7),lwd=2)
points(fitted(t5),pch=19,cex=1.5)
points(fitted(t1),pch=17,cex=1.5)

