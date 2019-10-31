

#install.packages(c("usethis"),dep=TRUE)
#install.packages('tseries',dep=TRUE)
#install.packages('rio',dep=TRUE)
#install.packages('taRifx',dep=TRUE)
#install.packages('qpcR',dep=TRUE)
install.packages(c("TSA","car","aod","forecast","statsDK","tidyverse","lmtest"),dep=TRUE)
install.packages("Rtools",dep=TRUE)
rm(list=ls())
library(plyr)
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
library(taRifx)
library(qpcR)
library(lmtest)

##Margins for plot



##Draw in the unmarried population 2008-2019
folk1a_info<-retrieve_metadata("FOLK1A")
glimpse(folk1a_info)
variables <- get_variables(folk1a_info)
glimpse(variables)

variable_overview <- variables %>% 
  group_by(param) %>%
  slice(c(1,round(n()/2), n())) %>%
  ungroup()

variable_overview


unmarried<-retrieve_data("FOLK1A",
              OMRÅDE="000",CIVILSTAND="U,E,F",ALDER="*")

unmarried$alder<-destring(unmarried$ALDER, keep = "0-9.-")
unmarried<-subset(unmarried,KØN=="Total" & alder > 17)
temp_x<-aggregate(unmarried$INDHOLD, by = list(trt = unmarried$TID), FUN=sum)
temp_x$Q<-substring(temp_x$trt,5,6)
temp_x$year<-substring(temp_x$trt,1,4)
unmarried<-subset(temp_x,Q=="Q1",select=c("year","x"))



##Draw in the unmarried population from 2007
bef1a07_info<-retrieve_metadata("BEF1A07")
glimpse(bef1a07_info)
variables <- get_variables(bef1a07_info)
glimpse(variables)

variable_overview <- variables %>% 
  group_by(param) %>%
  slice(c(1,round(n()/2), n())) %>%
  ungroup()

variable_overview

unmarried2007<-retrieve_data("BEF1A07", 
                         CIVILSTAND="U,E,F,L,O",ALDER="*",TID="2007",KOEN="*")
unmarried2007$alder<-destring(unmarried2007$ALDER, keep = "0-9.-")
unmarried2007<-subset(unmarried2007,OMRÅDE=="All Denmark" & alder > 17 & TID ==2007)
temp_x<-aggregate(unmarried2007$INDHOLD, by = list(trt = unmarried2007$TID), FUN=sum)
names(temp_x)[names(temp_x)=="trt"] <- "year"

unmarried <- rbind.fill(temp_x,unmarried)



##Take the mean number of unmarried start and end of year
mean<-rollmean(unmarried$x, 2, align="left")

unmarried<-subset(unmarried,year<2019,select=c("year","x"))
unmarried$x<-mean


#Pull in monthly divorces through API for Statistics Denmark's public database

bevc3 <- retrieve_data("BEV3C",lang="en")
attach(bevc3)
bevc3$year<-substr(TID,1,4)
bevc3$month<-substr(TID,6,7)

##Limit sample to January, 2007 -- December, 2018

data<-subset(bevc3,BEVÆGELSEV=="Divorces" & year>2006 & year < 2019,select=c("INDHOLD","year","month"))
marriage_rate<-subset(bevc3,BEVÆGELSEV=="Marriages" & year>2006 & year < 2019,select=c("INDHOLD","year","month"))

detach(bevc3)

marriage_rate<-marriage_rate[order(data$year,data$month),]
marriage_rate<-merge(marriage_rate,unmarried,by="year")
marriage_rate$INDHOLD=50000*marriage_rate$INDHOLD/marriage_rate$x
marriage_ts<-ts(marriage_rate$INDHOLD,  f=12, start = c(2007, 1))



#plot the time series

png(filename="marriage_timeseries.png", width = 10, height = 5, units = "in", pointsize = 14,
    bg = "white",  res = 250,  type = c("windows"))
par(mai=c(.9,.95,0.2,0.42))
plot(marriage_ts,xlab = "Year", ylab = "Monthly marriages per 100,000 unmarried dyads",lwd=2)
abline(v=2013.5,lwd=2,lty="dashed")
abline(v=2013.75,lwd=2,lty="dotted")
dev.off()



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

mydata$running <- 1:nrow(mydata)

#Combine TS-data with denominator dataset
mydata <- merge(mydata,marriages,by="year",all=TRUE)


##Clean up aux. data sets
rm(list=c("marriages","info","bevc3","data","marr","variable_overview","variables","bef1a07_info",
          "folk1a_info","unmarried2007"))

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

png(filename="timeseries.png", width = 10, height = 5, units = "in", pointsize = 14,
    bg = "white",  res = 250,  type = c("windows"))
  par(mai=c(.9,.95,0.2,0.42))
  plot(y,xlab = "Year", ylab = "Monthly divorces per 100,000 marriages",lwd=2, ylim=c(40,320))
  abline(v=2013.5,lwd=2,lty="dashed")
  abline(v=2013.75,lwd=2,lty="dotted")
dev.off()

#AUX ACF and PACF plots
ggPacf(logy)
ggAcf(logy)

##Examine likely shape of AR-term
plot(y=logy,x=zlag(logy),type='p')


##Run naïve model for comparison
naive(logy)
res <- residuals(naive(logy))
autoplot(res) + xlab("Month") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of naive residuals")


#Run no-intervention model to address reviewer comment R2.c
#Model 0 in paper
t0 <- arimax(logy, order = c(0,0,0), seasonal = c(1,0,0),
             io=c(79,97,132))

summary(t0)

shapiro.test(t0$residuals)
runs(t0$residuals)

#Additional residual check outside scope of Box-Ljung
checkresiduals(t0)

aicc0<-AICc(t0)
aicc0



#Run ITSD model without step-increase in divorce risk
#Model 1 in paper
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

#Corrected AIC
aicc1<-AICc(t1)
aicc1

#Run ITSD model with step-increase in divorce risk
#Model 2 in paper
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

#Corrected AIC
aicc5<-AICc(t5)
aicc5

dev.off()
plot(logy,lwd=2)
points(fitted(t5),pch=19,cex=1)
points(fitted(t1),pch=17,cex=1)



##Testing models
lrtest(t5,t1)

##Union dissolutions

dissolution<-read_dta("dissolution.dta")
diss_ts<-ts(dissolution$count*100000/dissolution$sum,f=12,start = c(2007, 1))
plot(diss_ts)

png(filename="dissolution.png", width = 10, height = 5, units = "in", pointsize = 14,
    bg = "white",  res = 250,  type = c("windows"))
par(mai=c(.9,.95,0.2,0.42))
plot(diss_ts,xlab = "Year", ylab = "Monthly dissolution per 100,000 unmarried unions",lwd=2, ylim=c(400,1600),xlim=c(2007,2019))
abline(v=2013.5,lwd=2,lty="dashed")
abline(v=2013.75,lwd=2,lty="dotted")
dev.off()


mydata$running2<- mydata$running^2
#Run ITSD model with step-increase in divorce risk
#Model 2 in paper
t6 <- arimax(logy, order = c(0,1,1), seasonal = c(1,0,0),
             io=c(79,97,132),
             xtransf = mydata[,c("pulse","step")], transfer = list(c(1,0),c(0,0)))
aicc6<-AICc(t6)
aicc6
summary(t6)
tsdiag(t6, gof=24, tol = 0.1, col = "red", omit.initial = TRUE)

shapiro.test(t6$residuals)
runs(t6$residuals)
checkresiduals(t6,lag=24)
