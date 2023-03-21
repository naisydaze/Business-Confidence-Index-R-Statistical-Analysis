#Installing prerequisite packages
packages <- c( 'forecast', 'tseries',"ggplot2", "data.table", 'tidyverse')
install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(install)

library(ggplot2)
library(forecast)
library(tseries)
library(data.table)
library(tidyverse)

#set working directory if necessary
options(scipen=999)
setwd()

#===============================================================================
##----------------------- DATA PREPARATION ------------------------------------------
#===============================================================================
data <- fread('owid-covid-data.csv', header = T,
              stringsAsFactors = T)

#update the data type of date
data$date <- as.Date(data$date, format = "%Y-%m-%d")

#country selection
Country <- data[location=='Australia']

#remove unnecessary variables
Country <- Country[,.(date,people_fully_vaccinated_per_hundred)]

#omit any NAs
Country <- na.omit(Country)

#===============================================================================
##----------------------- ARIMA MODEL ------------------------------------------
#===============================================================================

#Exact the first date(year and month)
year <- as.numeric(format(Country$date[1],"%Y"))
month <- as.numeric(format(Country$date[1],"%m"))

#Transfer to vaccination rate into time-series object
rate <- ts(Country[,'people_fully_vaccinated_per_hundred'], start = c(year,month),
           frequency = 365)
rate

#Graph the data
autoplot(rate) + ggtitle("People Fully Vaccinated in Australia") + ylab('total_vaccinations_per_hundred')

#Build ARIMA model where no seasonality is observed
fit_ARIMA <- auto.arima(rate,seasonal = F) 
summary(fit_ARIMA)

#Check residual plot
checkresiduals(fit_ARIMA)

#Perform forecasting for 80 periods ahead
fcast <- forecast(fit_ARIMA, h = 80)
fcast

#Check model accuracy 
qqnorm(fcast$residuals)
summary(fcast)
accuracy(fcast)

#===============================================================================
##----------------------- LOGISTIC CURVE ---------------------------------------
#===============================================================================

#Process data with date for training
Country2 <- Country[,Vrate:=people_fully_vaccinated_per_hundred/100]
Country2 <- Country2[,Vrate:=ifelse(date < "2021-01-11",0,Vrate)]
data_cleaned <- Country2[,.(date,Vrate)]

#Create date dataframe
date <- seq(as.Date("2021-01-27"), as.Date("2021-09-24"), by="days")
date <- as.data.frame(date)
data_cleaned <- merge(date,data_cleaned,all.x=T)

#Logistic regression(use S-shape sigmoid function)
m1 <- glm(Vrate ~ date, data = data_cleaned, family = "quasibinomial")

#Predict the full logistic curve with date for full curve
date <- seq(as.Date("2021-01-27"), as.Date("2022-12-01"), by="days")
date <- as.data.frame(date)
data_cleaned <- merge(data_cleaned,date,all.y=T)
data_cleaned$Predicted <- predict(m1, newdata = data.frame(date = date), type = "response")

#Plot the logistic curve
ggplot(data_cleaned, aes(x=date, y=Vrate)) +
  geom_point(size=2) +
  geom_line(data=data_cleaned, aes(x=date,y=Predicted), color="blue", size=2) +
  scale_y_continuous(label=scales::percent) +
  geom_hline(yintercept=0.85,size=1)+
  labs(y= "Vaccinated",
     title = "Proportion of Population Vaccinated",
     subtitle = "Logistic growth model") +
      theme(axis.title.x = element_blank())

#Predict the date when reaching herd immunity
data_cleaned <- as.data.table(data_cleaned)
data_sub <- data_cleaned[abs(Predicted-0.85)<0.005,]
data_sub$date

