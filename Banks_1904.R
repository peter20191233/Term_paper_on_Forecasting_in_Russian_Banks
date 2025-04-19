rm(list=ls()) 

#нужно задать папку где хранятся файлы
setwd("~/Documents/02 Studenti/Petr_kurs")

library(readxl)
library(dplyr)
library(tidyverse)
library(quantmod)
library(fBasics)
library(urca)



data <- read_excel("Data_forecast_banks.xlsx", sheet = "Data")
n <- NCOL(data)

data_all <- data.frame(Date = seq(min(data$Date), max(data$Date), by = "day"))
data_all <- data_all %>% left_join(data, by = "Date")

ts_data <- ts(data_all[,-1], start = c(2015,5), frequency = 365)
library(zoo)
ts_data <- na.locf(ts_data, na.rm = FALSE) #замена NA значений предыдущими



par(mfrow = c(3, 1)) 
ts.plot(ts_data[,1], main="BSPS stock prices")
ts.plot(ts_data[,2], main="VTBR stock prices")
ts.plot(ts_data[,3], main="SBER stock prices")
par(mfrow = c(1, 1)) 

ts_data_log <- diff(log(ts_data))

s1 <- ts_data_log[,1]
s1 <- s1[!s1==0]

s2 <- ts_data_log[,2]
s2 <- s2[!s2==0]

s3 <- ts_data_log[,3]
s3 <- s3[!s3==0]


par(mfrow = c(3, 1)) 
ts.plot(s1, main="BSPS log returns")
ts.plot(s2, main="VTBR log returns")
ts.plot(s3, main="SBER log returns")
par(mfrow = c(1, 1)) 

train <- window(s1, end=c(2023,365))
test <- window(s1, start=c(2024,1))

par(mfrow = c(2, 1)) 
ts.plot(train, main="BSPS log returns 2015-2023")
ts.plot(test, main="BSPS log returns 2024-2025")
par(mfrow = c(1, 1)) 

library(forecast)
summary(ur.df(train, type="none", lags = 24, selectlags ="BIC"))
mod_arima1 <- auto.arima(train)
summary(mod_arima1)

p0 <- mod_arima1$fitted
ts.plot(train, p0, col=c("black", "royalblue"), main= "ARIMA forecast 2015-2023")

p1 <- predict(mod_arima1, n.ahead=NROW(test))
#ts.plot(test, p1$pred, col=c("black", "blue"), main= "ARIMA forecast 2024-2025")

mrse1 <- sqrt(mean((test-p1$pred)^2))
mrse1

smape <- function(actual, predicted) {
  mean(200 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
}

smape(test,p1$pred)

#install.packages("fGarch")
library(fGarch)

mod_garch1 <- garchFit(~ arma(1,0) + garch(1,1), data = train)
summary(mod_garch1)

p02 <- fitted(mod_garch1)
ts.plot(train, p02, col=c("black", "pink"), main= "GARCH forecast")

p2 <- predict(mod_garch1, n.ahead=NROW(test))
ts.plot(test, p2$meanForecast, col=c("black", "red"), main= "GARCH forecast")
mrse2 <- sqrt(mean((test-p2$meanForecast)^2))
mrse2

smape(test,p2$meanForecast)

library(prophet)
library(tidyverse)
library(lubridate)

time_index <- seq.Date(from = as.Date("2025-01-01"), length.out=length(s1) ,by = "day")

data_p <- data.frame(ds=time_index, y=s1)
train2 <- data_p[1:2200,]
test2 <- data_p[2201:2489,]

m=prophet(train2,  yearly.seasonality=TRUE, weekly.seasonality = TRUE, daily.seasonality = TRUE) 
summary(m)

n <- length(data_p$y[2201:2489])

future <- make_future_dataframe(m, periods = n, freq="day", include_history = T)
tail(future) 

forecast <- predict(m, future)
summary(forecast)
tail(forecast[c("ds", "yhat", "yhat_lower", "yhat_upper")])

plot(m, forecast)
smape(test2$y, forecast$yhat)



