rm(list=ls()) 

library(readxl)
dd <- read_excel("Data_monthly.xlsx", sheet = "Data")
ts_dd <- ts(dd[,-1], start = c(2015,1), frequency = 12)

ts.plot(ts_dd, col = c("violet", "royalblue", "darkblue"), main="Stock prices 2015-2025")
legend("top", 
       legend = colnames(dd)[-1], 
       col = c("violet", "royalblue", "darkblue"), 
       lty = 1, cex=0.7)

ts_dd_log <- diff(log(ts_dd))
ts.plot(ts_dd_log, col = c("violet", "royalblue", "darkblue"), main="Logarithmic returns 2015-2025")
legend("bottomright", 
       legend = colnames(dd)[-1], 
       col = c("violet", "royalblue", "darkblue"), 
       lty = 1, cex=0.7)


par(mfrow=c(3,1))
ts.plot(dd[,2], main="Stock prices 2015-2025 BSPS")
ts.plot(dd[,3], main="Stock prices 2015-2025 SBER")
ts.plot(dd[,4], main="Stock prices 2015-2025 VTBR")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
acf(ts_dd_log[,1], main="ACF BSPS")
acf(ts_dd_log[,2], main="ACF SBER")
acf(ts_dd_log[,3], main="ACF VTBR")
par(mfrow=c(1,1))

train <- ts_dd_log[1:99,]
test <- ts_dd_log[100:122,]

smape <- function(actual, predicted) {
  mean(200 * abs(predicted - actual) / (abs(actual) + abs(predicted)))
}

library(forecast)
library(stats)
library(urca)

##########################
#ARIMA BSPS
#########################
summary(ur.df(train[,1], type="none", lags = 24, selectlags ="BIC"))
auto.arima(train[,1], stepwise = TRUE, approximation = TRUE, seasonal = TRUE)

mod_arima1 <- arima(train[,1], order = c(1,0,1))
summary(mod_arima1)

par(mfrow=c(2,1))
p0 <- fitted(mod_arima1)
ts.plot(train[,1], p0, col=c("black", "royalblue"), main= "ARIMA forecast 2015-2023 BSPS")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)

p1 <- predict(mod_arima1, n.ahead=NROW(test))
ts.plot(test[,1], p1$pred, col=c("black", "royalblue"), main= "ARIMA forecast 2024-2025 BSPS")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,1],p0)
smape(test[,1],p1$pred)

##########################
#ARIMA SBER 
#########################
summary(ur.df(train[,2], type="none", lags = 24, selectlags ="BIC"))
auto.arima(train[,2], stepwise = TRUE, approximation = TRUE, seasonal = TRUE)

mod_arima2 <- arima(train[,2], order = c(1,0,1))
summary(mod_arima2)

par(mfrow=c(1,2))
p0 <- fitted(mod_arima2)
ts.plot(train[,2], p0, col=c("black", "royalblue"), main= "ARIMA forecast 2015-2023 SBER")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)

p1 <- predict(mod_arima2, n.ahead=NROW(test))
ts.plot(test[,2], p1$pred, col=c("black", "royalblue"), main= "ARIMA forecast 2024-2025 SBER")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,2],p0)
smape(test[,2],p1$pred)

##########################
#ARIMA VTBR
#########################
summary(ur.df(train[,3], type="none", lags = 24, selectlags ="BIC"))

mod_arima3 <- arima(train[,3], order = c(1,0,1))
summary(mod_arima3)

par(mfrow=c(2,1))
p0 <- fitted(mod_arima3)
ts.plot(train[,3], p0, col=c("black", "royalblue"), main= "ARIMA forecast 2015-2023 VTBR")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)

p1 <- predict(mod_arima3, n.ahead=NROW(test))
ts.plot(test[,3], p1$pred, col=c("black", "royalblue"), main= "ARIMA forecast 2024-2025 VTBR")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "royalblue"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,3],p0)
smape(test[,3],p1$pred)


#################
#GARCH BSPS
#################

library(fGarch)
mod_garch1 <- garchFit(~ arma(1,0) + garch(1,1), data = train[,1])
summary(mod_garch1)

par(mfrow=c(1,2))
p02 <- fitted(mod_garch1)
train <- as.ts(train)
ts.plot(train[,1], p02, col=c("black", "red"), main="GARCH forecast 2015-2023 BSPS")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)

p2 <- predict(mod_garch1, n.ahead=NROW(test))
p2 <- as.ts(p2$meanForecast)
ts.plot(test[,1], p2, col=c("black", "red"), main= "GARCH forecast 2024-2025 BSPS")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,1],p02)
smape(test[,1],p2)

#################
#GARCH SBER
#################
mod_garch2 <- garchFit(~ arma(1,0) + garch(1,1), data = train[,2])
summary(mod_garch2)

par(mfrow=c(1,2))
p02 <- fitted(mod_garch2)
ts.plot(train[,2], p02, col=c("black", "red"), main="GARCH forecast 2015-2023 SBER")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)

p2 <- predict(mod_garch1, n.ahead=NROW(test))
p2 <- as.ts(p2$meanForecast)
ts.plot(test[,2], p2, col=c("black", "red"), main= "GARCH forecast 2024-2025 SBER")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,2],p02)
smape(test[,2],p2)

#################
#GARCH VTBR
#################
mod_garch3 <- garchFit(~ arma(1,0) + garch(1,1), data = train[,3])
summary(mod_garch3)

par(mfrow=c(1,2))
p02 <- fitted(mod_garch3)
ts.plot(train[,3], p02, col=c("black", "red"), main="GARCH forecast 2015-2023 VTBR")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)

p2 <- predict(mod_garch1, n.ahead=NROW(test))
p2 <- as.ts(p2$meanForecast)
ts.plot(test[,3], p2, col=c("black", "red"), main= "GARCH forecast 2024-2025 VTBR")
legend("bottomright", 
       legend = c("actual", "forecast"), 
       col = c("black", "red"), 
       lty = 1, cex=0.3)
par(mfrow=c(1,1))

smape(train[,3],p02)
smape(test[,3],p2)

library(prophet)
library(tidyverse)
library(lubridate)

#####################
#PROPHET BSPS
#####################

data_p <- data.frame(ds=dd$Date, y=dd$BSPS)

train2 <- data_p[1:99,]
test2 <- data_p[100:123,]

m=prophet(train2, yearly.seasonality = TRUE) 
summary(m)

n <- NROW(test2)

future <- make_future_dataframe(m, periods = n, freq="month", include_history = T)
tail(future)

forecast <- predict(m, future)
summary(forecast)


plot(m, forecast)
smape(train2$y, forecast$yhat[1:99])
smape(test2$y, forecast$yhat[100:123])

#####################
#PROPHET SBER
#####################

data_p <- data.frame(ds=dd$Date, y=dd$SBER)

train2 <- data_p[1:99,]
test2 <- data_p[100:123,]

m=prophet(train2, yearly.seasonality = TRUE) 
summary(m)

n <- NROW(test2)

future <- make_future_dataframe(m, periods = n, freq="month", include_history = T)
tail(future)

forecast <- predict(m, future)
summary(forecast)

plot(m, forecast)
smape(train2$y, forecast$yhat[1:99])
smape(test2$y, forecast$yhat[100:123])


#####################
#PROPHET SBER
#####################

data_p <- data.frame(ds=dd$Date, y=dd$VTBR)

train2 <- data_p[1:99,]
test2 <- data_p[100:123,]

m=prophet(train2, yearly.seasonality = TRUE) 
summary(m)

n <- NROW(test2)

future <- make_future_dataframe(m, periods = n, freq="month", include_history = T)
tail(future)

forecast <- predict(m, future)
summary(forecast)

plot(m, forecast)
smape(train2$y, forecast$yhat[1:99])
smape(test2$y, forecast$yhat[100:123])
