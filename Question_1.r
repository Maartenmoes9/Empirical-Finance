# Call necessary libraries (install them first, in case you haven't)
library("readxl")
library("forecast")
library("zoo") 
library("lmtest") # necessary for coeftest command
library("urca")
# call my forecasting function. It must be in the same folder
source("C:/Users/bertr/OneDrive/AAU Erhvervsøkonomi HA/8. semester/Empirical Finance/written assignment documents/expforecast.R", encoding = 'UTF-8')
# load data
my_data <- read_excel("C:/Users/bertr/OneDrive/AAU Erhvervsøkonomi HA/8. semester/Empirical Finance/written assignment documents/exchange.xls")

#declare as time series object (will be useful later in the course, in this case only generic)
exchange <- ts(my_data$return, start=1, end=262, frequency = 1)
plot(exchange)

mean_exchange <- mean(exchange)
print(mean_exchange)

#test for random walk

test_rw<-ur.df(exchange, type = "drift", lags = 10, selectlags = "AIC")  
summary(test_rw)


Box.test(exchange, lag = 26, type = c("Ljung-Box"))
Acf(exchange,lag = 26, main="ACF")
pacf (exchange, lag = 26, main="PACF")

# Number of lags selected by the rule of thumb presented in the class
# estimate an ARMA(2,1)
model2 <- arima(exchange, order=c(2,0,1))
print(model2)
# test coefficients to get p-value(not necessary, just to make it easier)
coeftest(model2)
# Diagnostics: test for white noise in the residuals of the above ARMA(2,1)
Box.test(model2$residuals, lag = 26, type = c("Ljung-Box"))


# Our analysis of ACF/PACF coincides with selection by Akaike

# Forecasting study:
# arma(2,1)
err_arma21 <- expforecast (exchange, 2, 1, 100, 1)
# 2= order ar, 1= order ma, 100= end of estimation window, 1=step-ahead
# calculate rmse with the vector of forecasting errors
rmse_arma21 <- sqrt(mean(err_arma21^2))
# arma(1,1)
err_arma11 <- expforecast (exchange, 1, 1, 100, 1)
rmse_arma11 <- sqrt(mean(err_arma11^2))
#approximate white noise (only intercept, no ar/ma term)
err_wn <- expforecast (exchange, 0, 0, 100, 1)
rmse_wn <- sqrt(mean(err_wn^2))
print(rmse_arma21)
print(rmse_arma11)
print(rmse_wn)

