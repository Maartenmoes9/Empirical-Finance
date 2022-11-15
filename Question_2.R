library("readxl")
library("forecast")
library("FinTS")
library("rugarch")
# load data
my_data <- read_excel("C:/Users/maart/OneDrive/Skrivebord/exchange.xls")
#declare as time series object
my_ts= ts(my_data, start=1, end=262, frequency = 1)
plot(my_ts)
#Let's check if the data is white noise using Box-test, ACF, PACF
# 24 lags come from the rule-of-thumb for ACF/PACF
Acf(my_ts,lag = 26, main="Sample ACF")
pacf (my_ts, lag = 26, main="Sample PACF")
Box.test(my_ts, lag = 26, type = c("Ljung-Box"))

Acf(my_ts^2,lag = 26, main="Sample ACF of Squared Error Terms")
pacf (my_ts^2, lag = 26, main="Sample PACF of Squared Error Terms")
# Box-test
Box.test(my_ts^2, lag = 26, type = c("Ljung-Box"))
#ARCH-LM test
test <- ArchTest (my_ts, lags=12, demean = TRUE)
print(test)
#Estimate model one with ARMA (1,1)-GARCH(1,1) With gaussian distribution and zero mean.
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                   mean.model = list(armaOrder = c(1, 1), include.mean = FALSE ), 
                   distribution.model = "norm")
model1 <- ugarchfit(spec, my_ts)
#Use the parameters to estimate model 1
print(model1)
#Estimate model two with ARMA (1,1)-GARCH(1,1) With student-t distribution and zero mean.
spec2 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(1, 1), include.mean = FALSE ), 
                    distribution.model = "std")
model2 <- ugarchfit(spec2, my_ts)
#Use the parameters to estimate model 2
print(model2)
#Test to see which model has the least information loss
infocriteria(model1)
infocriteria(model2)
# Estimate VaR
# level of significance of var
p <-0.05
#Forecast our volatility(std) only 1 step ahead, since the data is weekly.
fore <-ugarchforecast(model1,n.ahead = 1)
#This command gives us the the forecasted volatility t+1
fore@forecast$sigmaFor
# get the (1-p) quantile from standard normal distribution
quantile_n <- qnorm((1-p), mean = 0, sd = 1)
# var estimation
var_est <- 0 + quantile_n*0.5104809
print(var_est)
