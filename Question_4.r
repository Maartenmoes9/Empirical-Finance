library("readxl")
library("forecast")
library("lmtest")
library("sandwich")
library("urca")
library("tsDyn") 
library("zoo")

my_data <- read_excel("C:/Users/bertr/OneDrive/AAU Erhvervsøkonomi HA/8. semester/Empirical Finance/written assignment documents/cointegration.xlsx")
p1 <- my_data$logprice1 #already in logs
p2 <- my_data$logprice2

my_ts <- ts(my_data, 1, 300, frequency=1)
ts.plot(my_ts)

# Test data
test_ret1<-ur.df(p1, type = "drift", lags = 10, selectlags = "AIC")  
summary(test_ret1)
test_ret2<-ur.df(p2, type = "drift", lags = 10, selectlags = "AIC")  
summary(test_ret2)

# Estimate the cointegration relationship
eqm <- lm(p1 ~ p2)
summary(eqm)
ACF1 <- eqm$residuals
plot(ACF1, type='l')
Acf(ACF1, lag.max = 26)
test_ret3<-ur.df(ACF1, type = "none", lags = 10, selectlags = "AIC")  
summary(test_ret3)

delta <- sqrt(var(ect))
current_price_spread <- ACF1[length(ect)]
trade_decision1 <-   current_price_spread + delta
trade_decision1
# if negative asset 1 is undervalued and asset 2 overvalued
trade_decision2 <-  current_price_spread - delta
trade_decision2
# if positive asset 1 is overvalued and asset 2 is undervalued.
# If either, then no trading decision.
# we should buy asset 1 as it is undervalued and short asset 2
=2.991531-0.23696-2.13987*1.5617097

