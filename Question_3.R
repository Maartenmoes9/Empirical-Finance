library("readxl")
library("forecast")
library("FinTS")
library("lmtest")
library("sandwich")
library("rugarch")
library(urca)
my_data <- read_excel("C:/Users/maart/OneDrive/Skrivebord/factor_data.xlsx")
ft <- my_data$factor
r1 <- my_data$asset1
r2 <- my_data$asset2
plot(r1, type = "l")
# Estimate the regression for first asset
model1 <- lm(r1 ~ ft)
# create object for the summary of model so we can get
# this information later
model1_data <- summary(model1)
print(model1_data)
#Diagnostics
Acf(model1$residuals, lag.max=26)
Pacf(model1$residuals, lag.max = 26)
Box.test(model1$residuals, lag = 26, type = c("Ljung-Box"))
ArchTest(model1$residuals, lags=12)
bgtest(model1, order = 12)

# Estimate the regression for second asset
model2 <- lm(r2 ~ ft)
model2_data <- summary(model2)
print(model2_data)
Acf(model2$residuals, lag.max=26)
Pacf(model2$residuals, lag.max = 26)
Box.test(model2$residuals, lag = 26, type = c("Ljung-Box"))
ArchTest(model2$residuals, lags=12)
bgtest(model2, order = 12)

#Check if the factor model needs more factors, should be very close to 0, which means that we dont need more factors
vec_errs <- cbind(model1$residuals, model2$residuals)
#Get the correlation matrix
cond <- cor(vec_errs)
#
print(cond)

# Estimate VaR
# level of significance of var
p <-0.05
# get the (1-p) quantile from standard normal distribution
quantile_n <- qnorm((1-p), mean = 0, sd = 1)
# capture betas (factor loadings)
loading1 <-model1_data$coefficients[2]
loading2 <-model2_data$coefficients[2]
# capture variance of shocks (specific factor)
var_shock1 <- model1_data$sigma^2
var_shock2 <- model2_data$sigma^2
# mean and variance factor
var_f <- var(ft)
mu_f <- mean(ft)
# variance/covariance of assets
var_r1 <- (loading1^2)*var_f  + var_shock1
var_r2 <- (loading2^2)*var_f  + var_shock2
cov12 <- loading1*loading2*var_f
# weights portfolio
w1 <- (var_r2 - cov12) / ((var_r1 + var_r2 - (2*cov12)))
w2 <- 1- w1
# mean assets
mean_r1 <- loading1*mu_f  
mean_r2 <- loading2*mu_f
# mean of portfolio
mu_p <- w1* mean_r1 + w2*mean_r2
#variance of portfolio
varp <- (w1^2)*var_r1 + (w2^2)*var_r2 + 2*w1*w2*cov12
# var estimation
var_est <- mu_p + quantile_n*sqrt(varp)
print(var_est)

