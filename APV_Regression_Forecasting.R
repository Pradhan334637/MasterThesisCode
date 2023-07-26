#######################################################
# This file is for forecasting average purchase value

# Created on 14-03-2023 by Avishek Pradhan
#######################################################

# Clear all variabels in workspace
rm(list = ls())

# Install package fpp2

# Load forecasting package
library(fpp2)
library(forecast)
library(dplyr)
library(tidyverse)
library(tseries)
library(ggplot2)
library(stargazer)

# Load the data "APV.csv"
help (read.csv)
?read.csv
APV_CPI <- read.csv(file.choose(), header = T)

##strptime(APV_CPI$Date, format="%d-%m-%Y")##

############ Data variable somehow includes Timezone information, so converting it to Date

# Convert date variable to POSIXct object
APV_CPI$Date <- as.POSIXct(APV_CPI$Date, format = "%d-%m-%Y")
# Format POSIXct object to character string without timezone information
APV_CPI$Date <- format(APV_CPI$Date, "%d-%m-%Y")
# Convert character string to Date object
APV_CPI$Date <- as.Date(APV_CPI$Date, format = "%d-%m-%Y")

# For tslm() to work, converting the Date variable into time index
APV_CPI_ts <- ts(APV_CPI[, -1], start = c(year(APV_CPI$Date[1]), month(APV_CPI$Date[1])), frequency = 12)

# Assign variable names to the time series
colnames(APV_CPI_ts) <- c("APV", "CPI", "COVIDcases_log")

# Create train and test sets. We fit our model in the training set and then we test for the accuracy of the model on the test set
train_APV_CPI <- window(APV_CPI_ts, start=c(2020,1), end=c(2022,6))
test_APV_CPI <- window(APV_CPI_ts, start=c(2022,7), end=c(2023,1))


# Fit the TSLM model with the train set (dependent variable APV, independent variable CPI, and control variable COVIDcases_log)
fit_APV_CPI <- tslm(APV ~ CPI + COVIDcases_log, data = train_APV_CPI)

# Print summary of tslm
summary(fit_APV_CPI)

# Changing TSLM method to LM method, as stargazer function only works with LM
# By converting the "tslm" object to an "lm" object, you can use the same model specifications and obtain the same estimated coefficients, standard errors, t-values, p-values, and other diagnostic statistics. The only difference is the object type and the methods associated with each object.

fit_APV_CPI <- lm(fit_APV_CPI)

stargazer(fit_APV_CPI, type = 'text')

############################### CHECKING STATIONARY & PLOT ##############################################
# Our series fit_APV_CPI has trends and seasonality.
# To remove the trend, we take the first difference.
# The first differenced series still has seasonality.

# extract the APV, CPI, COVIDcases_log variables as a time series object from train set
train_APV <- train_APV_CPI[, "APV"]
train_CPI <- train_APV_CPI[, "CPI"]
train_COVIDcases_log <- train_APV_CPI[, "COVIDcases_log"]

# extract the APV, CPI, COVIDcases_log variables as a time series object from test set
test_APV <- test_APV_CPI[,"APV"]

#######################################################
# Preliminary analysis
#######################################################
# Time Plot
autoplot(train_APV) + 
  ggtitle("Time Plot: Average Purchase Value of German Ecommerce Customer (Jan 2020-Jun 2022)") +
  ylab("Euro") 

# Data has strong trends. Investigate transformations.
# Some of our forecasting method cannot be utilized with trend, data has to be stationary.
# Before seasonal analysis, getting rid of the trend

#######################################################
# Seasonal differences are the change between one year to the next
# First differences are the change between one observation and the next

# Taking seasonal difference of the data to remove seasonality of APV, CPI, COVIDcases_log variables
train_sdiff_APV <- diff(train_APV, differences = 1, lag = 12)

# Taking first difference of the seasonality differenced data to remove trend and convert to stationary (APV, CPI, COVIDcases_log variables)
train_diff_APV <- diff(train_sdiff_APV)

#######################################################
# check stationarity using ADF test
# These are unit root test
# When data are non-stationary, then we say data has a unit root(no. of unit root = no. of differences)

adf.test(train_diff_APV) #p-value = 0.01739, so null hypothesis is rejected; our time series data is stationary

# Time plot of differenced time series data
autoplot(train_diff_APV) + 
  ggtitle("Time Plot: Change in Average Purchase Value of German Ecommerce Customer (Jan 2020-Jun 2022)") +
  ylab("Euro") # Series appear trend-stationary

####################################### *************** #################################################

# Forecast with various methods
#######################################################

##############
# Data is stationary, first seasonal differenced then first differenced.
# Snaive will not be a good benchmark model as APV data is seasonal differenced initially.
# Also, due to 30 datasets in train_set, we do not have enough data to compare even two year cycles (due to seasonal differencing)
##############

##############
# Fit ETS (Exponential Smoothening) model
# ETS finds the best fit exponential smoothening model
# We can use regular data in place of differenced data in ETS

fit_ets_APV <- ets(train_APV) # Residual SD (sigma) is 7.8701
print(summary(fit_ets_APV))
checkresiduals(fit_ets_APV)

# ETS model chosen ETS(A,N,N)
# Residual plot data looks random (which is good)
# ACF error values are within the 95% level (which is good)

##############
# Finding a better model : An ARIMA model
# For ARIMA, data has to be stationary
# Even if we are using non-stationary data, in the function- d=1 means first difference; D=1 means seasonal difference
# auto.arima will test different arima models to identify the best fit
# d=1 will take first diff of data automatically; D=1 will take first seasonal diff(so no need object to be stationary)
##############

fit_arima_APV <- auto.arima(train_APV,D=1, d=1, stepwise = FALSE, approximation = FALSE, trace = TRUE) 
print(summary(fit_arima_APV)) # sigma sq: 47.51
checkresiduals(fit_arima_APV)

# ACF and PACF plot for ETS
acf(residuals(fit_ets_APV), lag.max = 20, main = "ACF Plot - APV")
pacf(residuals(fit_ets_APV), lag.max = 20, main = "PACF Plot - APV")

# ACF and PACF plot for ARIMA
acf(residuals(fit_arima_APV), lag.max = 20, main = "ACF Plot - APV")
pacf(residuals(fit_arima_APV), lag.max = 20, main = "PACF Plot - APV")

# To get Residual SD, take sq root of sigma sq
sqrt(47.51) # Residual SD 6.89275

# Chose model ARIMA(0,1,0)(0,1,0)[12] with zero mean (order (p,d,q)=(AR,Diff,MA))
# The first set of numbers (0,1,0) represents the order of differencing used in the model. In this case, the data was differenced once (d=1), indicating that the first differences of the series were used.The second set of numbers (0,1,0) represents the seasonal order of differencing. In this case, the seasonal differences were also taken once with a lag of 12 (D=1), indicating that the seasonal first differences were used.The [12] indicates that the model has a seasonal period of 12 months.
# ETS residual was 7.8701; ARIMA residual was 6.89275.
# Most ACF (AUto correlation Function) error values are within the 95% level (which is good)
# So best model is ARIMA.

#################################################
# Forecast with ets and ARIMA model, to check which forecasting model is better
#################################################

# Forecast for next 7 periods (months) using ARIMA forecast model
fcst_ets_APV <- forecast(fit_ets_APV, h=7)
print(summary(fcst_ets_APV)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ets_APV)

# Forecast for next 7 periods (months) using ARIMA forecast model
fcst_ARIMA_APV <- forecast(fit_arima_APV, h=7)
print(summary(fcst_ARIMA_APV)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ARIMA_APV)

#######compare both forecasting models with accuracy 

# Accuracy check of test_APV with forecast numbers of ARIMA model
accuracy(fcst_ets_APV,test_APV) # We are interested in error of the test set, MASE 0.4171011

# Accuracy check of test_APV with forecast numbers of ARIMA model
accuracy(fcst_ARIMA_APV,test_APV) # We are interested in error of the test set,MASE 0.6172328
 