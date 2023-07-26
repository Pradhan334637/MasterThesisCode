#######################################################
# This file is for forecasting Sales of Used Products

# Created on 10-05-2023 by Avishek Pradhan
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

# Load the data "UPS_Log.csv"
help (read.csv)
?read.csv
UPS_Log_CPI <- read.csv(file.choose(), header = T)

##strptime(UPS_Log_CPI$Date, format="%d-%m-%Y")

#Rename the column name
#colnames(UPS_Log_CPI)[1] <- "Date"#
#colnames(UPS_Log_CPI)[4] <- "COVIDcases_log"#

############ Data variable somehow includes Timezone information, so converting it to Date

# Convert date variable to POSIXct object
UPS_Log_CPI$Date <- as.POSIXct(UPS_Log_CPI$Date, format = "%d-%m-%Y")
# Format POSIXct object to character string without timezone information
UPS_Log_CPI$Date <- format(UPS_Log_CPI$Date, "%d-%m-%Y")
# Convert character string to Date object
UPS_Log_CPI$Date <- as.Date(UPS_Log_CPI$Date, format = "%d-%m-%Y")

# For tslm() to work, converting the Date variable into time index
UPS_Log_CPI_ts <- ts(UPS_Log_CPI[, -1], start = c(year(UPS_Log_CPI$Date[1]), month(UPS_Log_CPI$Date[1])), frequency = 12)

# Assign variable names to the time series
colnames(UPS_Log_CPI_ts) <- c("UPS_Log", "CPI", "COVIDcases_log")

# Create train and test sets. We fit our model in the training set and then we test for the accuracy of the model on the test set
train_UPS_Log_CPI <- window(UPS_Log_CPI_ts, start=c(2020,1), end=c(2022,6))
test_UPS_Log_CPI <- window(UPS_Log_CPI_ts, start=c(2022,7), end=c(2023,1))

# Fit the TSLM model with the train set (dependent variable UPS_Log, independent variable CPI, and control variable COVIDcases_log)
fit_UPS_Log_CPI <- tslm(UPS_Log ~ CPI + COVIDcases_log, data = train_UPS_Log_CPI)

# Print summary of tslm
summary(fit_UPS_Log_CPI)

# Changing TSLM method to LM method, as stargazer function only works with LM
# By converting the "tslm" object to an "lm" object, you can use the same model specifications and obtain the same estimated coefficients, standard errors, t-values, p-values, and other diagnostic statistics. The only difference is the object type and the methods associated with each object.

fit_UPS_Log_CPI <- lm(fit_UPS_Log_CPI)

stargazer(fit_UPS_Log_CPI, type = 'text')

############################### CHECKING STATIONARY & PLOT ##############################################
# Our series fit_UPS_Log_CPI has trends and seasonality.
# To remove the trend, we take the first difference.
# The first differenced series still has seasonality.

# extract the UPS_Log, CPI, COVIDcases_log variables as a time series object from train set
train_UPS_Log <- train_UPS_Log_CPI[, "UPS_Log"]
train_CPI <- train_UPS_Log_CPI[, "CPI"]
train_COVIDcases_log <- train_UPS_Log_CPI[, "COVIDcases_log"]

# extract the UPS_Log, CPI, COVIDcases_log variables as a time series object from test set
test_UPS_Log <- test_UPS_Log_CPI[,"UPS_Log"]

#######################################################
# Preliminary analysis
#######################################################
# Time Plot
autoplot(train_UPS_Log) + 
  ggtitle("Time Plot: Sales of used product through Idealo (Jan 20-Jun 22)") +
  ylab("Euro") 

# Data has slight trends. Investigate transformations.
# Some of our forecasting method cannot be utilized with trend, data has to be stationary.
# Before seasonal analysis, getting rid of the trend

#######################################################
# Seasonal differences are the change between one year to the next
# First differences are the change between one observation and the next

# Taking seasonal difference of the data to remove seasonality of UPS_Log, CPI, COVIDcases_log variables
train_sdiff_UPS_Log <- diff(train_UPS_Log, differences = 1, lag = 12)

# Taking first difference of the seasonality differenced data to remove trend and convert to stationary (UPS_Log, CPI, COVIDcases_log variables)
train_diff_UPS_Log <- diff(train_sdiff_UPS_Log)

#######################################################
# check stationarity using ADF test
# These are unit root test
# When data are non-stationary, then we say data has a unit root(no. of unit root = no. of differences)

adf.test(train_diff_UPS_Log) #p-value smaller than printed p-value, so null hypothesis is rejected; our time series data is stationary

# Time plot of differenced time series data
autoplot(train_diff_UPS_Log) + 
  ggtitle("Time Plot: Change in Sales of Used Products through Idealo (Jan 21-Jun 2023)") +
  ylab("Euro") # Series appear trend-stationary

####################################### *************** #################################################

# Forecast with various methods
#######################################################

##############
# Data is not stationary after first seasonal differenced then first differenced.
# Snaive will not be a good benchmark model as UPS_Log data is seasonal differenced initially.
# Also, due to 30 datasets in train_set, we do not have enough data to compare even two year cycles (due to seasonal differencing)
##############

##############
# Fit ETS (Exponential Smoothening) model
# ETS finds the best fit exponential smoothening model
# We can use regular data in place of differenced data in ETS

fit_ets_UPS_Log <- ets(train_UPS_Log) # Residual SD (sigma) is 0.0413
print(summary(fit_ets_UPS_Log))
checkresiduals(fit_ets_UPS_Log)

#################### Check ets residual SD, it is wrong.

# ETS model chosen ETS(M,Ad,N)
# Residual plot data looks random (which is good)
# ACF error values are within the 95% level (which is good)

##############
# Finding a better model : An ARIMA model
# For ARIMA, data has to be stationary
# Even if we are using non-stationary data, in the function- d=1 means first difference; D=1 means seasonal difference
# auto.arima will test different arima models to identify the best fit
# d=1 will take first diff of data automatically; D=1 will take first seasonal diff(so no need object to be stationary)
##############

fit_arima_UPS_Log <- auto.arima(train_UPS_Log,D=1,d=1,stepwise = FALSE, approximation = FALSE, trace = TRUE) 
print(summary(fit_arima_UPS_Log)) # sigma sq: 0.1193
checkresiduals(fit_arima_UPS_Log)
# To get Residual SD, take sq root of sigma sq
sqrt(0.1193) #Residual SD 0.3453983

# ACF and PACF plot for ETS
acf(residuals(fit_ets_UPS_Log), lag.max = 20, main = "ACF Plot - UPS_Log")
pacf(residuals(fit_ets_UPS_Log), lag.max = 20, main = "PACF Plot - UPS_Log")

# ACF and PACF plot for ARIMA
acf(residuals(fit_arima_UPS_Log), lag.max = 20, main = "ACF Plot - UPS_Log")
pacf(residuals(fit_arima_UPS_Log), lag.max = 20, main = "PACF Plot - UPS_Log")

# Chose model ARIMA(0,1,0)(0,1,0)[12]
# ETS residual was 0.0413; ARIMA residual was 0.3453983.
# Most ACF error values are within the 95% level (which is good)
# So best model is ETS due to lower residual value.

#################################################
# Forecast with ets and ARIMA model, to check which forecasting model is better
#################################################

# Forecast for next 7 periods (months) using ets forecast model
fcst_ets_UPS_Log <- forecast(fit_ets_UPS_Log, h=7)
print(summary(fcst_ets_UPS_Log)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ets_UPS_Log)

# Forecast for next 7 periods (months) using ARIMA forecast model
fcst_ARIMA_UPS_Log <- forecast(fit_arima_UPS_Log, h=7)
print(summary(fcst_ARIMA_UPS_Log)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ARIMA_UPS_Log)

# Accuracy check of test_UPS_Log with forecast numbers of ARIMA model
accuracy(fcst_ets_UPS_Log,test_UPS_Log) # We are interested in error of the test set, MASE 1.5071647, RMSE 3269412

# Accuracy check of test_Sessions with forecast numbers of ARIMA model
accuracy(fcst_ARIMA_UPS_Log,test_UPS_Log) # We are interested in error of the test set, MASE 1.2919423, RMSE 3103930

####### Both MASE and RMSE is lower in ARIMA model.

