#######################################################
# This file is for Number of IDT

# Created on 25-04-2023 by Avishek Pradhan
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
library(stargazer)   #professional regression table
library(Hmisc)       #add significance level to correlation table
library(car)         #for vif analysis

# Load the data "IDT.csv"
help (read.csv)
?read.csv
IDT_CPI <- read.csv(file.choose(), header = T)

#Rename the column name
colnames(IDT_CPI)[1] <- "Date"

##strptime(IDT_CPI$Date, format="%d-%m-%Y")##

############ Data variable somehow includes Timezone information, so converting it to Date

# Convert date variable to POSIXct object
IDT_CPI$Date <- as.POSIXct(IDT_CPI$Date, format = "%d-%m-%Y")
# Format POSIXct object to character string without timezone information
IDT_CPI$Date <- format(IDT_CPI$Date, "%d-%m-%Y")
# Convert character string to Date object
IDT_CPI$Date <- as.Date(IDT_CPI$Date, format = "%d-%m-%Y")

# For tslm() to work, converting the Date variable into time index
IDT_CPI_ts <- ts(IDT_CPI[, -1], start = c(year(IDT_CPI$Date[1]), month(IDT_CPI$Date[1])), frequency = 12)

# Assign variable names to the time series
colnames(IDT_CPI_ts) <- c("IDT", "CPI", "COVIDcases_log")

# Create train and test sets. We fit our model in the training set and then we test for the accuracy of the model on the test set
train_IDT_CPI <- window(IDT_CPI_ts, start=c(2020,1), end=c(2022,6))
test_IDT_CPI <- window(IDT_CPI_ts, start=c(2022,7), end=c(2023,1))

# Fit the TSLM model with the train set (dependent variable IDT, independent variable CPI, and control variable COVIDcases_log)
fit_IDT_CPI <- tslm(IDT ~ CPI + COVIDcases_log, data = train_IDT_CPI)

# Print summary of tslm
summary(fit_IDT_CPI)

# Changing TSLM method to LM method, as stargazer function only works with LM
# By converting the "tslm" object to an "lm" object, you can use the same model specifications and obtain the same estimated coefficients, standard errors, t-values, p-values, and other diagnostic statistics. The only difference is the object type and the methods associated with each object.

fit_IDT_CPI <- lm(fit_IDT_CPI)

stargazer(fit_IDT_CPI, type = 'text')

# vif analysis for multicollinearity

vif(fit_IDT_CPI)

############################### CHECKING STATIONARY & PLOT ##############################################
# Our series fit_IDT_CPI has trends and seasonality.
# To remove the trend, we take the first difference.
# The first differenced series still has seasonality.

# extract the IDT, CPI, COVIDcases_log variables as a time series object from train set
train_IDT <- train_IDT_CPI[, "IDT"]
train_CPI <- train_IDT_CPI[, "CPI"]
train_COVIDcases_log <- train_IDT_CPI[, "COVIDcases_log"]

# extract the IDT, CPI, COVIDcases_log variables as a time series object from test set
test_IDT <- test_IDT_CPI[,"IDT"]

#######################################################
# Preliminary analysis
#######################################################
# Time Plot
autoplot(train_IDT) + 
  ggtitle("Time Plot: Session traffic of German Ecommerce Customer in Idealo (Jan 20-Jun 22)") +
  ylab("Unit") 

# Data has slight trends. Investigate transformations.
# Some of our forecasting method cannot be utilized with trend, data has to be stationary.
# Before seasonal analysis, getting rid of the trend

#######################################################
# Seasonal differences are the change between one year to the next
# First differences are the change between one observation and the next

# Taking seasonal difference of the data to remove seasonality of IDT, CPI, COVIDcases_log variables
train_sdiff_IDT <- diff(train_IDT, differences = 1, lag = 12)

# Taking first difference of the seasonality differenced data to remove trend and convert to stationary (IDT, CPI, COVIDcases_log variables)
train_diff_IDT <- diff(train_sdiff_IDT)

#######################################################
# check stationarity using ADF test
# These are unit root test
# When data are non-stationary, then we say data has a unit root(no. of unit root = no. of differences)

adf.test(train_diff_IDT) #p-value = 0.01 (smaller than printed p-value), so null hypothesis is rejected; our time series data is stationary

# Time plot of differenced time series data
autoplot(train_diff_IDT) + 
  ggtitle("Time Plot: Change in Traffic IDT of German Ecommerce Customer in Idealo (Jan 21-Jun 22)") +
  ylab("Unit") # Series appear trend-stationary

####################################### *************** #################################################

# Forecast with various methods
#######################################################

##############
# Data is stationary, first seasonal differenced then first differenced.
# Snaive will not be a good benchmark model as IDT data is seasonal differenced initially.
# Also, due to 30 datasets in train_set, we do not have enough data to compare even two year cycles (due to seasonal differencing)
##############

##############
# Fit ETS (Exponential Smoothening) model
# ETS finds the best fit exponential smoothening model
# We can use regular data in place of differenced data in ETS

fit_ets_IDT <- ets(train_IDT) # Residual SD (sigma) is 14791805
print(summary(fit_ets_IDT))
checkresiduals(fit_ets_IDT)

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

fit_arima_IDT <- auto.arima(train_IDT,D=1,d=1,stepwise = FALSE, approximation = FALSE, trace = TRUE) 
print(summary(fit_arima_IDT)) # sigma sq: 8.903e+13
checkresiduals(fit_arima_IDT)
# To get Residual SD, take sq root of sigma sq
sqrt(8.903e+13) # Residual SD 9435571

# ACF and PACF plot for ETS
acf(residuals(fit_ets_IDT), lag.max = 20, main = "ACF Plot - IDT")
pacf(residuals(fit_ets_IDT), lag.max = 20, main = "PACF Plot - IDT")

# ACF and PACF plot for ARIMA
acf(residuals(fit_arima_IDT), lag.max = 20, main = "ACF Plot - IDT")
pacf(residuals(fit_arima_IDT), lag.max = 20, main = "PACF Plot - IDT")

# Chose model ARIMA(0,1,0)(0,1,0)[12]
# ETS residual was 14791805; ARIMA residual was 9435571.
# Most ACF error values are within the 95% level (which is good)
# So best model is ARIMA due to lower residual value.

#################################################
# Forecast with ets and ARIMA model, to check which forecasting model is better
#################################################

# Forecast for next 7 periods (months) using ets forecast model
fcst_ets_IDT <- forecast(fit_ets_IDT, h=7)
print(summary(fcst_ets_IDT)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ets_IDT)

# Forecast for next 7 periods (months) using ARIMA forecast model
fcst_ARIMA_IDT <- forecast(fit_arima_IDT, h=7)
print(summary(fcst_ARIMA_IDT)) #Point forecast is the best guess as well as associated prediction intervals
autoplot(fcst_ARIMA_IDT)

# Accuracy check of test_IDT with forecast numbers of ets model
accuracy(fcst_ets_IDT,test_IDT) # We are interested in error of the test set, MASE 1.2528194, RMSE 22312195

# Accuracy check of test_IDT with forecast numbers of ARIMA model
accuracy(fcst_ARIMA_IDT,test_IDT) # We are interested in error of the test set, MASE 0.3497557, RMSE 5512303

