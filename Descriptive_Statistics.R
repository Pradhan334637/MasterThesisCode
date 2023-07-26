#######################################################
# This file is for descriptive statistics of variables

# Created on 15-05-2023 by Avishek Pradhan
#######################################################

# Clear all variabels in workspace
rm(list = ls())

library(psych)

# Load the data "Descriptive statistic"
help (read.csv)
?read.csv
DS <- read.csv(file.choose(), header = T)

############## Descriptive statistics ##################

#Removing the Date variable from the dataset
DS_1 <- subset(DS, select = -Date)

#Descriptive statistics
psych::describe(DS_1)

# Convert date variable to POSIXct object
DS$Date <- as.POSIXct(DS$Date, format = "%d-%m-%Y")
# Format POSIXct object to character string without timezone information
DS$Date <- format(DS$Date, "%d-%m-%Y")
# Convert character string to Date object
DS$Date <- as.Date(DS$Date, format = "%d-%m-%Y")

# For tslm() to work, converting the Date variable into time index
DS_ts <- ts(DS[, -1], start = c(year(DS$Date[1]), month(DS$Date[1])), frequency = 12)

#Changing col names
colnames(DS_ts) <- c("APV", "UPS_Log", "IDT" , "CPI", "COV_Log")


############## Pearson correlation matrix ##################

#Creating train set
DS_2 <- window(DS_ts, start=c(2020,1), end=c(2022,6))

# Pearson correlation matrix
cor_matrix <- round(cor(DS_2[,c("APV", "UPS_Log", "IDT", "CPI", "COV_Log")], method = "pearson"),2)

print(cor_matrix)


