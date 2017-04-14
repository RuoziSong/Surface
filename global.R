library(dplyr)

data <- read.csv("restaurant1.csv", na.strings = '')
data <- data[which(data$INSPECTION.DATE != '01/01/1900'),]
data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
data <- data[data[, "lat"] != 0,]
data <- data[data[, "ing"] != 0,]