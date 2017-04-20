library(dplyr)

data <- read.csv("restaurant1.csv", na.strings = '')
data <- data[which(data$INSPECTION.DATE != '01/01/1900'),]
data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%Y")
data <- data[data[, "lat"] != 0,]
data <- data[data[, "ing"] != 0,]

most_vio_rt<-data[!is.na(data$VIOLATION.CODE),][data[, "CRITICAL.FLAG"] == "Critical",] %>% count(CUISINE.DESCRIPTION, VIOLATION.CODE) %>%slice(which.max(n))
most_rt_vio<-data[!is.na(data$CUISINE.DESCRIPTION),][data[, "CRITICAL.FLAG"] == "Critical",] %>% count(VIOLATION.CODE, CUISINE.DESCRIPTION) %>%slice(which.max(n))

cleanData <- data[!is.na(data$SCORE),]
restByTimeData <- aggregate(cleanData$SCORE, by=list(cleanData$DBA, cleanData$CAMIS, cleanData$INSPECTION.DATE), FUN = mean)
colnames(restByTimeData) = c("restName", "restID", "time", "score")
restByIdData = aggregate(cleanData$SCORE, by = list(cleanData$VIOLATION.CODE, cleanData$CAMIS), FUN=mean)
colnames(restByIdData) = c("vioCode", "restId","scoreId")
restByZipTypeData = aggregate(cleanData$SCORE, by = list(cleanData$ZIPCODE, cleanData$CUISINE.DESCRIPTION, cleanData$VIOLATION.CODE), FUN=mean)
colnames(restByZipTypeData) = c("ZipCode", "Type", "Code", "Score")
