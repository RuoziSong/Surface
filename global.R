library(dplyr)
library(rgdal)
library(leaflet)
library(dplyr)

# Load data
data <- read.csv("data/restaurant1.csv", na.strings = '')
data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%y")
data <- data[which(data$INSPECTION.DATE != '2000-01-01'),]
data <- data[data[, "lat"] != 0,]
data <- data[data[, "ing"] != 0,]
data$YEAR <- as.numeric(format(data$INSPECTION.DATE, "%Y")) 



# Data processing for panel 3
most_vio_rt<-data[!is.na(data$VIOLATION.CODE),][data[, "CRITICAL.FLAG"] == "Critical",] %>% count(CUISINE.DESCRIPTION, VIOLATION.CODE) %>%slice(which.max(n))
most_rt_vio<-data[!is.na(data$CUISINE.DESCRIPTION),][data[, "CRITICAL.FLAG"] == "Critical",] %>% count(VIOLATION.CODE, CUISINE.DESCRIPTION) %>%slice(which.max(n))
cleanData <- data[!is.na(data$SCORE),]
restByTimeData <- aggregate(cleanData$SCORE, by=list(cleanData$DBA, cleanData$CAMIS, cleanData$INSPECTION.DATE), FUN = mean)
colnames(restByTimeData) = c("restName", "restID", "time", "score")
restByIdData = aggregate(cleanData$SCORE, by = list(cleanData$VIOLATION.CODE, cleanData$CAMIS), FUN=mean)
colnames(restByIdData) = c("vioCode", "restId","scoreId")
restByZipTypeData = aggregate(cleanData$SCORE, by = list(cleanData$ZIPCODE, cleanData$CUISINE.DESCRIPTION, cleanData$VIOLATION.CODE), FUN=mean)
colnames(restByZipTypeData) = c("ZipCode", "Type", "Code", "Score")



# Data processing for panel 2
## Read zipcode boundaires in NYC
### boundaries from https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
zipcodes<-readOGR(dsn="data/zipcode-boundaries", layer="ZIP_CODE_040114")
## Cut out unnecessary columns
zipcodes@data <- zipcodes@data[,c(1,2,6,10,11,12)]
## transform to WGS884 reference system 
zipcodes<-spTransform(zipcodes, CRS("+init=epsg:4326"))
## Find the edges of our map
bounds<-bbox(zipcodes)
## Generate data for second panel: inspection score by zipcode
data_score <- data[which(!is.na(data$SCORE)),]
data_score <- data_score[which(!is.na(data_score$VIOLATION.CODE)),]
data_score$VIOLATION.TYPE <- sapply(data_score$VIOLATION.CODE, substring, 1, 2)
## Reclassify violation type to make it easily understood
data_score <- data_score[which(data_score$VIOLATION.TYPE!='22'),]
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='02'] <- '02 Probelm with food temperature'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='03'] <- '03 Probelm with food source'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='04'] <- '04 Probelm with food preparation'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='05'] <- '05 Probelm with kitchen facilities'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='06'] <- '06 Probelm with food containers'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='07'] <- '07 Probelm with restaurant management'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='08'] <- '08 Probelm with vermin proof'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='09'] <- '09 Probelm with thawing procedures'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='10'] <- '10 Probelm with non-kitchen facilities'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='15'] <- '15 Violation of smoke free'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='16'] <- '16 Problem with nutritional fact labels'
data_score$VIOLATION.TYPE[data_score$VIOLATION.TYPE=='20'] <- '20 Current letter grade card not posted'
# Generate result dataframe
score_zip <- data_score %>% select(ZIPCODE,YEAR,VIOLATION.TYPE,SCORE) %>%
  group_by(ZIPCODE,VIOLATION.TYPE,YEAR) %>%
  dplyr::summarise(Mean = round(mean(SCORE),digits = 2), Median = round(median(SCORE),digits=2))
score_zip <- score_zip[score_zip$YEAR >= 2013,]
maxcolor <- max(max(score_zip$Mean),max(score_zip$Median))
pal <- colorQuantile("YlGn", c(0,maxcolor), n = 30, na.color = "#808080") 