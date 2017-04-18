library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)

# boundaries from https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
zipcodes<-readOGR(dsn="Data/zipcode-boundaries", layer="ZIP_CODE_040114")
#boroughs<-readOGR(dsn="Data/statistical-gis-boundaries-london/ESRI", layer="London_Borough_Excluding_MHW")

# Cut out unnecessary columns
zipcodes@data <- zipcodes@data[,c(1,2,6,10,11,12)]
#boroughs@data[,c(1,2)]

# transform to WGS884 reference system 
zipcodes<-spTransform(zipcodes, CRS("+init=epsg:4326"))

# Find the edges of our map
bounds<-bbox(zipcodes)

# input = list(dataYear=2013,measure='Median')
# input$dataYear
# input$measure

# Generate the score by zip data
data <- read.csv("Data/restaurant1.csv", na.strings = '')
data$INSPECTION.DATE <- as.Date(data$INSPECTION.DATE, format="%m/%d/%y")
data <- data[which(data$INSPECTION.DATE != '2000-01-01'),]
data$YEAR <- as.numeric(format(data$INSPECTION.DATE, "%Y")) 
data_score <- data[which(!is.na(data$SCORE)),]
score_zip <- data_score %>% select(ZIPCODE,YEAR,SCORE) %>%
  group_by(ZIPCODE,YEAR) %>%
  dplyr::summarise(Mean = mean(SCORE), Median = median(SCORE))
score_zip <- score_zip[score_zip$YEAR >= 2013,]


function(input, output, session){
  
  getDataSet<-reactive({
    # Get a subset of the income data which is contingent on the input variables
    dataSet<-score_zip[score_zip$YEAR == input$dataYear,c('ZIPCODE','YEAR',input$measure)]
    View(dataSet)
    # Copy our GIS data
    joinedDataset<-zipcodes
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(unique(merge(dataSet,joinedDataset@data,by="ZIPCODE")))
    View(joinedDataset@data)
    joinedDataset
  })
  
  # Due to use of leafletProxy below, this should only be called once
  output$zipcodeMap<-renderLeaflet({
    
      leaflet() %>%
      addTiles() %>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=10 # set to 10 as 9 is a bit too zoomed out
      )       
    
  })
  
  
  
  observe({
    theData<-getDataSet() 
    #theData<-joinedDataset
    View(theData)
    # colour palette mapped to data
    measure <- input$measure

    pal <- colorQuantile("YlGn", c(0,48.5), n = 30, na.color = "#808080") 
    help("colorQuantile")
    # set text for the clickable popup labels
    zipcode_popup <- paste0("<strong>Zipcode: </strong>", 
                            theData@data[,'ZIPCODE'], 
                            "<br><strong>",
                            input$measure,
                            " score: </strong>", 
                            formatC(theData@data[,measure], format="d", big.mark=',')
                            )
    print(theData@data[,measure])
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("zipcodeMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData@data[,measure]), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2,
                  popup = zipcode_popup)  
    
  })
 
  # table of results, rendered using data table
  output$zipcodeTable <- renderDataTable(datatable({
    dataSet<-getDataSet()
    measure <- input$measure
    dataSet<-dataSet@data[,c('ZIPCODE', measure)] # Just get name and value columns
    dataSet
    },
    options = list(lengthMenu = c(5, 10, 33), pageLength = 5))
  )
    
  # year selecter; values based on those present in the dataset
  output$yearSelect<-renderUI({
    yearRange<-sort(unique(as.numeric(score_zip$YEAR)), decreasing=TRUE)
    selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1])
  })
}