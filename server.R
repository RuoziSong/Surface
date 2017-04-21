library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)


set.seed(100)
zipdata <- data


function(input, output, session) {
  
  ## Restaurant Locator panel ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -74.0059, lat = 40.7128, zoom = 12)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # generate interacitve map
  observeEvent(input$click,{
    tmpdata<- zipdata
    if (input$type != "Any"){
      tmpdata<-tmpdata[tmpdata[, "CUISINE.DESCRIPTION"] == input$type,]
    }
    if (input$directions != "Any"){
      tmpdata <- subset(tmpdata,is.element(tmpdata[,"BORO"],input$directions))
    }
    if (input$zipcode!=""){
      tmpdata<-tmpdata[tmpdata[, "ZIPCODE"] == input$zipcode,]
    }
    if (input$restName!=""){
      tmpdata<-tmpdata[grepl(input$restName, tmpdata$DBA),]
    }
    if (nrow(tmpdata)==0){
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = -74.0059, lat = 40.7128, zoom = 12)
      })
    }
    else{
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles(
            urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
            attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
          ) %>%
          setView(lng = mean(tmpdata$ing), lat = mean(tmpdata$lat), zoom = 12)
      })
      zipsInBounds <- reactive({
        if (is.null(input$map_bounds))
          return(tmpdata[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(tmpdata,
               latitude >= latRng[1] & latitude <= latRng[2] &
                 longitude >= lngRng[1] & longitude <= lngRng[2])
      })
      leafletProxy("map", data = tmpdata) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addMarkers(~ing, ~lat,layerId=~CAMIS)
    }
  })
  
  # Show a popup at the given location
  showResPopup <- function(name, lat, lng) {
    selectedRes <- zipdata[zipdata$CAMIS == name,][1,]
    content <- as.character(tagList(
      tags$h4("Restaurant name: ", selectedRes$DBA),
      tags$strong(HTML(sprintf("%s %s, %s %s",
                               selectedRes$BUILDING, selectedRes$STREET, selectedRes$BORO, selectedRes$ZIPCODE
      ))), tags$br(),
      sprintf("Restaurant type: %s", selectedRes$CUISINE.DESCRIPTION), tags$br(),
      sprintf("Phone: %s",selectedRes$PHONE), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = name)
  }
  
  # When map is clicked, show historical inspection record for the corresponding restaurant
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    isolate({
      showResPopup(event$id, event$lat, event$lng)
      output$scorebyTime <- renderPlot({
        # If no zipcodes are in view, don't plot
        if (is.null(event$id))
          return(NULL)
        restByTime <- restByTimeData[restByTimeData$restID==event$id,]
        print(ggplot(restByTime, aes(x = time, y = score))+geom_line()+ggtitle("Historical inspection scores"))
      })
      restById = cleanData[cleanData$CAMIS == event$id & cleanData$CRITICAL.FLAG == "Critical", ]
      restScoreById = restByIdData[restByIdData$restId == event$id,]
      restType = unique(restById$CUISINE.DESCRIPTION)
      restZip = unique(restById$ZIPCODE)
      restByZipType = restByZipTypeData[restByZipTypeData$ZipCode==restZip & restByZipTypeData$Type==restType,]
      codeCount = restById%>%group_by(VIOLATION.CODE) %>% summarise(count=n())
      codeCount = codeCount[order(-codeCount$count),]
      if(dim(codeCount)[1] > 5)
        codeCount = codeCount[1:5,]
      numCode = dim(codeCount)[1]
      output$scorebyViolationCode <- renderPlot({
        rstByZipType_tmp = merge(codeCount, restByZipType, by.x = "VIOLATION.CODE", by.y = "Code", all.x = TRUE)
        rstByZipType = merge(rstByZipType_tmp, restScoreById,  by.x = "VIOLATION.CODE", by.y = "vioCode", all.x = TRUE)
        plotData <- rstByZipType[,-c(2,3,4,6)]
        colnames(plotData) <- c("VIOLATION.CODE", "Resturant Score", "Mean score in this zipcode area")
        plotData <- plotData %>% gather(key = ScoreType, value = ScoreMean, -VIOLATION.CODE)
        print(ggplot(plotData, aes(x = VIOLATION.CODE, y = ScoreMean, fill = ScoreType))+geom_bar(stat = "identity", position = "dodge")+ggtitle("Top 5 worst inspection scores and corresponding violations"))
      })
      strCode <- unique(substr(as.character(codeCount$VIOLATION.CODE),1,2))
      output$codeIcon1 <- renderImage({
        width  <- session$clientData$output_codeIcon4_width
        height <- session$clientData$output_codeIcon4_height
        list(src = './images/blank.png',
             width = width,
             height = height)
      }, deleteFile = FALSE)
      output$codeIcon2 <- renderImage({
        width  <- session$clientData$output_codeIcon4_width
        height <- session$clientData$output_codeIcon4_height
        list(src = './images/blank.png',
             width = width,
             height = height)
      }, deleteFile = FALSE)
      output$codeIcon3 <- renderImage({
        width  <- session$clientData$output_codeIcon4_width
        height <- session$clientData$output_codeIcon4_height
        list(src = './images/blank.png',
             width = width,
             height = height)
      }, deleteFile = FALSE)
      output$codeIcon4 <- renderImage({
        width  <- session$clientData$output_codeIcon4_width
        height <- session$clientData$output_codeIcon4_height
        list(src = './images/blank.png',
             width = width,
             height = height)
      }, deleteFile = FALSE)
      output$codeIcon5 <- renderImage({
        width  <- session$clientData$output_codeIcon4_width
        height <- session$clientData$output_codeIcon4_height
        list(src = './images/blank.png',
             width = width,
             height = height)
      }, deleteFile = FALSE)
      if(numCode > 0){
        output$codeIcon1 <- renderImage({
          width  <- session$clientData$output_codeIcon1_width
          height <- session$clientData$output_codeIcon1_height
          filename1 <- normalizePath(file.path('./images',
                                              paste('image', strCode[1], '.png', sep='')))
          list(src = filename1,
               width = width,
               height = height)
        }, deleteFile = FALSE)
      }
      if(numCode > 1){
        output$codeIcon2 <- renderImage({
          width  <- session$clientData$output_codeIcon2_width
          height <- session$clientData$output_codeIcon2_height
          filename2 <- normalizePath(file.path('./images',
                                              paste('image', strCode[2], '.png', sep='')))
          list(src = filename2,
               width = width,
               height = height)
        }, deleteFile = FALSE)
      }
      if(numCode > 2){
        output$codeIcon3 <- renderImage({
          width  <- session$clientData$output_codeIcon3_width
          height <- session$clientData$output_codeIcon3_height
          filename3 <- normalizePath(file.path('./images',
                                              paste('image', strCode[3], '.png', sep='')))
          list(src = filename3,
               width = width,
               height = height)
        }, deleteFile = FALSE)
      }
      if(numCode > 3){
        output$codeIcon4 <- renderImage({
          width  <- session$clientData$output_codeIcon4_width
          height <- session$clientData$output_codeIcon4_height
          filename4 <- normalizePath(file.path('./images',
                                              paste('image', strCode[4], '.png', sep='')))
          list(src = filename4,
               width = width,
               height = height)
        }, deleteFile = FALSE)
      }
      if(numCode > 4){
        output$codeIcon5 <- renderImage({
          width  <- session$clientData$output_codeIcon5_width
          height <- session$clientData$output_codeIcon5_height
          filename5 <- normalizePath(file.path('./images',
                                              paste('image', strCode[5], '.png', sep='')))
          list(src = filename5,
               width = width,
               height = height)
        }, deleteFile = FALSE)
      }
    })
  })
  
  
  ## Analysis - Inspection score by zipcode ###########################################
  getDataSet<-reactive({
    # Get a subset of the data which is contingent on the input variables
    dataSet<-score_zip[score_zip$YEAR == input$dataYear & score_zip$VIOLATION.TYPE == input$violation,c('ZIPCODE','YEAR',input$measure)]
   
    # Copy GIS data
    joinedDataset<-zipcodes
    
    # Join the two datasets together
    joinedDataset@data <- suppressWarnings(unique(merge(dataSet,joinedDataset@data,by="ZIPCODE")))
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
      ) %>%
      addLegend("bottomright",colors=c(pal(0),pal(7),pal(14),pal(21),pal(27),pal(50),pal(maxcolor)), 
                labels = c('0','|        Grade A','14','|        Grade B','27','|        Grade C', maxcolor),                     
                title = "Inspection score",
                opacity = 1
      )
    
  })
  
  
  observe({
    theData<-getDataSet() 
    
    # colour palette mapped to data
    measure <- input$measure
    # set text for the clickable popup labels
    # zipcode_popup <- paste0("<strong>Zipcode: </strong>", 
    #                         theData@data[,'ZIPCODE'], 
    #                         "<br><strong>",
    #                         input$measure,
    #                         " score: </strong>", 
    #                         formatC(theData@data[,measure], format="d", big.mark=',')
    # )
    # If the data changes, the polygons are cleared and redrawn, however, the map (above) is not redrawn
    leafletProxy("zipcodeMap", data = theData) %>%
      clearShapes() %>%
      addPolygons(data = theData,
                  fillColor = pal(theData@data[,measure]), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 2)
                  #popup = zipcode_popup)
  })
  
  # table of results, rendered using data table
  output$zipcodeTable <- DT::renderDataTable(datatable({
    dataSet<-getDataSet()
    measure <- input$measure
    dataSet<-dataSet@data[,c('ZIPCODE', measure)] # Just get name and value columns
    dataSet
  },
  options = list(lengthMenu = c(5, 10, 30), pageLength = 5))
  )
  
  # year selecter; values based on those present in the dataset
  output$yearSelect<-renderUI({
    yearRange<-sort(unique(as.numeric(score_zip$YEAR)), decreasing=TRUE)
    selectInput("dataYear", "Year", choices=yearRange, selected=yearRange[1])
  })
  output$violationType<-renderUI({
    typeRange<-unique(score_zip$VIOLATION.TYPE)
    radioButtons("violation", "Violation type",choices=typeRange,selected=typeRange[1])
  })
  
  
  ### Analysis - others #########################
  observeEvent(input$click2,{
    output$histCentile <- renderPlot({
      if (input$analysis_x == "CUISINE.DESCRIPTION"){
        ggplot(data[which(!is.na(data$CUISINE.DESCRIPTION)),], aes(x = factor(CUISINE.DESCRIPTION, levels = names(sort(table(CUISINE.DESCRIPTION), decreasing = TRUE)))))+ geom_bar(stat='count')+labs(x="Restaurant Type",y="number of inspection",title="Restaurant Type Distribution")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      else if (input$analysis_x == "VIOLATION.CODE"){
        ggplot(data[which(!is.na(data$VIOLATION.CODE)),], aes(x = factor(VIOLATION.CODE, levels = names(sort(table(VIOLATION.CODE), decreasing = TRUE)))))+ geom_bar(stat='count')+labs(x="Violation Type",y="number of inspection",title="Violation Type Distribution")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      else if (input$analysis_x =="INSPECTION.DATE"){
        ggplot(data[which(!is.na(data$INSPECTION.DATE)),],aes(INSPECTION.DATE))+geom_histogram(binwidth = 30)+labs(x="Inspection Date",y="number of inspection",title="Number of inspections over time")
      }
      else{
        return(NULL)
      }
    })
    output$score<- renderPlot({
      if (input$analysis_x == "CUISINE.DESCRIPTION"){
        ggplot(aggregate(SCORE~CUISINE.DESCRIPTION, data[which(!is.na(data$SCORE)),], mean),aes(x=factor(CUISINE.DESCRIPTION, levels = CUISINE.DESCRIPTION[order(SCORE)]),y=SCORE))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="Restaurant Type",y="Mean of Score",title="Mean Score of Restaurant Type")
      }
      else if (input$analysis_x == "VIOLATION.CODE"){
        ggplot(aggregate(SCORE~VIOLATION.CODE, data[which(!is.na(data$SCORE)),], mean),aes(x=factor(VIOLATION.CODE, levels = VIOLATION.CODE[order(SCORE)]),y=SCORE))+geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="Violation Type",y="Mean of Score",title="Mean Score of Violation Type")
      }
      else{
        return(NULL)
      }
    })
    output$most_v<- renderPlot({
      if (input$analysis_x == "CUISINE.DESCRIPTION"){
        ggplot(most_vio_rt, aes(x = factor(CUISINE.DESCRIPTION, levels = CUISINE.DESCRIPTION[order(-n)]),y=n))+ geom_bar(stat='identity') +labs(x="Restaurant Type",y="Number of Violation Type",title="The Most Common Violation in Each Restaurant Type")+geom_text(aes(label=VIOLATION.CODE),size=2.5,angle = 90,hjust=-0.25)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      else{
        return(NULL)
      }
    })
    
  })

  
}
