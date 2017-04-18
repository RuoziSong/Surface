library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- data[sample.int(nrow(data), 10000),]


function(input, output, session) {
  
  ## Interactive Map ###########################################
  
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
  
  # Precalculate the breaks we'll need for the two histograms
  # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  # 
  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   hist(zipsInBounds()$centile,
  #        breaks = centileBreaks,
  #        main = "SuperZIP score (visible zips)",
  #        xlab = "Percentile",
  #        xlim = range(allzips$centile),
  #        col = '#00DD00',
  #        border = 'white')
  # })
  # 
  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  #   
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  
  observeEvent(input$click,{
    tmpdata<- zipdata
    if (input$type != "Any"){
      tmpdata<-tmpdata[tmpdata[, "CUISINE.DESCRIPTION"] == input$type,]
    }
    if (!is.null(input$directions)){
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
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      # colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      # pal <- colorFactor("viridis", colorData)
      
      #colorData <- zipdata[[locationBy]]
      #pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      # 
      # 
      # if (sizeBy == "superzip") {
      #   # Radius is treated specially in the "superzip" case.
      #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
      # } else {
      #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
      # }
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
      #addCircles(~ing, ~lat, radius=100, layerId=~ZIPCODE,stroke=FALSE, fillOpacity=0.4)
      #            stroke=FALSE, fillOpacity=0.4) %>%
      # addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
      #           layerId="colorLegend")
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
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    isolate({
      showResPopup(event$id, event$lat, event$lng)
      
      #Show plot1
      output$scorebyTime <- renderPlot({
        if (is.null(event$id))
          return(NULL)
        restByTime <- restByTimeData[restByTimeData$restID==event$id,]
        print(ggplot(restByTime, aes(x = time, y = score))+geom_line())
      })
      #Show plot2
      output$scorebyViolationCode <- renderPlot({
        if (is.null(event$id))
          return(NULL)
        restById = cleanData[cleanData$CAMIS == event$id, ]
        restScoreById = restByIdData[restByIdData$restId == event$id,]
        restType = unique(restById$CUISINE.DESCRIPTION)
        restZip = unique(restById$ZIPCODE)
        restByZipType = restByZipTypeData[restByZipTypeData$ZipCode==restZip & restByZipTypeData$Type==restType,]
        codeCount = restById%>%group_by(VIOLATION.CODE) %>% summarise(count=n())
        codeCount = codeCount[order(-codeCount$count),]
        if(dim(codeCount)[1] > 5)
          codeCount = codeCount[1:5,]
        rstByZipType_tmp = merge(codeCount, restByZipType, by.x = "VIOLATION.CODE", by.y = "Code", all.x = TRUE)
        rstByZipType = merge(rstByZipType_tmp, restScoreById,  by.x = "VIOLATION.CODE", by.y = "vioCode", all.x = TRUE)
        plotData <- rstByZipType[,-c(2,3,4,6)]
        colnames(plotData) <- c("VIOLATION.CODE", "Resturant Score", "Zip-Type Score")
        plotData <- plotData %>% gather(key = ScoreType, value = ScoreMean, -VIOLATION.CODE)
        print(ggplot(plotData, aes(x = VIOLATION.CODE, y = ScoreMean, fill = ScoreType))+geom_bar(stat = "identity", position = "dodge"))
    })
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
