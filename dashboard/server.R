library(shiny)
#Load JSON data
data <- fromJSON("data.json")

#Data Preprocessing
data$current_page <- as.factor(data$current_page)
#Finding crashes
crashData <- subset(data,did_aww_snap ==TRUE)
cd <- data.frame(pages = c(levels(crashData$current_page)),Crashed = c(summary(crashData$current_page)), Total = c(summary(data$current_page)))

#indices just before crash
beforeCrash <- which(data$did_aww_snap)
beforeCrash <- beforeCrash-1

#summary of bytes used when page crashed
dataBeforeCrash <- data$bytes_used[beforeCrash]
#from mean and standard deviation we can see that
#around 66% of crashes were due to memory usage more than 176440486 bytes

#frequency of crashes
dataBackup <- data
#find start time
dataBackup$timestamp = dataBackup$timestamp - dataBackup$timestamp[1]
#find frequency of crashes by finding total hours since the start
freq <- length(which(dataBackup$did_aww_snap))/(dataBackup$timestamp[20000]/3600)
#26-27 crashes per hour

#avg bytes used
avgBytes <- ddply(data,~current_page,summarise,"mean in MB"=round(mean(bytes_used/1000000), digits = 1))
names(avgBytes) <- c('current_page' = "Current Page", 'mean in MB' = "Avg. Bytes")

#avg bytes used by each page is approx the same.

#to find crashes each hour
crashData$timestamp = crashData$timestamp - data$timestamp[1]
hourCrashDataT = data.frame(matrix(0, ncol = 7, nrow = 28))  
colnames(hourCrashDataT) <- c("Hour","rootCrash","analyticsCrash","dataCrash","playerCrash","processesCrash","processEditorCrash")
rootCount = 0
analyticsCount = 0
dataCount = 0
playerCount = 0
processCrash = 0
procEditorCrash = 0

rowNum = ceiling(crashData$timestamp[1]/3600)
for(i in 1:nrow(crashData)){
  if(ceiling(crashData$timestamp[i]/3600) == rowNum){
    if(crashData$current_page[i] == '/'){
      rootCount = rootCount + 1
    }
    else if(crashData$current_page[i] == '/analytics'){
      analyticsCount = analyticsCount + 1
    }
    else if(crashData$current_page[i] == '/data'){
      dataCount = dataCount + 1
    }
    else if(crashData$current_page[i] == '/player'){
      playerCount = playerCount + 1
    }
    else if(crashData$current_page[i] == '/processes'){
      processCrash = processCrash + 1
    }
    else if(crashData$current_page[i] == '/processes/editor'){
      procEditorCrash = procEditorCrash + 1
    }
  }
  else{
    #newrow = c(rowNum,rootCount, analyticsCount, dataCount, playerCount, processCrash, procEditorCrash, 0)
    #hourCrashDataT[rowNum] = data.frame(rowNum,rootCount, analyticsCount, dataCount, playerCount, processCrash, procEditorCrash, 0)
    hourCrashDataT$Hour[rowNum] <-rowNum
    hourCrashDataT$rootCrash[rowNum] <- rootCount
    hourCrashDataT$analyticsCrash[rowNum] <- analyticsCount
    hourCrashDataT$dataCrash[rowNum] <- dataCount
    hourCrashDataT$playerCrash[rowNum] <- playerCount
    hourCrashDataT$processesCrash[rowNum] <- processCrash
    hourCrashDataT$processEditorCrash[rowNum] <- procEditorCrash
    rowNum = ceiling(crashData$timestamp[i]/3600)
    rootCount = 0
    analyticsCount = 0
    dataCount = 0
    playerCount = 0
    processCrash = 0
    procEditorCrash = 0
    if(crashData$current_page[i] == '/'){
      rootCount = rootCount + 1
    }
    else if(crashData$current_page[i] == '/analytics'){
      analyticsCount = analyticsCount + 1
    }
    else if(crashData$current_page[i] == '/data'){
      dataCount = dataCount + 1
    }
    else if(crashData$current_page[i] == '/player'){
      playerCount = playerCount + 1
    }
    else if(crashData$current_page[i] == '/processes'){
      processCrash = processCrash + 1
    }
    else if(crashData$current_page[i] == '/processes/editor'){
      procEditorCrash = procEditorCrash + 1
    }
  }
}
hourCrashDataT$Hour[28] <-rowNum
hourCrashDataT$rootCrash[28] <- rootCount
hourCrashDataT$analyticsCrash[28] <- analyticsCount
hourCrashDataT$dataCrash[28] <- dataCount
hourCrashDataT$playerCrash[28] <- playerCount
hourCrashDataT$processesCrash[28] <- processCrash
hourCrashDataT$processEditorCrash[28] <- procEditorCrash


shinyServer(function(input, output) {

  output$crashPerHourText <- renderText({ 
    rowNumber <- input$hours
    paste("Crashes this hour were: ",sum(hourCrashDataT$rootCrash[rowNumber], hourCrashDataT$analyticsCrash[rowNumber], hourCrashDataT$dataCrash[rowNumber], hourCrashDataT$playerCrash[rowNumber], hourCrashDataT$processesCrash[rowNumber], hourCrashDataT$processEditorCrash[rowNumber]))
  })
  
  output$avgBytesPerHourText <- renderText({ 
    rowNumber <- input$hours
    paste("Average bytes used this hour were: ",rowNumber)
  })
  
  output$avgByteTable <- renderTable({
    avgBytes
    })
  
  output$crashPagesHour <- renderGvis({
    rowNumbers <- input$hours
    dataVis <- data.frame(pages = c(levels(crashData$current_page)),crashes = as.numeric(hourCrashDataT[rowNumbers,2:7]))
    gr <- gvisBarChart(dataVis, options = list(title="Crashes per Hour",animation.startup='true',hAxis="{title:'Page'}",vAxis="{title:'No of Crashes'}",orientation='horizontal',legend="none",fontSize="10"))
    return(gr)
  })
  
  output$crashPages <- renderGvis({
    pl <- gvisBarChart(cd,options = list(title="Crashes vs Total Pages",animation.startup='true',hAxis="{title:'No. of Pages'}",vAxis="{title:'Pages'}",legend="top"))
    return(pl)
  })
})