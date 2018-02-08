library(shinyFiles)
library(shiny)
library(shinydashboard)
library(datasets)
library(DT)
library(tidyr)
library(dplyr)
library(shinyjs)
library(ggrepel)
library(ggbiplot)
library(stringr)
library(readr)
#runExample("test.csv")

shinyServer(function(input, output, session) {
  values <- reactiveValues(
    plots = list(),
    maindata = NULL     # the data frame used throughout the app
  ) 
  
  #https://stackoverflow.com/questions/48339128/create-list-of-file-paths-in-r-shiny-without-uploading-files
  volumes = getVolumes()
  observe({
    
    shinyDirChoose(input, "Btn_GetFolder", roots = c(home = '~'),filetypes='xlsx', session = 
                     session)
    if(!is.null(input$Btn_GetFolder)){
      # browser()
      myInputDir1 <- parseDirPath(c(home = '~'), input$Btn_GetFolder)
      listBands <- list.files(myInputDir1, full.names = T)
      
      
      dat = lapply(listBands, function(i){
        data = read_excel(i, range = anchored("A43", dim = c(9, 13)))
        
        date <- readxl::read_excel(i, range = "E40", col_names ="date")
        plate <- cbind(data, date$date)
        all_plates = do.call("rbind.data.frame", dat) 
        col_names <- c("Layout", "1","2","3","4", "5","6","7", "8","9","10","11","12", "StartTime")
        
        names(all_plates) <- col_names
        output$contents <- DT::renderDataTable(all_plates)
      })
    }
  })
  
  
})  