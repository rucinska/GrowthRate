library(shinyFiles)
library(shiny)
library(readxl)
library(XLConnect)
library(dplyr)

library(tidyr)
library(ggplot2)
library(data.table)
library(outliers)
library(gtools)
ui <- fluidPage(
  shinyDirButton("Btn_GetFolder", "Choose a folder" ,
                 title = "Please select a folder:",
                 buttonType = "default", class = NULL),
  
  DT::dataTableOutput("contents")
  #textOutput("txt_file")
)


server <- function(input,output,session){
  
  observe({
    
    shinyDirChoose(input, "Btn_GetFolder", roots = c(home = '~'), filetypes='xlsx' ,session = 
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
        
        output$contents <- DT::renderDataTable(all_plates)
        #output$txt_file <- renderText(listBands)
      })
    }
  })
}

shinyApp(ui = ui, server = server)