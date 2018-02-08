library(shiny)
library(dplyr)
server <-  function(input, output) {
  getData <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }else {
      # browser()
      numfiles = nrow(inFile) 
      kata_csv1 = list()
      
      
      for (i in 1:numfiles)
      {
        
        JSON_csv = read.csv(input$file1[[i, 'datapath']], header = TRUE)
        lastrow = nrow(JSON_csv)
        shift = function(x, n){
          c(x[-(seq(n))], rep(NA, n))
        }
        JSON_csv$companyID1 = shift(JSON_csv$companyID1, 1)
        kata_csv1[[i]] = JSON_csv[-lastrow, ]
        
      }
      # browser()
      do.call(rbind, kata_csv1)
    }
  })
  output$contents <- renderTable( 
    getData() 
  )
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) { 
      write.csv(getData(), file, row.names=FALSE)   
    })
}

#shinyApp(ui = ui, server = server)