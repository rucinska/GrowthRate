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
    maindata = NULL,     # the data frame used throughout the app
    df1 = data.frame()
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
        col_names <- c("Layout", "col1","col2","col3","col4", "col5","col6","col7", "col8","col9","col10","col11","col12", "StartTime")
        names(all_plates) <- col_names
        time <- format(all_plates$StartTime, format="%H:%M:%S") 
        all_plates$StartTime <- strptime(time, format = "%H:%M:%S")
        all_plates$StartTime=all_plates$StartTime$sec/60 + all_plates$StartTime$min + all_plates$StartTime$hour*60
        print(all_plates)
    
        # remove water outlayer
        if(!input$removewater){
          all_plates
        }else{
          all_plates <- all_plates %>% 
            select(-col1, -col12) %>%
            filter(!Layout %in% c("A","H") )
        }
        #take a mean of each col, group by Layout
        if(!input$mean_rep){
          all_plates
        }else{
          all_plates <- all_plates %>% 
            group_by(Layout, StartTime) %>%
            summarise_at(vars(matches("col")),mean)# %>%
            #select(-Layout_mean)
        }
        cols <-colnames(all_plates) 
          #cols <- cols 
          cols <- c("NULL", cols[!cols %in% c("Layout","StartTime")])
          
          for(i in 1:9) {
            updateSelectInput(session, paste0('sam',i), choices=cols)
            updateSelectInput(session, paste0('bla',i), choices=cols)
          } 
        
          
       
        
       
          #df1 <- data.frame()
          reval_df <- reactiveVal(all_plates)
          
          ## take a colum choosed before and substract the blank - save as one column 
          observeEvent(input$update, {
            df <- reval_df()
            for (i in 1:9)
            {
              if(input[[paste0('sam',i)]]!='NULL' & input[[paste0('bla',i)]]!='NULL')
              {
                
                df[[input[[paste0('name',i)]]]] = df[[input[[paste0('sam',i)]]]]- df[[input[[paste0('bla',i)]]]]
              }
            }
            reval_df(df <- df%>% select(-(starts_with("col"))) )
           
            
          
          })
          
        
        
          output$contents1 <- DT::renderDataTable(reval_df(), 
                                                  rownames = FALSE,
                                                  options = list(pageLength = 8) )
          
    
          ####PLOT #### 
          
          Plot <- eventReactive({ 
            input$update
          },{
            
            
            df_mod <- reval_df() %>% gather(Sample, Value, -one_of("StartTime", "Layout")) 
            
            
            
              ggplot(df_mod,  aes(x = StartTime, y = Value, col = Layout)) +
              geom_point() +
              geom_smooth(method="glm",
                          method.args=list(family=gaussian(link="log")), 
                          se = F) 
            
          })
          
          output$plot <- renderPlot(Plot() ) 
          output$plot_brushed_points<-DT::renderDataTable({ 
            res<- brushedPoints(reval_df() %>% gather(Sample, Value, -one_of("StartTime", "Layout")) , input$plot_brush)
            #subset_res<-subset(res,select=c(HUC12,Ecological_Health,Ecological_Stress,Comprehensive_Score))
            datatable(res)
            

            })
          #http://www.cyclismo.org/tutorial/R/linearLeastSquares.html
          #https://stackoverflow.com/questions/33748649/subset-dataframe-based-on-zoom-interaction
          output$summary1 <- renderPrint({
            res<- brushedPoints(reval_df() %>% gather(Sample, Value, -one_of("StartTime", "Layout")) , input$plot_brush)
            fit <- lm(StartTime~Value, res)
            summary(fit)
          })
            })
    }
   
  })
  

  
})  