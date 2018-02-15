library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 3,
          div(style = "white-space: nowrap;", 
              div(style = "white-space: nowrap;", 
                  
                  h5(textInput("name1", label = "Sample 1 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                  h5(selectInput(inputId = "sam1", label = "Sample 1",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                  h5(selectInput(inputId = "bla1", label = "Blank 1",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
              ),
              div(style = "white-space: nowrap;",
                  
                  h5(textInput("name2", label = "Sample 2 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                  h5(selectInput(inputId = "sam2", label = "Sample 2",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                  h5(selectInput(inputId = "bla2", label = "Blank 2",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
              ),
              div(style = "white-space: nowrap;",
                  
                  h5(textInput("name3", label = "Sample 3 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                  h5(selectInput(inputId = "sam3", label = "Sample 3",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                  h5(selectInput(inputId = "bla3", label = "Blank 3",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
              ),
              div(style = "white-space: nowrap;",
                  
                  
                  h5(textInput("name4", label = "Sample 4 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                  h5(selectInput(inputId = "sam4", label = "Sample 4",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                  h5(selectInput(inputId = "bla4", label = "Blank 4",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
              ),
              actionButton("update", "Update", class = "btn-primary",style='padding:4px; font-size:120%')
          )))),
    mainPanel(
      DT::dataTableOutput("contents"),
      br(),
      br(),
      DT::dataTableOutput("contents1"),
      plotOutput("plot_preview", height = "auto")
    )))

server <- function(input, output, session) {
  
  Layout <- c("A", " B", " A", "B")
  
  col1 <- c(0.84, 0.65, 0.97, 0.81)
  col2 <- c(0.43,0.55,0.53,0.66)
  col3 <- c(0.74, 0.75, 0.87, 0.71)
  
  df <- data.frame(Layout, col1, col2, col3) 
  
  cols <- colnames(df) 
  cols <- c("NULL", cols[2:4])

  for(i in 1:4) {
    updateSelectInput(session, paste0('sam',i), choices=cols)
    updateSelectInput(session, paste0('bla',i), choices=cols)
  }
  
  df1 <- data.frame()
  reval_df <- reactiveVal(df)
  
  ## take a colum choosed before and substract the blank - save as one column 
  observeEvent(input$update, {
    df1 <- reval_df()
    for (i in 1:4)
    {
      if(input[[paste0('sam',i)]]!='NULL' & input[[paste0('bla',i)]]!='NULL')
      {
        print(i)
        df1[[input[[paste0('name',i)]]]] = df[[input[[paste0('sam',i)]]]]- df[[input[[paste0('bla',i)]]]]
      }
    }
    
    reval_df(df1%>% select(-(starts_with("col"))))
   
    # reset inputs
    # lapply(1:4,function(x) {updateSelectInput(session,paste0('bla',x),selected='NULL')})
    # lapply(1:4,function(x) {updateSelectInput(session,paste0('name',x),selected='NULL')})
    # lapply(1:4,function(x) {updateSelectInput(session,paste0('sam',x),selected='NULL')})
    
    
  })
  
  output$contents <- DT::renderDataTable(df,rownames = FALSE)
  
  output$contents1 <- DT::renderDataTable(reval_df(), rownames = FALSE)
  
}

shinyApp(ui, server)