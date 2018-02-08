ui <- fluidPage(
  fluidPage(
    titlePanel("MY CSV FILES MERGER"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file1",
                  "Choose CSV files from directory",
                  multiple = TRUE,
                  accept=c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
        downloadButton('downloadData', 'Download')
      ),
      mainPanel(
        tableOutput('contents')
      )
    )
  )
)
