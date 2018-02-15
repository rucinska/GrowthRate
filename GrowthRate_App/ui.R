#### GROWTH RATE APP #####
library(shinyFiles)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(readxl)


inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

shinyUI(fluidPage(
  
  dashboardPage(skin = "black",
    #HEADER
    dashboardHeader(title = "Calculate Growth Rate"),
    #SLIDER
    dashboardSidebar(
      sidebarMenu(
        
        id = "tabs",
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Plot", tabName = "plot", icon = icon("bar-chart-o")),
        menuItem("Export", tabName = "export", icon = icon("export", lib = "glyphicon"))
      )
    ),
    #BODY
    dashboardBody(
      
      tags$style(HTML("

                      
                      .box.box-solid.box-primary>.box-header {
                      color:#fff;
                      background:#666666
                      }
                      
                      .box.box-solid.box-primary{
                      border-bottom-color:#666666;
                      border-left-color:#666666;
                      border-right-color:#666666;
                      border-top-color:#666666;
                      }
                      
                      ")),
      
      
      tabItems(
        tabItem(  
          tabName = "data",    
          fluidRow( 
            box(width = 5, title = "Upload Data",
                shinyjs::useShinyjs(),
                div(
                  id = "side-panel",
                  shinyDirButton("Btn_GetFolder", "Choose a folder" ,
                                 title = "Please select a folder:",
                                 buttonType = "default", class = NULL),
                  hr(),
                  checkboxInput("removewater", "Remove water", value = FALSE),
                 
                  box(width = 12,title = "Assign columns",
                      
                      
                      column(
                        width = 3,
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
                        div(style = "white-space: nowrap;",

                            h5(textInput("name5", label = "Sample 5 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                            h5(selectInput(inputId = "sam5", label = "Sample 5",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                            h5(selectInput(inputId = "bla5", label = "Blank 5",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                        ),
                        div(style = "white-space: nowrap;",
                            
                            h5(textInput("name6", label = "Sample 6 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                            h5(selectInput(inputId = "sam6", label = "Sample 6",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                            h5(selectInput(inputId = "bla6", label = "Blank 6",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                        ),
                        div(style = "white-space: nowrap;",
                          
                            h5(textInput("name7", label = "Sample 7 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                            h5(selectInput(inputId = "sam7", label = "Sample 7",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                            h5(selectInput(inputId = "bla7", label = "Blank 7",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                        ),
                        div(style = "white-space: nowrap;",

                            h5(textInput("name8", label = "Sample 8 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                            h5(selectInput(inputId = "sam8", label = "Sample 8",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                            h5(selectInput(inputId = "bla8", label = "Blank 8",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                        ),
                        div(style = "white-space: nowrap;",

                            h5(textInput("name9", label = "Sample 9 Name", value = "Enter name..."),style="display: inline-block; width: 100%;"),
                            h5(selectInput(inputId = "sam9", label = "Sample 9",c(),  multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;"),
                            h5(selectInput(inputId = "bla9", label = "Blank 9",c(), multiple = FALSE, selectize = TRUE),style="display:inline-block; width: 100%;")
                        )
                        
                      )#end column
                  ), #end box
                  
                      # column(2, 
                      #        selectInput(inputId = "sam1", label = "Sample 1",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam2", label = "Sample 2",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam3", label = "Sample 3",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam4", label = "Sample 4",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam5", label = "Sample 5",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam6", label = "Sample 6",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam7", label = "Sample 7",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam8", label = "Sample 8",c(),  multiple = FALSE, selectize = TRUE),
                      #        selectInput(inputId = "sam9", label = "Sample 9",c(),  multiple = FALSE, selectize = TRUE)
                      #        
                      #       
                      # ),
                    #   column(2,
                    #         selectInput(inputId = "bla1", label = "Blank 1",c(), 
                    #                      multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla2", label = "Blank 2",c(), 
                    #                      multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla3", label = "Blank 3",c(), 
                    #                      multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla4", label = "Blank 4",c(), 
                    #                      multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla5", label = "Blank 5",c(), 
                    #                      multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla6", label = "Blank 6",c(), 
                    #                     multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla7", label = "Blank 7",c(), 
                    #                     multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla8", label = "Blank 8",c(), 
                    #                     multiple = FALSE, selectize = TRUE),
                    #         selectInput(inputId = "bla9", label = "Blank 9",c(), 
                    #                     multiple = FALSE, selectize = TRUE)
                    # 
                    # ),
                    # column(3,
                    #       textInput("text1", "Sample1 Name", "" ),
                    #       textInput("text2", label ="Sample2 Name"),
                    #       textInput("text3", label ="Sample3 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text4", label ="Sample4 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text5", label ="Sample5 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text6", label ="Sample6 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text7", label ="Sample7 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text8", label ="Sample8 Name", 
                    #           value = "Enter name..."),
                    #       textInput("text9", label ="Sample9 Name", 
                    #           value = "Enter name...")
                    #        
                    # )
                    #),
                  textInput("text1", "Sample1 Name", "" ),
                  checkboxInput("mean_rep", "Take average for replicates", value = FALSE),
                  actionButton("update", "Update", class = "btn-primary",style='padding:4px; font-size:120%'),
                  actionButton("reset", "Reset", class = "btn-primary",style='padding:4px; font-size:120%')
                
                
                )
            ),
            box(width = 7, title = "Data", solidHeader = TRUE,
                column( width = 7,
                box(width = 7,title = "Data orginal",
                  div(style = "white-space: nowrap;",  
                        DT::dataTableOutput("contents"))
                )
                ),
                br(),
                br(),
                column( width = 7,
                box(width = 7,title = "Data modified",
                    div(style = "white-space: nowrap;", 
                    DT::dataTableOutput("contents1"),
                    downloadButton("downloadData", "Download")))
                )
            )
            
                    ) # end FluidRow
                
        ),
        
        tabItem( tabName = "plot",
                 
                 fluidRow(
                   
                   # box(width = 3, radioButtons("plot_type", "Select Plot Type", c("Lipid Abundance", "Box Plot","PCA","Standard Deviation", "Line Plot"), selected = "Lipid Abundance"),
                   #     hr(),
                   #     #conditionalPanel(
                   #     # condition = "input.plot_type == 'Standard Deviation'", numericInput("obs", "Set y axes:", 7)),
                   #     conditionalPanel(
                   #       condition = "input.plot_type == 'Lipid Abundance'", checkboxInput("hg_lipabu", "Seperate by Head Group Class", FALSE)),
                   #     conditionalPanel(
                   #       condition = "input.plot_type == 'Line Plot'", checkboxInput("hg", "Seperate by Head Group Class", FALSE)),
                   #     conditionalPanel(
                   #       condition = "input.plot_type == 'Box Plot'", checkboxInput("hg_bx", "Seperate by Head Group Class", FALSE)),
                   #     conditionalPanel(
                   #       condition = "input.plot_type == 'PCA'", checkboxInput("pca_names", "Hide names", FALSE)),
                   #     textInput('xlab', 'X axis label', value = "Lipid Species"),
                   #     textInput('ylab', 'Y axis label', value = "Abundance [mol %]"),
                   #     textInput('plotTitle', 'Plot title', value = ""),
                   #     textInput('Legend', 'Legend', value = "Lipid Abundance"),
                   #     selectInput('legendposition', label ='Legend Position',
                   #                 choices=c("left", "right", "bottom", "top"),
                   #                 multiple=FALSE, selectize=TRUE,selected="bottom"),
                   #     actionButton("update_plot", "Plot", class = "btn-primary",style='padding:4px; font-size:120%')),
                   box(
                     #h1("Experimental conditions:", textOutput("selected_var")),
                     textOutput("selected_var"),
                     textOutput("selected_feat"),
                     
                     width = 8, title = "Plot", solidHeader = TRUE, column(
                       12,
                       plotOutput('plot', brush=brushOpts("plot_brush",resetOnNew=T)),
                       #verbatimTextOutput("brush_info"),
                       wellPanel(width=9,h4("Select points to calculate Growth Rate:"),DT::dataTableOutput("plot_brushed_points")),
                       div(
                         id = "save_plot_area",
                         inline_ui(
                           textInput("save_plot_name", NULL, "",
                                     placeholder = "Enter plot name to save")
                         ),
                         actionButton("save_plot_btn", "Save plot", icon = icon("star")),
                         shinyjs::hidden(
                           span(
                             id = "save_plot_checkmark",
                             icon("check")
                           )
                         )
                       )
                     )) #end box
                   
                 )
        ), #end tabItem
        tabItem( tabName = "export",
                 fluidRow(      conditionalPanel(
                   condition = "!output.saved_plots_exist",
                   h2("You do not have any saved plots to export")
                 ),
                 conditionalPanel(
                   condition = "output.saved_plots_exist",
                   fluidRow(
                     column(
                       4,
                       h2("Export Options"),
                       div(
                         id = "exporting_plots_options",
                         selectInput("export_file_type", "File type",
                                     c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png")),
                         conditionalPanel(
                           condition = "input.export_file_type == 'pdf'",
                           selectInput("export_pdf_orientation", "Page orientation",
                                       c("Portrait (8.5\" x 11\")" = "portrait",
                                         "Landscape (11\" x 8.5\")" = "landscape",
                                         "Custom dimensions" = "custom")
                           ),
                           conditionalPanel(
                             condition = "input.export_pdf_orientation == 'custom'",
                             numericInput("export_pdf_width", "Page width (inches)",
                                          value = 8.5, min = 1, max = 50, step = 0.5),
                             numericInput("export_pdf_height", "Page height (inches)",
                                          value = 11, min = 1, max = 50, step = 0.5)
                           )
                         ),
                         conditionalPanel(
                           condition = "input.export_file_type != 'pdf'",
                           numericInput("export_file_width", "Image width (pixels)",
                                        value = 480, min = 100, max = 2000),
                           numericInput("export_file_height", "Image height (pixels)",
                                        value = 480, min = 100, max = 2000)
                         ),
                         checkboxInput("export_multiple", "Multiple plots per page"),
                         conditionalPanel(
                           condition = "input.export_multiple",
                           selectInput("export_arrangement", NULL,
                                       c("Arrange plots by row" = "byrow",
                                         "Arrange plots by column" = "bycol")),
                           numericInput("export_nrow", "Rows per page",
                                        value = 1, min = 1, max = 20),
                           numericInput("export_ncol", "Columns per page",
                                        value = 1, min = 1, max = 20)
                           
                         ),
                         uiOutput("export_btn_ui")
                       )
                     ),
                     column(
                       8,
                       h2("Preview"),
                       strong("Remove plot"), br(),
                       inline_ui(uiOutput("plots_remove_ui")),
                       actionButton("remove_plot_btn", "Remove"),
                       uiOutput("plots_order_ui"),
                       div(
                         id = "preview_plots_options",
                         uiOutput("plots_select_page_ui"),
                         plotOutput("plot_preview", height = "auto")
                       )
                     )
                   )
                 )))
      )
    )
  )
))
