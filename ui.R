#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#
# This section is built up to make an user interface for Volvo's LTVF application



library(shiny) # Package for making our app
library(reactlog)
library(leaflet) #...adding an interactive map in our tool
library(shinydashboard) #...make a 
library(shinydashboardPlus)
library(rnaturalearth)   # geographic data
library(rnaturalearthdata)
library(sf) 
library(shinyWidgets)
library(DT)
library(forecast)
library(gt)
library(shinyalert)
library(highcharter)
library(RSQLite)
library(shinyjs)
library(dplyr)
library(sf)
library(r2d3) # To make D3.js visualization








options(shiny.reactlog = TRUE) # Check React graph







#-----------------------------------------------------------------------------------------------------------------------------------------------------------------



ResultPanel <- tabsetPanel(id = "results",
                           type = "tabs",
                           
                              tabPanel("Forecast results",
                                       fluidRow(
                                         box(
                                           title = "Summary",
                                           width = 12,
                                           uiOutput("summaryBox"),
                                           htmlOutput("drivers_list"))
                                       ),
                                       
                                       box(title = 'Drivers and sales curve', highchartOutput('Curves', height = '470px'), width=12),
                                       box(title = 'Map', leafletOutput('Map1', height = 470, width = "100%"), highchartOutput("Gbackcast", height = '470px'),
                                           tableOutput('Metrics'), width=6),
                                       box(title = "Forecast results", highchartOutput("Gforecast"), DTOutput("Tforecast"),
                                                     downloadButton("download", "Export csv"),width = 6)
                                       
                              ),
                           
                              tabPanel("Principal Component Analysis",
                                       box(title = "Correlation matrix", highchartOutput("Correlation", height = '700px'), width = 12),
                                       box(title = "PCA graph", plotOutput("PCAres"), width = 6),
                                       box(title = "Variable contribution", plotOutput("Contrib"), width = 6)
                              ),
                           
                              tabPanel("Model validation",
                                    box(title = "Residuals", plotOutput("residuals"), width = 12),
                                    box(title = "Estimate", verbatimTextOutput("Estimate"), width = 6),
                                    box(title = "Test", verbatimTextOutput("Test"), width = 6)
                              ),
                           
                              selected = "Forecast results"
                              
               )



GlobalViewPanel <- tabsetPanel(id = "results1",
                               type = "tabs",
                               
                               tabPanel("Global forecast",
                                  fluidRow(
                                    
                                    column(2,
                                       prettyRadioButtons('Options', label='Options', choices = c('By country','By driver')),
                                    ),
                                    
                                    column(3,
                                       pickerInput(
                                         inputId = 'Area1',
                                         label = 'country',  # label dans le menuItem
                                         choices = unique(table %>% pull(COUNTRY)),
                                         multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           `live-search` = TRUE)
                                       )
                                    ),
                                      
                                    column(3, 
                                       pickerInput(
                                         inputId = 'Drivers1',
                                         label = 'Drivers',
                                         choices = NULL,
                                         choicesOpt = NULL,
                                         multiple = TRUE,
                                         options = list(
                                           `actions-box` = TRUE,
                                           `live-search` = TRUE)
                                       )
                                    ),
                                    
                                    column(2,
                                      pickerInput(
                                        inputId = 'Category1',
                                        label = "Truck category",
                                        choices = unique(table %>% pull(SEGMENT)),
                                        multiple = FALSE,
                                        options = list(
                                          `actions-box` = FALSE,
                                          `live-search` = TRUE)
                                        )
                                    ),
                                    
                                    column(2,
                                         actionBttn(
                                           icon = icon("forward"),
                                           inputId = "switch2",
                                           label = "Simulate",
                                           style = 'jelly',      # autres styles: borderless, gradient, simple
                                           color = "success", # primary, warning, danger, etc.
                                           size ='lg'
                                         )
                                         
                                    ),
                                           
                                  ),
                                  
                                  conditionalPanel(
                                      condition  = "input.Options == 'By country'",
                                      fluidRow(column(12, uiOutput('Bycountry')))
                                      
                                  ),
                                  
                                  conditionalPanel(
                                    condition  = "input.Options == 'By driver'", 
                                    fluidRow(column(12,  uiOutput('Bydriver')))
                                  )
                               
                                ),
                               
                               tabPanel("Residual analysis",
                                   
                                    conditionalPanel(
                                      condition  = "input.Options == 'By driver'", 
                                      fluidRow(column(12,  uiOutput('Bydriver_residual')))
                                    )
                                        
                                        
                                        
                               )
                               
                            )







ui <- shinyUI(dashboardPage(
  
    
     

    dashboardHeader(title = "LTVF"),
  
  

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------  



    dashboardSidebar(
          
   
        
        sidebarMenu(
            menuItem("Area", icon = icon("globe"),
                     pickerInput(
                       inputId = 'Area',
                       label = NULL,  # label dans le menuItem
                       choices = unique(table %>% pull(COUNTRY)),
                       multiple = FALSE,
                       options = list(`live-search` = TRUE),
                       width = "100%"
                     )
            ),
            
            menuItem("Truck category", icon = icon("truck"),
                    pickerInput(
                      inputId = 'Category',
                      label = NULL,
                      choices = unique(table %>% pull(SEGMENT)),
                      multiple = FALSE,
                      options = list(
                        `actions-box` = FALSE,
                        `live-search` = TRUE
                                )
                    )
            ),
        

            
            menuItem("Choose drivers", icon = icon("chart-line"),
                    pickerInput(
                      inputId = 'Drivers',
                      label = NULL,
                      choices = NULL,
                      choicesOpt = NULL,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `live-search` = TRUE
                                )
                    )
            ),
            
            
            
            menuItem("Forecast", icon = icon("play"),
                    actionBttn(
                      inputId = "switch",
                      label = "Go",
                      style = 'jelly',      # autres styles: borderless, gradient, simple
                      color = "success",    # primary, warning, danger, etc.
                      
                    )
            
            ),
            
            
            
            menuItem("Simulation", icon = icon("forward"),
                     actionBttn(
                       inputId = "switch1",
                       label = "Go",
                       style = 'jelly',      # autres styles: borderless, gradient, simple
                       color = "royal",    # primary, warning, danger, etc.
                       
                     )
                     
            )
        
        )
        
      
    ), 


##----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


       dashboardBody(
         

          
          fluidRow( column(5),
                    column(2,
                    imageOutput('Logo', width ="300" , height ="30px")
                          ),
                    column(5)
          ),
          
          conditionalPanel(
            
              condition = "input.switch % 2 == 0 && input.switch1 % 2 == 0",
              leafletOutput('Map', height = 1000, width = "100%")
          ),
          
          
          conditionalPanel(
            
            condition = "input.switch % 2 == 1 && input.switch1 % 2 == 0",
            ResultPanel
            
            
          ),
          
          conditionalPanel(
            
            condition = "input.switch1 % 2 == 1",
            GlobalViewPanel
            
            
          )
          
       )
        
  ) 

)
      
                         













