
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(
        fluidPage(
                
                titlePanel("Upload Facotrs Data"),
                
                sidebarLayout(
                        sidebarPanel(
                                helpText("Note:This is an interactive visualization applet on SciMag data"), 
                                
                                
                                tags$hr(),
                                helpText("Data Filtering"),
                                # Checkbox with several selection for filtering the data
                                checkboxGroupInput(inputId = "selected_quartile",
                                                   label = "Which SJR Quartile(s)?",
                                                   choiceNames = as.list(c("Q1","Q2","Q3","Q4")), 
                                                   choiceValues = as.list(c(1,2,3,4)),
                                                   selected = c(1,2,3,4 )),
                                
                                checkboxGroupInput(inputId = "selected_access",
                                                   label = "Which access type(s)?",
                                                   choiceNames =  as.list(c("Conventional","OpenAccess")),
                                                   choiceValues = as.list(c(1,2)), selected = c(1,2)) , 
                                
                                checkboxGroupInput(inputId = "selected_region",
                                                   label = "Which region(s)?",
                                                   choiceNames = as.list(c("United States","United Kingdom","Europe","Rest of the World")),
                                                   choiceValues = as.list(c(1,4,2,3)), selected = c(1,2,3,4)),
                                
                                numericInput("num_of_inputs", "Number of Input factors", 1), 
                                
                                checkboxInput('header', 'Header', TRUE),
                                checkboxInput('dmu_labels', 'DMU Labels', FALSE),
                                
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                
                                radioButtons('dec', 'Decimal Symbol',
                                             c(Comma=',',
                                               Dot='.'),
                                             '.'),
                                
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"'),
                                tags$hr(),
                                
                                
                                
                                helpText("Note: When the data shown in the right panel is what ",
                                         "it is supposed to be, each variable in one column and",
                                         "inputs are separated correctly from outputs,",
                                         "then press the submit button."),
                                
                                actionButton("submit_button","Submit") 
                                
                                
                                
                                
                        ),
                        
                        
                        mainPanel(
                                
                                tabsetPanel(
                                        tabPanel("Data View",
                                                 "plot"
                                        ),
                                        tabPanel("Correlations", 
                                                 "corr"),
                                        tabPanel("Data View",
                                                 tableOutput("table")),
                                        tabPanel("Help",
                                                 "kos help"
                                                 
                                        )
                                )
                                
                                
                        ) # End mainPanel
                ) # End sidebar layout
        ) # end fluidPage
) # end shiny