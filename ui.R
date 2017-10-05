
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
                                                   choiceValues = as.list(c("Q1","Q2","Q3","Q4")),
                                                   selected = c("Q1","Q2","Q3","Q4"), inline = TRUE),
                                
                                checkboxGroupInput(inputId = "selected_access",
                                                   label = "Which access type(s)?",
                                                   choiceNames =  as.list(c("Conventional","OpenAccess")),
                                                   choiceValues = as.list(c("Conventional","OpenAccess")), selected = c("Conventional","OpenAccess"), inline = TRUE) , 
                                
                                checkboxGroupInput(inputId = "selected_region",
                                                   label = "Which region(s)?",
                                                   choiceNames = as.list(c("United States","United Kingdom","Europe","Rest of the World")),
                                                   choiceValues = as.list(c("United States","United Kingdom","Europe","Rest of the World")), selected = c("United States","United Kingdom","Europe","Rest of the World")) ,
                                
                                checkboxGroupInput(inputId = "selected_variable",
                                                   label = "Which Variable(s) for dimension reduction Method?", 
                                                   choiceNames = as.list(c("SJR","H index","Total Docs. (2016)", "Total Docs. (3years)","Total Refs.","Total Cites (3years)","Citable Docs. (3years)","Cites / Doc. (2years)","Ref. / Doc." )),
                                                   choiceValues = as.list(c("SJR","H index","Total Docs. (2016)", "Total Docs. (3years)","Total Refs.","Total Cites (3years)","Citable Docs. (3years)","Cites / Doc. (2years)","Ref. / Doc." )),
                                                   selected = as.list(c("SJR","H index","Total Docs. (2016)", "Total Docs. (3years)","Total Refs.","Total Cites (3years)","Citable Docs. (3years)","Cites / Doc. (2years)","Ref. / Doc." ))
                                                   ),
                                tags$hr(),
                                
                                radioButtons("dim_reduct_method",label = "Dimension Reduction Method", choices = list("PCA","MDS")),
                                
                                selectInput(inputId = "point_shape",
                                            label = "Point Shape reflects:",
                                            choices = c("None","open.access","region","SJR Quartile")
                                                ),
                                
                                
                                sliderInput("biplot_vector_size", label = "Biplot Vector Size", min = 1 , max = 10 , value = 2),
                                sliderInput("point_size", label = "Point Size", min = 1 , max = 10 , value = 2),
                                
                                sliderInput("point_alpha", label = "Point Opacity", min = 0.1 , max = 1 , value = 0.5),
                                
                                
                                numericInput("num_of_inputs", "Number of Input factors", 1), 
                                
                                
                                
                                actionButton("plot_button","Plot") 
                                
                                
                                
                                
                        ),
                        
                        
                        mainPanel(
                                
                                tabsetPanel(
                                        tabPanel("Data View",
                                                 
                                                 plotOutput("sciMag_plot")
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