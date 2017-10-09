
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#------ dropdownbuton func
# https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
        
        status <- match.arg(status)
        # dropdown button content
        html_ul <- list(
                class = "dropdown-menu",
                style = if (!is.null(width)) 
                        paste0("width: ", validateCssUnit(width), ";"),
                lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
        )
        # dropdown button apparence
        html_button <- list(
                class = paste0("btn btn-", status," dropdown-toggle"),
                type = "button", 
                `data-toggle` = "dropdown"
        )
        html_button <- c(html_button, list(label))
        html_button <- c(html_button, list(tags$span(class = "caret")))
        # final result
        tags$div(
                class = "dropdown",
                do.call(tags$button, html_button),
                do.call(tags$ul, html_ul),
                tags$script(
                        "$('.dropdown-menu').click(function(e) {
                        e.stopPropagation();
});")
  )
        }

#-----------------
# for data preparation that may be used in selectInput
# for deploying on shinyapps.io
#suppressWarnings(sciMag.data <- read_csv(file = "/srv/connect/apps/SciMagOR2016/scimag.csv" ))
sciMag.data <- read_csv(file = "./data/scimag.csv" )
sciMag.data$X1 <- NULL
sciMag.data$Type <- NULL

sciMag.data$Country <- as.factor(sciMag.data$Country)
sciMag.data$region <- as.factor(sciMag.data$region)
levels(sciMag.data$region) <- c("United States","Europe","Rest of the World","United Kingdom")
sciMag.data$open.access <- as.factor(sciMag.data$open.access)
levels(sciMag.data$open.access) <- c("Conventional","OpenAccess")
sciMag.data$SJR <- as.numeric(gsub(pattern = ",",replacement = ".", x = sciMag.data$SJR ))
sciMag.data$`Cites / Doc. (2years)` <- as.numeric(gsub(pattern = ",",replacement = ".", x = sciMag.data$`Cites / Doc. (2years)` ))

# tidy data with categories 
a<- strsplit(x= sciMag.data$Categories, "; ")
names(a) <- sciMag.data$Title

df <- data.frame(Title = names(unlist(a)), Categories = matrix(unlist(a), byrow=T),
                 stringsAsFactors=FALSE)
df$Title <- gsub(x = df$Title, pattern = "\\d" , replacement = "" )
df$Categories <- gsub(x = df$Categories, pattern = "\\s\\(.\\d\\)" , replacement = "")

sciMag.data$Categories <- NULL 

sciMag.data<- merge(x = sciMag.data, y = df, by = "Title", all = TRUE)

sciMag.data$Categories <- factor(tidy_scimag$Categories)
sciMag.data$`SJR Quartile` <- factor(tidy_scimag$`SJR Quartile`)

#print(levels(sciMag.data$Categories))

#selection of default categories for selectInput()
default_cats<-tidy_scimag %>% filter(Title %in% c("Omega","European Journal of Operations Research", "Operations Research")) %>% select(Categories)
default_cats <- as.vector(default_cats$Categories)
print(default_cats)
#-----------------

shinyUI(
        fluidPage(
                
                titlePanel("SciMagViz Applet"),
                
                fluidRow(
                        column(2,wellPanel(
                                helpText("Data Filtering"),
                                # Checkbox with several selection for filtering the data
                                dropdownButton(
                                        label = "Select some Categories", status = "default", width = "100%",
                                        tags$div(style='overflow-y: scroll; height: 200px;',
                                                 checkboxGroupInput(inputId = "selected_categories",
                                                                    label = "Choose",
                                                                    choices = levels(sciMag.data$Categories),
                                                                    selected = default_cats))
                                        
                                ),
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
                             
                                actionButton("plot_button","Plot")
                                
                                
                        )
                        ),
                        
                        
                        column(8,wellPanel(
                                
                                tabsetPanel(
                                        tabPanel("Data View",
                                                 
                                                 plotOutput("sciMag_plot",width = "800px",height = "600px",
                                                            dblclick = "plot_dblclick",
                                                            brush = brushOpts(
                                                                    id = "plot_brush",
                                                                    resetOnNew = TRUE
                                                            )), 
                                                 downloadButton('download_plot', 'Download the Plot'),
                                                 tags$h3("PCA data table"),
                                                 DT::dataTableOutput("brush_info_pca", width = "800px"),
                                                 tags$h3("MDS data table"),
                                                 DT::dataTableOutput("brush_info", width = "800px")
                                        ),
                                        tabPanel("Correlations", 
                                                 plotOutput("correlation_plot", width = "800px",height = "600px")
                                                 ),
                                        tabPanel("Category-Frequency Plots",
                                                 plotOutput("category_variable_plot", width = "800px",height = "600px"),
                                                 downloadButton('download_category_plot', 'Download the Plot')
                                                 ),
                                        tabPanel("Dataset",
                                                 DT::dataTableOutput("table")
                                                 ),
                                        tabPanel("Help",
                                                 tags$iframe(src = "SciMagVizHelp.html", style="height:600px; width:100%")
                                                 
                                        )
                                )
                                
                                
                        )), # End mainPanel
                        column(2,
                               wellPanel(
                                       
                                       helpText("Map Manipulation"),
                                       radioButtons("dim_reduct_method",label = "Dimension Reduction Method", choices = list("PCA","MDS"),inline = TRUE),
                                       
                                       selectInput(inputId = "point_shape",
                                                   label = "Point Shape reflects:",
                                                   choices = c("None","open.access","region","SJR Quartile")
                                       ),
                                       uiOutput("color_variable_select"),
                                       
                                       radioButtons("journal_label",label = "Show the label of points?", choices = list("Yes","No"),selected = "No",inline = TRUE),
                                       
                                       sliderInput("biplot_vector_size", label = "Biplot Vector Size", min = 1 , max = 10 , value = 6),
                                       sliderInput("point_size", label = "Point Size", min = 1 , max = 10 , value = 7),
                                       
                                       sliderInput("point_alpha", label = "Point Opacity", min = 0.1 , max = 1 , value = 0.7),
                                       tags$br(),
                                       radioButtons("category_variable",label = "Category plot on:",
                                                    choices = as.list(c("SJR","H index","Total Docs. (2016)", "Total Docs. (3years)","Total Refs.","Total Cites (3years)","Citable Docs. (3years)","Cites / Doc. (2years)","Ref. / Doc." )),
                                                    selected = "SJR",inline = FALSE, width = "100%")
                                       
                                       
                                       
                                       
                               )
                               )
                ) # End sidebar layout
        ) # end fluidPage
) # end shiny