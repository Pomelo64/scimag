
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(readr)

sciMag.data <- read_csv(file = "./data/scimag.csv" )
sciMag.data$X1 <- NULL
sciMag.data$Type <- NULL

sciMag.data$Country <- as.factor(sciMag.data$Country)
sciMag.data$region <- as.factor(sciMag.data$region)
sciMag.data$open.access <- as.factor(sciMag.data$open.access)
sciMag.data$SJR <- as.numeric(gsub(pattern = ",",replacement = ".", x = sciMag.data$SJR ))
#print(dim(sciMag.data))
print(colnames(sciMag.data))

shinyServer(function(input, output) {
        
        #filtering the main sciMag.data
        dataset <- reactive({
                
                #print((input$selected_quartile))
                
                sciMag.data %>% 
                        select(-c(Categories,Issn, Rank)) %>% 
                        filter(`SJR Quartile` %in% c(input$selected_quartile)) %>%
                        filter(open.access %in% input$selected_access) %>% 
                        filter(region %in% input$selected_region)
     
        }
        )
        
        
        # for printing the filtered sciMag.data in the sciMag.dataView tabset 
        output$table <- renderTable({
                #print(dim(dataset()))
                dataset()
        })
        
})
