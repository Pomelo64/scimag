
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(readr)

data <- read_csv(file = "./Data/scimag.csv" )
data$X1 <- NULL
data$Type <- NULL

data$Country <- as.factor(data$Country)
data$region <- as.factor(data$region)
data$open.access <- as.factor(data$open.access)
data$SJR <- as.numeric(gsub(pattern = ",",replacement = ".", x = data$SJR ),)


shinyServer(function(input, output) {
        
        
        output$table <- renderTable({
                data
        })
        
})
