
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(smacof)

sciMag.data <- read_csv(file = "./data/scimag.csv" )
sciMag.data$X1 <- NULL
sciMag.data$Type <- NULL

sciMag.data$Country <- as.factor(sciMag.data$Country)
sciMag.data$region <- as.factor(sciMag.data$region)
sciMag.data$open.access <- as.factor(sciMag.data$open.access)
sciMag.data$SJR <- as.numeric(gsub(pattern = ",",replacement = ".", x = sciMag.data$SJR ))
sciMag.data$`Cites / Doc. (2years)` <- as.numeric(gsub(pattern = ",",replacement = ".", x = sciMag.data$`Cites / Doc. (2years)` ))
#print(dim(sciMag.data))
#print(colnames(sciMag.data))

shinyServer(function(input, output) {
        
        #filtering the main sciMag.data
        dataset <- eventReactive(input$plot_button,{
                
                #print((input$selected_quartile))
                print(head(sciMag.data[,input$selected_variable]))
                
                sciMag.data %>% 
                        select(-c(Categories,Issn, Rank)) %>% 
                        filter(`SJR Quartile` %in% c(input$selected_quartile)) %>%
                        filter(open.access %in% input$selected_access) %>% 
                        filter(region %in% input$selected_region)
     
        }
        )
        
        
        # for printing the filtered sciMag.data in the sciMag.dataView tabset 
        output$table <- renderTable({
                
                dataset()
        })
        
        
        biplot_list <- reactive({
               
                # keeping the numeric variables for PCA
                biplot_dataset <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                #print(str(biplot_dataset))
                
                #standardization before PCA
                biplot_dataset <- scale(biplot_dataset)
                
                #PCA 
                scimag_pca <-prcomp(x = biplot_dataset)
                
                importance<- summary(scimag_pca)$importance[2,1:2]
                #print(summary(scimag_pca))
                
                # coordinates df has non-numeric variables as well as PC1 and PC2
                coordinates <- dataset() %>% 
                        select(c(Title,Country,open.access,region)) %>% 
                        cbind(scimag_pca$x[,1:2]) 
                

                #vectors df for biplot
                vectors <- data.frame(Variables = (rownames(scimag_pca$rotation)), scimag_pca$rotation[, 1:2])
                
                list(coordinates,vectors,importance)
        })
        
        # for MDS version of the plot 
        scimag_mds <- reactive({
                
                #numeric variables
                mds_dataset <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                mds_dataset <- scale(mds_dataset)
                mds_dist <- dist(mds_dataset)
                
                mds_model <- smacofSym(delta = mds_dist, ndim = 2 , type = "ratio")
                
                coordinates <- dataset() %>% 
                        select(c(Title,Country,open.access,region)) %>% 
                        cbind(mds_model$conf) 
      
        })
        
        
        output$sciMag_plot <- renderPlot({
                
                coordinates<- biplot_list()[[1]]
                vectors <- biplot_list()[[2]]
                
                ggplot(data = coordinates) + geom_point(aes(PC1,PC2))
        })
        
})
