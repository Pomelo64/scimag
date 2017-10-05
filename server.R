
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
library(ggrepel)
library(PerformanceAnalytics)
library(DT)

# bug ---> shape legend  : done 

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


print(colnames(sciMag.data))
print("-----")

shinyServer(function(input, output) {

# ------- Filtering the main dataset 
        
        #filtering the main sciMag.data
        dataset <- eventReactive(input$plot_button,{
                
                # First remove unnecessary variables
                sciMag.data<- sciMag.data %>% 
                        select(-c(Categories,Issn, Rank)) 
                
                #Here for using the selected_cols in select()
                myCols <- input$selected_variable
                selected_cols <- match(myCols,colnames(sciMag.data))
                
                #filtering the dataset
                sciMag.data <- sciMag.data %>% 
                        filter(`SJR Quartile` %in% c(input$selected_quartile)) %>%
                        filter(open.access %in% input$selected_access) %>% 
                        filter(region %in% input$selected_region) %>% 
                        select(c(`SJR Quartile`,open.access,region,Title,Country,selected_cols))
                 
                
                
                
                
     
        }
        )
        
# ------ Table for data view
  
        # for printing the filtered sciMag.data in the sciMag.dataView tabset 
        output$table <- DT::renderDataTable({
                data<- dataset()
                datatable(data)
        })
        
        
        
# ------- biplot dataset        
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
                       
                        #select(c(Title,Country,open.access,region,`SJR Quartile`)) %>% 
                       
                        cbind(scimag_pca$x[,1:2]) 
                

                #vectors df for biplot
                vectors <- data.frame( vector_label = rownames(scimag_pca$rotation), scimag_pca$rotation[, 1:2])
                
                list(coordinates,vectors,importance)
        })
        
#------- PCA Biplot function
        
        pca_brush_ranges <- reactiveValues(x = NULL, y = NULL)
        
        biplot_func <- reactive({
                coordinates<- biplot_list()[[1]]
                vectors <- biplot_list()[[2]]

            g<- ggplot(data = coordinates) + 
                        geom_point(aes(x = PC1,
                                       y = PC2),
                                   size = input$point_size,
                                   alpha = input$point_alpha) + 
                        geom_segment(data=vectors, 
                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                     col="red") + 
                        geom_text_repel(data=vectors ,
                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                        col="red" 
                        )
                
                # shape yes 
                if (input$point_shape != "None"){ 
                        
                        #shape yes - color yes 
                        if (input$color_variable != "None" ) {
                                
                                
                                
                                shape_index <- match(input$point_shape,colnames(coordinates))
                                coordinates$shape <- coordinates[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(coordinates))

                                coordinates$color <- coordinates[,color_index]
                                
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       shape = shape,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) +
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                         # shape yes- color no        
                        } else {
                                
                                index <- match(input$point_shape,colnames(coordinates))
                                coordinates$shape <- coordinates[,index]
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       shape = shape),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = coordinates,
                                                        aes(x = PC1 , y = PC2),
                                                        label = dataset$Title , 
                                                        color = "orange", alpha = 0.4 )
                        }
                        
                # shape no 
                } else {
                        #shape no - color yes 
                        if (input$color_variable != "None" ) {
                                
                                
                                
                                #shape_index <- match(input$point_shape,colnames(coordinates))
                                #coordinates$shape <- coordinates[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(coordinates))
                                coordinates$color <- coordinates[,color_index]
                                
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2,
                                                       
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) +
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                                # shape no- color no        
                        } else {
                                
                                #index <- match(input$point_shape,colnames(coordinates))
                                #coordinates$shape <- coordinates[,index]
                                
                                g<- ggplot(data = coordinates) + 
                                        geom_point(aes(x = PC1,
                                                       y = PC2),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        
                                        geom_segment(data=vectors, 
                                                     aes(PC1*input$biplot_vector_size, PC2*input$biplot_vector_size, xend=0, yend=0), 
                                                     col="red" ) + 
                                        geom_text_repel(data=vectors ,
                                                        aes(x = PC1*input$biplot_vector_size,y =  PC2*input$biplot_vector_size, label = vector_label ),
                                                        col="red" )
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = coordinates,
                                                        aes(x = PC1 , y = PC2,label = dataset$Title ),
                                                        color = "orange", alpha = 0.8 )
                        }
                }
            
                g <- g + 
                        coord_cartesian(xlim = pca_brush_ranges$x,
                                        ylim = pca_brush_ranges$y,
                                        expand = TRUE) +
                         theme_linedraw(base_size = 16)
               
                
                
                return(g)
                
        })
        
        observeEvent(input$plot_dblclick, {
                brush <- input$plot_brush
                if (!is.null(brush)) {
                        pca_brush_ranges$x <- c(brush$xmin, brush$xmax)
                        pca_brush_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        pca_brush_ranges$x <- NULL
                        pca_brush_ranges$y <- NULL
                }
        })
        
        # for the DT table. it uses brush info
        output$brush_info_pca <- DT::renderDataTable({
                        
                        data <- biplot_list()[[1]]
                        
                        res <- brushedPoints(data,input$plot_brush)
                        datatable(res)
                        
                })
                
       
                
                
        
## MDS Part
# ------- MDS coordinates dataset        
        # for MDS version of the plot 
        scimag_mds <- reactive({
                
                #numeric variables
                mds_dataset <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                mds_dataset <- scale(mds_dataset)
                mds_dist <- dist(mds_dataset)
                
                mds_model <- smacofSym(delta = mds_dist, ndim = 2 , type = "ratio")
                
                #the badnes-of-fit
                stress <- mds_model$stress
                
                coordinates <- dataset() %>% 
                        cbind(mds_model$conf) 
      
        })
        
        

        
#------- MDS plot function
        mds_brush_ranges <- reactiveValues(x = NULL, y = NULL)
        
        mds_plot_func <- reactive({
                
                mds_dataset <- scimag_mds()
             
                
                g<- ggplot(data = mds_dataset) + 
                        geom_point(aes(x = D1,
                                       y = D2),
                                   size = input$point_size,
                                   alpha = input$point_alpha) 
                
                
                if (input$point_shape != "None"){ #yes shape 

                        if (input$color_variable != "None" ) { #yes shape - yes color
                                
                                
                                
                                shape_index <- match(input$point_shape,colnames(mds_dataset))
                                mds_dataset$shape <- mds_dataset[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(mds_dataset))

                                mds_dataset$color <- mds_dataset[,color_index]
                                
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       shape = shape,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) 
                                        
                                        
                                
                                
                        } else { #yes shape - no color
                                
                                index <- match(input$point_shape,colnames(mds_dataset))
                                mds_dataset$shape <- mds_dataset[,index]
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       shape = shape),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha)
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = mds_dataset,
                                                        aes(x = D1 , y = D2),
                                                        label = dataset$Title , 
                                                        color = "orange", size = 1, alpha = 0.4 )
                        }
                        
                        
                } else { #no shape 
                        
                        if (input$color_variable != "None" ) { #no shape - yes color
                                
                                
                                
                                #shape_index <- match(input$point_shape,colnames(mds_dataset))
                                #mds_dataset$shape <- mds_dataset[,shape_index]
                                
                                color_index <- match(input$color_variable, colnames(mds_dataset))
                                
                                mds_dataset$color <- mds_dataset[,color_index]
                                
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2,
                                                       color = color),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha) + 
                                        labs(title = input$dim_reduct_method, color = input$color_variable) 
                                
                                
                                
                                
                        } else { #no shape no color
                                
                                #index <- match(input$point_shape,colnames(mds_dataset))
                                #mds_dataset$shape <- mds_dataset[,index]
                                
                                g<- ggplot(data = mds_dataset) + 
                                        geom_point(aes(x = D1,
                                                       y = D2
                                                       ),
                                                   size = input$point_size,
                                                   alpha = input$point_alpha)
                                
                        }
                        
                        if (input$journal_label == "Yes") {
                                dataset <- dataset()
                                g <- g + 
                                        geom_text_repel(data = mds_dataset,
                                                        aes(x = D1 , y = D2),
                                                        label = dataset$Title , 
                                                        color = "orange", size = 1, alpha = 0.4 )
                        }
                        
                }
                
                g <- g   + 
                        coord_cartesian(xlim = mds_brush_ranges$x,
                                        ylim = mds_brush_ranges$y,
                                        expand = TRUE) +
                         theme_linedraw()
                
                return(g)
                
                
        })
        
        #for brushing and zooming as well as DT
        observeEvent(input$plot_dblclick, {
                brush <- input$plot_brush
                if (!is.null(brush)) {
                        mds_brush_ranges$x <- c(brush$xmin, brush$xmax)
                        mds_brush_ranges$y <- c(brush$ymin, brush$ymax)
                        
                } else {
                        mds_brush_ranges$x <- NULL
                        mds_brush_ranges$y <- NULL
                }
        })
        
        #for DT output, it uses brush data
        output$brush_info <- DT::renderDataTable({
                data <- scimag_mds()
               
                res <- brushedPoints(data, input$plot_brush)
                datatable(res)
        })
        
# -------- plot generation  
        
        output$sciMag_plot <- renderPlot({
                
                switch(EXPR = input$dim_reduct_method,
                       "PCA" = biplot_func(),
                       "MDS" = mds_plot_func()
                       )
                
        })
        
# ------ color UI 
        output$color_variable_select <- renderUI({
                selectInput(inputId = "color_variable",
                            label = "Point Color reflects:",
                            choices = c("None",input$selected_variable,"region","open.access","SJR Quartile")
                )
        })
        


# ------- Correlation 

        output$correlation_plot <- renderPlot({
                corr_data <- dataset() %>% 
                        select(-c(Title,Country,open.access,region,`SJR Quartile`))
                
                chart.Correlation(corr_data, histogram=TRUE, pch=19) 
        })

# ----- download graph 
        
        output$download_plot <- downloadHandler(
                filename = "SciMap_plot.png",
                content = function(file) {
                        g<- switch(EXPR = input$dim_reduct_method,
                               "PCA" = biplot_func(),
                               "MDS" = mds_plot_func()
                                )
                        ggsave(file, g ,device = "png", dpi = 450)
                        
                }
        )

}) # end of shiny()