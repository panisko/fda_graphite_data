# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#install.packages('devtools')
library(shiny)
library('fda')
library(devtools)
#devtools::install_github("moviedo5/fda.usc")
library('fda.usc')
source("./functions.R")
# url <-
# 'https://raw.github.com/panisko/graphite2r/master/resource/20200401_20200501_file.csv'


#Global variavles
clust.basis = c("bspline",
                "fourier",
                "constant",
                "power",
                "exponential",
                "polygonal",
                "monomial")

dist.metrics = c("euclidean",
                 "maximum",
                 "manhattan",
                 "canberra",
                 "binary",
                 "minkowski")
metrics = c("metric.DTW",
            "metric.lp",
            "metric.hausdorff",
            "semimetric.NPFDA")

methods = c(
    "complete",
    "average",
    "ward.D",
    "ward.D2",
    "single",
    "mcquitty",
    "median",
    "centroid"
)

#UI
ui <- fluidPage(
    # Application title
    titlePanel("Functional data clustering of time series/graphite data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(
                "date",
                strong("Date"),
                format = "yyyy-mm-dd",
                start = Sys.Date(),
                end = Sys.Date()
            ),
            radioButtons(
                "removeEmptyRows",
                label = "Remove empty rows?",
                choices = c("Yes", "No"),
                selected = "Yes"
            ),
            radioButtons(
                "basis",
                label = "Basis type",
                choices = clust.basis,
                selected =
                    'fourier'
            ),
            conditionalPanel(
                condition = "input.basis.indexOf('bspline') > -1",
                sliderInput(
                    "bspline.norder",
                    label  = "norder na razie nie działa",
                    min = 0,
                    max = 10,
                    value = 4
                )
            ),
            radioButtons(
                "method",
                label = "Clustering method",
                choices = methods,
                selected = "average"
            ),
            radioButtons(
                "metric",
                label = "Metric",
                choices = metrics,
                selected = "metric.lp"
            ),
            sliderInput(
                "nderiv",
                label = "Deriv",
                min = 0,
                max = 2,
                value = 0
            ),
            sliderInput(
                "nharm",
                label = "nharm",
                min = 1,
                max = 5,
                value = 2
            ),
            sliderInput(
                "nbasis",
                label = "Number of basis functions",
                min = 1,
                max = 300,
                value = 51
            ),
            sliderInput(
                "ncl",
                label = "Number of clusters (PAM)",
                min = 1,
                max = 7,
                value = 3
            ),
            actionButton("fda", "Funcit"),
            downloadButton("saveCsv", "Save CSV file"),
            fileInput(
                "file",
                "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            # ,actionButton("uploadCvs", "Upload CSV file")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("dataPlot"),
            plotOutput("fdata"),
            plotOutput("pca"),
            plotOutput("pcaHarm"),
            plotOutput("pam"),
            plotOutput("hclust"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    graphiteData <- reactive({
        if (is.null(input$file)) {
            startDate <- format(input$date[1], "%Y%m%d")
            endDate <- format(input$date[2], "%Y%m%d")
            url <-
                BuildUrl(
                    graphiteHost = "http://graphite:8080",
                    startDate = startDate,
                    endDate = endDate
                )
            graphiteData <- GetData(url = url, separator = ',')
        }
        else {
            tryCatch({
                file <- input$file$datapath
                graphiteData <-
                    read.table(
                        file = 'test.csv',
                        header = TRUE,
                        sep = ',',
                        dec = '.',
                        check.names = FALSE,
                        quote = "\""
                    )
                # colnames(data) <- as.numeric(colnames(data))
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            })
        }
        return(graphiteData)
    })
    
    data <- reactive( {
        data <- GraphiteToMatrix(graphiteData())
       
         if (input$removeEmptyRows == "Yes") {
             data <- RemoveEmptyRows(data)
        }
        #Replacing missing values with zeros
        data[is.na(data)] <- 0
        return(data)
    })
    
    output$saveCsv <- downloadHandler(
        # data <- data()
        filename = function() {
            paste(
                "graphite_data_",
                format(input$date[1], "%Y%m%d"),
                '_',
                format(input$date[2], "%Y%m%d"),
                ".csv",
                sep = ''
            )
        } ,
        # content = write.csv(data(), )
        content = function(file) {
            write.csv(data(), file)
        },
        contentType = "text/csv"
        
    )
    
    clust.data <- eventReactive(input$fda, {
        data <- data()
        fca <- fca(
            data = data,
            basis =  input$basis,
            nderiv = input$nderiv,
            #width = 1100,
            #height = 660,
            nbasis = input$nbasis,
            metric = input$metric,
            method = input$method,
            optimise = FALSE,
            nharm = input$nharm,
            ncl = input$ncl,
            lambda = 1e6
        )
        return(fca)
    })
    output$dataPlot <- renderPlot({
        data <- data()
        matplot(
            t(data),
            lty = 0.5,
            cex = 0.1,
            type = "p",
            pch = 1,
            col = 1:ncol(data),
            ylab = "Temperatura",
            xlab = "Czas w formacie epoc",
            axes = F,
        )
        axis(2)
        axis(
            side = 1,
            at = 1:ncol(data),
            labels = colnames(data)
        )
        legend(
            "topleft",
            legend = rownames(data),
            col = 1:7,
            pch = 1
        )
    })
    output$fdata <- renderPlot({
        fdata <- clust.data()$fd
        mean <- clust.data()$mean
        plot(
            fdata,
            ylab = "Temperatura",
            xlab = "Czas w formacie epoc",
            col = 1:length(fd$fdnames$reps),
            cex = 0.1,
            axes = F
        )
        lines(mean,
              cex = 14,
              cex.lab = 10 ,
              col = "red")
        axis(2)
        axis(
            side = 1,
            at = 1:length(fdata$fdnames$time),
            labels = fdata$fdnames$time
        )
        legend(
            "topleft",
            legend = fdata$fdnames$reps,
            col = 1:length(fdata$fdnames$reps),
            pch = 1
        )
    })
    output$hclust <- renderPlot({
        hclust <- clust.data()$hclust
        plot(
            hclust,
            main = "Dendogram metryka Mińskowskiego p=2" ,
            sub = "",
            xlab = "Pokój",
            ylab = "Miara (nie)podobieństwa"
        )
    })
    output$pca <- renderPlot({
        pca <- clust.data()$pca
        plot(
            pca$scores,
            ylab = "PC Score 2",
            xlab = "PC Score 1",
            col = 4,
            cex.lab = 0.5,
            ylim = c(min(pca$scores[, 2] - 1),
                     max(pca$scores[, 2] + 1)),
            xlim = c(min(pca$scores[, 1] - 1),
                     max(pca$scores[, 1] +
                             1)),
            type = "n"
        )
        text(pca$scores[, 1],
             pca$scores[, 2],
             labels = rownames(pca$scores),
             cex = 1)
        
        
    })
    output$pcaHarm <- renderPlot({
        pca <- clust.data()$pca
        plot(pca$harmonics)
        #plot.pca.fd(pca, cycle = FALSE)
    })
    output$pam <- renderPlot({
        pam <- clust.data()$pam
        data <- pam$centers$data
        matplot(
            t(data),
            lty = 0.5,
            cex = 0.1,
            #type = "p",
            pch = 1,
            col = 1:ncol(data),
            ylab = "Temperatura",
            xlab = "Czas w formacie epoc",
            axes = F,
        )
        
        axis(2)
        axis(
            side = 1,
            at = 1:ncol(data),
            labels = colnames(data)
        )
        legend(
            "topleft",
            legend = rownames(data),
            col = 1:ncol(data),
            pch = 1
        )
    })
}
shinyApp(ui = ui, server = server)
# Run the application
