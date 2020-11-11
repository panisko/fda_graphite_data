#
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
url <-
    'https://raw.github.com/panisko/graphite2r/master/resource/20200401_20200501_file.csv'
graphite_data <-
    read.csv(
        url,
        na.strings = c("NA"),
        quote = "\"",
        header = TRUE,
        sep = ' '
    )
### Convert data to matrix
data <- GraphiteToMatrix(graphite_data)

#Global variavles
clust.basis = c("bspline",
                "fourier",
                "constant",
                "power",
                "exponential",
                "polygonal",
                "monomial")

metrics = c("minkowski")

methods = c("complete", "average")

#UI
ui <- fluidPage(
    # Application title
    titlePanel("Functional data clustering of time series/graphite data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                "basis",
                label = "Basis type",
                choices = clust.basis,
                selected =
                    'fourier'
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
                selected = "minkowski"
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
                max = 1000,
                value = 51
            ),
            sliderInput(
                "ncl",
                label = "Number of clusters (PAM)",
                min = 1,
                max = 7,
                value = 3
            ),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("pca"),
            plotOutput("pcaHarm"),
            plotOutput("pam"),
            plotOutput("fdata", width = "100%"),
            plotOutput("hclust"),
            plotOutput("dataPlot"), 
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    clust.data <- reactive({
        fca(
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
            ncl=input$ncl,
            lambda = 1e6
        )
    })
    output$dataPlot <- renderPlot({
        # fca_data <- clust.data()$data
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
        plot.fd(
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
        # axis(side=1,at=1:ncol(data), labels = colnames(data))
        axis(
            side = 1,
            at = 1:length(fdata$fdnames$time),
            labels = fdata$fdnames$time
        )
        legend(
            "topleft",
            legend = rownames(data),
            col = 1:ncol(data),
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
        # plot(
        #     pca$scores,
        #     ylab = "PC Score 2",
        #     xlab = "PC Score 1",
        #     col = 4,
        #     cex.lab = 0.5,
        #     ylim = c(min(pca$scores[, 2] - 1),
        #              max(pca$scores[, 2] + 1)),
        #     xlim = c(min(pca$scores[, 1] - 1),
        #              max(pca$scores[, 1] +
        #                      1)),
        #     type = "n"
        # )
        # text(pca$scores[, 1],
        #      pca$scores[, 2],
        #      labels = rownames(pca$scores),
        #      cex = 1)
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
    # Run the application
    shinyApp(ui = ui, server = server)
    