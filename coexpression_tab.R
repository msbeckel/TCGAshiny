# Define UI for coexpression module
coexpression_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Upload Gene List",
      width = 12,
      fileInput(ns("geneFile"), "Choose CSV File", accept = ".csv"),
      selectInput(ns("cancerType"), "Select Cancer type", choices = cancertypes),
      actionButton(ns("analyzeFile"), "Analyze")
    ),
    box(
      title = "Correlation Plot",
      width = 12,
      plotOutput(ns("correlationPlot")),
      downloadButton(ns("downloadCorrelationPlot"), "Download Correlation Plot"),
      downloadButton(ns("downloadCorrelationMatrix"), "Download Correlation Matrix")
    )
  )
}

# Define server logic for coexpression module
coexpression_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      coexpression_data <- reactiveVal()
      testRes           <- reactiveVal()
      
      observeEvent(input$analyzeFile, {
        req(input$geneFile, input$cancerType)
        
        gene_list_data <- reactive({
          inFile <- input$geneFile
          if (is.null(inFile))
            return(NULL)
          read.csv(inFile$datapath, header = FALSE)
        })
        
        cancerType <- reactive({
          return(input$cancerType)
        })
        
        #Get gene expression from TCGAplot
        gene_exp <- lapply(gene_list_data()[, 1], function(gene) {
          tryCatch({
            showNotification(paste("Fetching data for gene:", gene), type = "message", duration = 20)
            pan_tumor_boxplot(gene)$data
          }, error = function(e) {
            showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
            return(NULL)
          })
        })
        
        dfGeneExp = do.call("cbind", gene_exp)
        dfGeneExp = as.data.frame(dfGeneExp[, -seq(3,ncol(dfGeneExp), by=2)])
        
        showNotification(paste("Nrow:", nrow(dfGeneExp)), type = "message", duration = 60)
        showNotification(paste("Crow:", ncol(dfGeneExp)), type = "message", duration = 60)
        
        # Split data by Cancer and calculate correlation matrices
        tryCatch({
          correlations <- dfGeneExp %>%
            split(.$Cancer) %>%
            map(~ cor(dplyr::select(.x, !starts_with("Cancer"))))
          showNotification(paste("Correlation ready"), type = "message")
        }, error = function(e){
          showNotification(paste("Error calculating correlation"), type = "error", duration = NULL)
        }) 
        

        #Correlation test
        tryCatch({
          testRes = corrplot::cor.mtest(correlations[[cancerType()]], conf.level = 0.95)
          showNotification(paste("Correlation test ready"), type = "message")
        }, error = function(e){
          showNotification(paste("Error calculating correlation test"), type = "error", duration = NULL)
        }) 
        
        
        # Continue with processing or handle errors
        output$correlationPlot <- renderPlot({
          corrplot::corrplot(correlations[[cancerType()]], p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
        })
        
        # Update the reactive value
        coexpression_data(correlations[[cancerType()]])
        testRes(testRes)
      })
      
      output$downloadCorrelationPlot <- downloadHandler(
        filename = function() {
          paste("correlation_plot-", input$cancerType, "-", Sys.Date(), ".png", sep = "")
        },
        content = function(file) {
          png(file)
          corrplot::corrplot(coexpression_data(), p.mat = testRes()$p, method = 'color', diag = FALSE, type = 'upper',
                   sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
                   insig = 'label_sig', pch.col = 'grey20', order = 'AOE')
          dev.off()
        }
      )
      
      output$downloadCorrelationMatrix <- downloadHandler(
        filename = function() {
          paste("gene_coexpression_cancer_data-", input$cancerType, "-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(coexpression_data(), file, quote = FALSE, row.names = FALSE)
        }
      )
    }
  )
}
