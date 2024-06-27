# Define UI for gene expression module
gene_expression_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Input Gene Name", status = "warning", solidHeader = TRUE,
      width = 12,
      textInput(ns("gene"), "Gene Name", value = "TP53"),
      radioButtons(ns("paired"), "Paired", choices = list("Yes" = TRUE, "No" = FALSE)),
      #checkboxInput(ns("paired"), "Paired", value = TRUE),
      actionButton(ns("analyze"), "Analyze")
    ),
    box(
      title = "Expression Plot", status = "warning", solidHeader = TRUE,
      width = 12,
      plotOutput(ns("expressionPlot")),
      downloadButton(ns("downloadPlot"), "Download Plot")
    ),
    box(
      title = "Gene Expression Data", status = "warning", solidHeader = TRUE,
      width = 12,
      DT::DTOutput(ns("table")),
      downloadButton(ns("downloadTable"), "Download Table")
    )
  )
}

# Define server logic for gene expression module
gene_expression_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      expression_data <- reactiveVal()
      gene_exp_plot   <- reactiveVal()
      
      observeEvent(input$analyze, {
        req(input$gene, input$paired)
        
        paired <- input$paired
        
        gene <- input$gene
        
        showNotification("Fetching gene expression data from TCGA...", type = "message", duration = 20)
        
        if(paired){
          # Query TCGA for the specified gene
          gene_exp_plot <- tryCatch({
            pan_paired_boxplot(gene)
          }, error = function(e) {
            showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
            return(NULL)
          })
          
          # Filter the data to include only the specified gene
          expression_data <- gene_exp_plot$data %>%
            group_by(Cancer, Group) %>%
            summarise(
              n      = n(),
              mean   = round(mean(.data[[gene]], na.rm = TRUE), 3),
              median = round(median(.data[[gene]], na.rm = TRUE), 3),
              sd     = round(sd(.data[[gene]], na.rm = TRUE), 3)
            )
          expression_data <- as.data.frame(expression_data)
          
          # Update the reactive value
          expression_data(expression_data)
        } else {
          # Query TCGA for the specified gene
          gene_exp_plot <- tryCatch({
            pan_tumor_boxplot(gene)
          }, error = function(e) {
            showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
            return(NULL)
          })
          
          # Filter the data to include only the specified gene
          expression_data <- gene_exp_plot$data %>%
            group_by(Cancer) %>%
            summarise(
              n      = n(),
              mean   = round(mean(.data[[gene]], na.rm = TRUE), 3),
              median = round(median(.data[[gene]], na.rm = TRUE), 3),
              sd     = round(sd(.data[[gene]], na.rm = TRUE), 3)
            )
          expression_data <- as.data.frame(expression_data)
          
          # Update the reactive value
          expression_data(expression_data)
        }
        
        if (is.null(gene_exp_plot)) {
          showNotification("No data returned for the specified gene.", type = "error", duration = NULL)
          return(NULL)
        }
        
        gene_exp_plot(gene_exp_plot)
        
        output$table <- DT::renderDT({
          DT::datatable(expression_data())
        })
        
        output$expressionPlot <- renderPlot({
          gene_exp_plot()
        })
        
        showNotification("Gene expression data fetched successfully", type = "message")
      })
      
      output$downloadTable <- downloadHandler(
        filename = function() {
          paste("gene_expression_cancer_data-", input$gene, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(expression_data(), file, quote = FALSE, row.names = FALSE)
        }
      )
      
      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste("gene_expression_cancer_plot-", input$gene, ".png", sep = "")
        },
        content = function(file) {
          png(file)
          gene_exp_plot()
          dev.off()
        }
      )
    }
  )
}
