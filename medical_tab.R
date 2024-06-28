# Define UI for gene expression module
medical_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(    
      box(
      title = "Input Gene Name",
      width = 12,
      textInput(ns("gene"), "Gene Name", value = "TP53"),
      selectInput(ns("cancer"), "Select Cancer type", choices = cancertypes),
      actionButton(ns("analyze"), "Analyze")
      ),
      tabsetPanel(
        tabPanel(
          title = "Survavial analysis",
          plotOutput(ns("survivalPlot")),
          downloadButton(ns("downloadSurvivalPlot"), "Download Plot")
        ),
        tabPanel(
          title = "Diagnostic ROC Curve",
          plotOutput(ns("rocPlot")),
          downloadButton(ns("downloadRocPlot"), "Download Plot")
        )
      )
    )
  )
}

# Define server logic for gene expression module
medical_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_survival_reactive      <- reactiveVal()
      plot_roc_reactive           <- reactiveVal()

      observeEvent(input$analyze, {
        req(input$gene, input$cancer)
        
        gene   <- input$gene
        cancer <- input$cancer
        
        tryCatch({
          showNotification("Calculating K-M survival plot", type = "message", duration = 5)
          survival_out <- tcga_kmplot(cancer,gene)
        }, error = function(e){
          survival_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        tryCatch({
          showNotification("Calculating Diagnosis ROC curve", type = "message", duration = 5)
          roc_out <- tcga_roc(cancer,gene)
        }, error = function(e){
          roc_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        output$survivalPlot <- renderPlot({
          survival_out
        })
        
        output$rocPlot <- renderPlot({
          roc_out
        })

        plot_survival_reactive(survival_out)
        plot_roc_reactive(survival_out)
      })
      
      output$downloadSurvivalPlot <- downloadHandler(
        filename = function() {
          paste("K-M_survival_plot-", input$gene, "-", input$cancer,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_survival_reactive())
          dev.off()
        }
      )
      
      output$downloadRocPlot <- downloadHandler(
        filename = function() {
          paste("Diagnosis_ROC_curve-", input$gene, "-", input$cancer,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_roc_reactive())
          dev.off()
        }
      )
    }
  )
}
