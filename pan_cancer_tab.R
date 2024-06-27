# Define UI for gene expression module
pan_cancer_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        title = "Settings",
        width = 12,
        textInput(ns("gene"), "Input Gene Name", value = "TP53"),
        selectInput(ns("type"), "Select Feature type", choices = c("TMB",
                                                                   "MSI", 
                                                                   "Immune Cell Ratio", 
                                                                   "Immune Score")),
        actionButton(ns("analyze"), "Analyze")
      ),
      box(
        title = "Feature Correlation Plot",
        width = 12,
        plotOutput(ns("corPlot")),
        downloadButton(ns("downloadCorPlot"), "Download Plot")
      )
    )
  )
}

# Define server logic for gene expression module
pan_cancer_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      gene_reactive <- reactiveVal()
      type_reactive <- reactiveVal()
      
      observeEvent(input$analyze, {
        req(input$gene, input$type)
        
        showNotification("Fetching data from TCGA...", type = "message", duration = 20)
        
        gene <- input$gene
        
        type <- reactive({
          return(input$type)
        })
        
        type_chosen<-function(gene, type){
          if(type() == "TMB"){
            gene_TMB_radar(gene)
          } else if(type() == "MSI"){
            gene_MSI_radar(gene)
          } else if(type() == "Immune Cell Ratio"){
            gene_immucell_heatmap(gene)
          } else if(type() == "Immune Score"){
            gene_immunescore_heatmap(gene)
          } else {
            return(NULL)
          }
        }
        
        output$corPlot <- renderPlot({
          type_chosen(gene = gene, type = type())
        })
        
        gene_reactive(gene)
        type_reactive(type)
      })
      
      output$corPlot <- downloadHandler(
        filename = function() {
          paste("pan_cancer_correlation_plot-", input$type, "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          output$corPlot
          dev.off()
        }
      )
    }
  )
}
