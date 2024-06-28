# Define UI for gene expression module
networks_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(    
      box(
      title = "Input Gene List",
      width = 12,
      fileInput(ns("geneFile"), "Choose CSV File", accept = ".csv"),
      actionButton(ns("analyze"), "Analyze")
      ),
      tabsetPanel(
        tabPanel(
          title = "GO",
          plotOutput(ns("goPlot")),
          downloadButton(ns("downloadGoPlot"), "Download Plot")
        ),
        tabPanel(
          title = "KEGG",
          plotOutput(ns("keggPlot")),
          downloadButton(ns("downloadKeggPlot"), "Download Plot")
        )
      )
    )
  )
}

# Define server logic for gene expression module
networks_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_go_reactive      <- reactiveVal()
      plot_kegg_reactive    <- reactiveVal()

      observeEvent(input$analyze, {
        req(input$geneFile)
        
        gene_list_data <- reactive({
          inFile <- input$geneFile
          if (is.null(inFile))
            return(NULL)
          read.csv(inFile$datapath, header = FALSE)
        })
        
        
        tryCatch({
          showNotification("Calculating GO network", type = "message", duration = 10)
          go_out <- gene_network_go(gene_list_data()[, 1])
        }, error = function(e){
          go_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        tryCatch({
          showNotification("Calculating KEGG network", type = "message", duration = 10)
          kegg_out <- gene_network_kegg(gene_list_data()[, 1])
        }, error = function(e){
          kegg_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        output$goPlot <- renderPlot({
          go_out
        })
        
        output$keggPlot <- renderPlot({
          kegg_out
        })

        plot_go_reactive(go_out)
        plot_kegg_reactive(kegg_out)
      })
      
      output$downloadGoPlot <- downloadHandler(
        filename = function() {
          paste("gene_go-network_plot-",".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_go_reactive())
          dev.off()
        }
      )
      
      output$downloadKeggPlot <- downloadHandler(
        filename = function() {
          paste("gene_kegg-network_plot",".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_kegg_reactive())
          dev.off()
        }
      )
    }
  )
}
