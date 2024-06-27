# Define UI for gene expression module
deg_ui <- function(id) {
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
          title = "DEG",
          plotOutput(ns("heatmapPlot")),
          downloadButton(ns("downloadHeatmapPlot"), "Download Plot")
        ),
        tabPanel(
          title = "GSEA-GO",
          plotOutput(ns("goPlot")),
          downloadButton(ns("downloadGoPlot"), "Download Plot")
        ),
        tabPanel(
          title = "GSEA-KEGG",
          plotOutput(ns("keggPlot")),
          downloadButton(ns("downloadKeggPlot"), "Download Plot")
        )
      )
    )
  )
}

# Define server logic for gene expression module
deg_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_heatmap_reactive <- reactiveVal()
      plot_go_reactive      <- reactiveVal()
      plot_kegg_reactive    <- reactiveVal()

      observeEvent(input$analyze, {
        req(input$gene, input$cancer)
        
        gene   <- input$gene
        cancer <- input$cancer

        tryCatch({
          showNotification(paste("Fetching expression data for gene:", gene), type = "message", duration = 30)
          heatmap_out <- gene_deg_heatmap(cancer, gene,top_n=20)
        }, error = function(e){
          heatmap_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        }) 
        
        tryCatch({
          showNotification("Calculating GSEA-GO", type = "message", duration = 30)
          go_out <- gene_gsea_go(cancer,gene)
        }, error = function(e){
          go_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        tryCatch({
          showNotification("Calculating GSEA-KEGG", type = "message", duration = 30)
          kegg_out <- gene_gsea_kegg(cancer,gene)
        }, error = function(e){
          kegg_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        output$heatmapPlot <- renderPlot({
          heatmap_out
        })
        
        output$goPlot <- renderPlot({
          go_out
        })
        
        output$keggPlot <- renderPlot({
          kegg_out
        })

        plot_heatmap_reactive(heatmap_out)
        plot_go_reactive(go_out)
        plot_kegg_reactive(kegg_out)
      })
      
      output$downloadHeatmapPlot <- downloadHandler(
        filename = function() {
          paste("gene_deg_plot-", gsub(" ", "", input$cancer), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_heatmap_reactive())
          dev.off()
        }
      )
      
      output$downloadGoPlot <- downloadHandler(
        filename = function() {
          paste("gene_gsea-go_plot-", gsub(" ", "", input$cancer), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_go_reactive())
          dev.off()
        }
      )
      
      output$downloadKeggPlot <- downloadHandler(
        filename = function() {
          paste("gene_gsea-kegg_plot-", gsub(" ", "", input$cancer), "_", input$gene,".png", sep = "")
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
