# Define UI for gene expression module
geneset_cor_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      box(
        title = "Settings",
        width = 12,
        textInput(ns("gene"), "Input Gene Name", value = "TP53"),
        selectInput(ns("type"), "Select Gene Set", choices = c("Immune Checkpoint", 
                                                               "Chemokine", 
                                                               "Chemokine Receptors", 
                                                               "Immune Stimulators", 
                                                               "Immune Inhibitors")),
        selectInput(ns("method"), "Select Correlation Method", choices =list("Pearson"  = "pearson", 
                                                                             "Spearman" = "spearman",
                                                                             "Kendall"  = "kendall")),
        actionButton(ns("analyze"), "Analyze")
      ),
      box(
        title = "Geneset Correlation Plot",
        width = 12,
        plotOutput(ns("corPlot")),
        downloadButton(ns("downloadCorPlot"), "Download Plot")
      )
    )
  )
}

# Define server logic for gene expression module
geneset_cor_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      #gene_reactive <- reactiveVal()
      #type_reactive <- reactiveVal()
      plot_reactive  <- reactiveVal()

      observeEvent(input$analyze, {
        req(input$gene, input$type)
        
        gene <- input$gene
        
        type <- reactive({
          return(input$type)
        })
        
        method <- reactive({
          return(input$method)
        })
        
        
        type_chosen<-function(gene, type){
          if(type() == "Immune Checkpoint"){
            gene_checkpoint_heatmap(gene, method = method())
          } else if(type() == "Chemokine"){
            gene_chemokine_heatmap(gene, method = method())
          } else if(type() == "Chemokine Receptors"){
            gene_receptor_heatmap(gene, method = method())
          }else if(type() == "Immune Stimulators"){
            gene_immustimulator_heatmap(gene, method = method())
          }else if(type() == "Immune Inhibitors"){
            gene_immuinhibitor_heatmap(gene, method = method())
          }else {
            return(NULL)
          }
        }
        
        tryCatch({
          showNotification(paste("Fetching data for gene:", gene, "May take a couple of minutes"), type = "message", duration = 60)
          showNotification("May take a couple of minutes", type = "message", duration = 120)
          out <- type_chosen(gene = gene, type = type())
          
        }, error = function(e){
          out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        }) 
        
        output$corPlot <- renderPlot({
          out
        })
        
        plot_reactive(out)
        #gene_reactive(gene)
        #type_reactive(type)
      })
      
      output$downloadCorPlot <- downloadHandler(
        filename = function() {
          paste("geneset_correlation_plot-", gsub(" ", "", input$type), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_reactive())
          dev.off()
        }
      )
    }
  )
}
