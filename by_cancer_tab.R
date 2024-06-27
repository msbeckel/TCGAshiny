# Define UI for gene expression module
by_cancer_ui <- function(id) {
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
          title = "Expression",
          radioButtons(ns("paired"), "Paired", choices = list("Yes" = TRUE, "No" = FALSE)),
          plotOutput(ns("expressionPlot")),
          downloadButton(ns("downloadExpressionPlot"), "Download Plot")
        ),
        tabPanel(
          title = "Grouped by Age",
          numericInput(ns("age1"), "Age1", value = 40, min = 5, max = 120, step = 5),
          numericInput(ns("age2"), "Age2", value = 60, min = 5, max = 120, step = 5),
          plotOutput(ns("agePlot")),
          downloadButton(ns("downloadAgePlot"), "Download Plot")
        ),
        tabPanel(
          title = "Grouped by Gender",
          plotOutput(ns("genderPlot")),
          downloadButton(ns("downloadGenderPlot"), "Download Plot")
        ),
        tabPanel(
          title = "Grouped by Stage",
          plotOutput(ns("stagePlot")),
          downloadButton(ns("downloadStagePlot"), "Download Plot")
        )
      )
    )
  )
}

# Define server logic for gene expression module
by_cancer_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      plot_exp_reactive     <- reactiveVal()
      plot_age_reactive     <- reactiveVal()
      plot_gender_reactive  <- reactiveVal()
      plot_stage_reactive   <- reactiveVal()

      observeEvent(input$analyze, {
        req(input$gene, input$cancer)
        req(input$age1, input$age2, input$paired)
        
        gene   <- input$gene
        cancer <- input$cancer
        age1   <- input$age1
        age2   <- input$age2
        
        paired <- input$paired
        
        tryCatch({
          showNotification(paste("Fetching expression data for gene:", gene), type = "message", duration = 10)
          if(paired){
            exp_out <- paired_boxplot(cancer = cancer,gene = gene)
          } else{
            exp_out <- tcga_boxplot(cancer = cancer,gene = gene)
          }
        }, error = function(e){
          exp_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        }) 
        
        tryCatch({
          showNotification("Grouping by age", type = "message", duration = 10)
          age_out <- gene_3age(cancer,gene,age=age1,age2 = age2)
        }, error = function(e){
          age_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        tryCatch({
          showNotification("Grouping by gender", type = "message", duration = 10)
          gender_out <- gene_gender(cancer,gene)
        }, error = function(e){
          gender_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        tryCatch({
          showNotification("Grouping by stage", type = "message", duration = 10)
          stage_out  <- gene_stage(cancer,gene)
        }, error = function(e){
          gender_out <- NULL
          showNotification(paste("Error fetching data for gene:", gene), type = "error", duration = NULL)
        })
        
        output$expressionPlot <- renderPlot({
          exp_out
        })
        
        output$agePlot <- renderPlot({
          age_out
        })
        
        output$genderPlot <- renderPlot({
          gender_out
        })
        
        output$stagePlot <- renderPlot({
          stage_out
        })
        
        
        plot_exp_reactive(exp_out)
        plot_age_reactive(age_out)
        plot_gender_reactive(gender_out)
        plot_stage_reactive(stage_out)
      })
      
      output$downloadExpressionPlot <- downloadHandler(
        filename = function() {
          paste("gene_exp_plot-", gsub(" ", "", input$type), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_exp_reactive())
          dev.off()
        }
      )
      
      output$downloadAgePlot <- downloadHandler(
        filename = function() {
          paste("gene_age_plot-", gsub(" ", "", input$type), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_age_reactive())
          dev.off()
        }
      )
      
      output$downloadGenderPlot <- downloadHandler(
        filename = function() {
          paste("gene_gender_plot-", gsub(" ", "", input$type), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_gender_reactive())
          dev.off()
        }
      )
      
      output$downloadStagePlot <- downloadHandler(
        filename = function() {
          paste("gene_stage_plot-", gsub(" ", "", input$type), "_", input$gene,".png", sep = "")
        },
        content = function(file) {
          png(file)
          print(plot_stage_reactive())
          dev.off()
        }
      )
      
    }
  )
}
