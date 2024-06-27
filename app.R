library(shiny)
library(shinydashboard)
library(TCGAplot)
library(tidyverse)

setwd("~/Maxi/s100_exp_correlation")

# Load module definitions
source("gene_expression_tab.R")
source("coexpression_tab.R")
source("pan_cancer_tab.R")
source("geneset_cor_tab.R")
source("by_cancer_tab.R")
source("deg_tab.R")

# Define cancer types
cancertypes <- c("ACC",  "BLCA", "BRCA", "CESC", "CHOL", "COAD", "DLBC", 
                 "ESCA", "GBM",  "HNSC", "KICH", "KIRC", "KIRP", "LAML", 
                 "LGG",  "LIHC", "LUAD", "LUSC", "MESO", "OV",  "PAAD", 
                 "PCPG", "PRAD", "READ", "SARC", "SKCM", "STAD", "TGCT", 
                 "THCA", "THYM", "UCEC", "UCS",  "UVM")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "TCGA Gene Expression"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pan-Cancer Gene Expression", icon = icon("search"), tabName = "input"),
      menuItem("Gene List Coexpression", icon = icon("list"), tabName = "coexpression"),
      menuItem("Feature Correlation", icon = icon("up-right-and-down-left-from-center"), tabName = "pan_cancer"),
      menuItem("Geneset Correlation", icon = icon("th"), tabName = "geneset_cor"),
      menuItem("Cancer type", icon = icon("disease"), tabName = "by_cancer"),
      menuItem("Differential expressed genes", icon = icon("up-down"), tabName = "deg")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input", gene_expression_ui("gene_expression")),
      tabItem(tabName = "coexpression", coexpression_ui("coexpression")),
      tabItem(tabName = "pan_cancer", pan_cancer_ui("pan_cancer")),
      tabItem(tabName = "geneset_cor", geneset_cor_ui("geneset_cor")),
      tabItem(tabName = "by_cancer", by_cancer_ui("by_cancer")),
      tabItem(tabName = "deg", deg_ui("deg"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  gene_expression_server("gene_expression")
  coexpression_server("coexpression")
  pan_cancer_server("pan_cancer")
  geneset_cor_server("geneset_cor")
  by_cancer_server("by_cancer")
  deg_server("deg")
}

# Run the application
shinyApp(ui, server)
