# R/global.R

# List of required packages and installation (omitted for brevity)
# Load required libraries and source helpers and modules
library(shiny)
library(caret)
library(MASS)
library(lme4)
library(randomForest)
library(xgboost)
library(glmnet)
library(pROC)
library(summarytools)

source("helpers.R")
source("modules/mod_dataUpload.R")
source("modules/mod_preprocessing.R")
source("modules/mod_modelTraining.R")
source("modules/mod_performance.R")
source("modules/mod_rocPlot.R")
source("modules/mod_summary.R")

# Compose overall UI:
app_ui <- function() {
  fluidPage(
    titlePanel("Modular Binary Outcome Analytics Pipeline"),
    sidebarLayout(
      sidebarPanel(
        mod_dataUpload_ui("dataUpload"),
        mod_preprocessing_ui("preproc"),
        mod_modelTraining_ui("modelTrain")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Summary", mod_summary_ui("summary")),
          tabPanel("Performance", mod_performance_ui("perf")),
          tabPanel("ROC Curve", mod_rocPlot_ui("rocPlot"))
        )
      )
    )
  )
}

# Compose overall server:
app_server <- function(input, output, session) {
  # Data Upload Module returns a reactive dataset
  dataset <- mod_dataUpload_server("dataUpload")
  
  # Preprocessing Module (e.g., CV folds, imputation options)
  preproc_data <- mod_preprocessing_server("preproc", dataset)
  
  # Model Training Module (variable selection, model training)
  modelResults <- mod_modelTraining_server("modelTrain", preproc_data)
  
  # Performance and ROC modules (display results)
  mod_performance_server("perf", dataset, modelResults)
  mod_rocPlot_server("rocPlot", dataset, modelResults)
  
  # Summary Module uses the reactive dataset directly
  mod_summary_server("summary", data = dataset)
}
