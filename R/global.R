# R/global.R

# Load required libraries
library(shiny)
library(caret)
library(MASS)
library(lme4)
library(randomForest)
library(xgboost)
library(glmnet)
library(pROC)

# Source helper functions and modules
source("helpers.R")
source("modules/mod_dataUpload.R")
source("modules/mod_preprocessing.R")
source("modules/mod_modelTraining.R")
source("modules/mod_performance.R")
source("modules/mod_rocPlot.R")

# Compose the UI by combining modules
ui <- fluidPage(
  titlePanel("Binary Outcome Analytics Pipeline (With Extended Metrics)"),
  sidebarLayout(
    sidebarPanel(
      mod_dataUpload_ui("dataUpload"),
      mod_preprocessing_ui("preproc"),
      mod_modelTraining_ui("modelTrain")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Summary", mod_performance_ui("perf")),
        tabPanel("ROC Curves", mod_rocPlot_ui("roc"))
      )
    )
  )
)

# Compose the server by calling modules and passing reactive values between them.
server <- function(input, output, session) {
  
  # Data Upload Module
  dataset <- mod_dataUpload_server("dataUpload")
  
  # Preprocessing Module
  preproc_data <- mod_preprocessing_server("preproc", data = dataset)
  
  # Model Training Module (returns a list with the trained model and performance metrics)
  modelResults <- mod_modelTraining_server("modelTrain", preproc_data = preproc_data)
  
  # Performance Module: display the metrics computed in the model training module.
  mod_performance_server("perf", modelResults = modelResults, testData = reactive({ preproc_data()$test }))
  
  # ROC Plot Module: re-compute the ROC curve from the test data using the trained model.
  mod_rocPlot_server("roc", testData = reactive({ preproc_data()$test }),
                     model = reactive({ modelResults()$model }))
}
