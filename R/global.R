# R/global.R

# List of required packages
list_of_packages <- c("shiny", "caret", "MASS", "lme4", 
                      "randomForest", "xgboost", "glmnet", "pROC")

# Install missing packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load required packages
library(shiny)
library(caret)         # for confusionMatrix, cross-validation, preProcess, createDataPartition
library(MASS)          # for stepAIC (stepwise regression)
library(lme4)          # for mixed-effects logistic regression
library(randomForest)  # for random forest
library(xgboost)       # for gradient boosting
library(glmnet)        # for ridge regression
library(pROC)          # for ROC curves and AUC

# Source helper functions
source("helpers.R")

# Source modules
source("modules/mod_dataUpload.R")
source("modules/mod_preprocessing.R")
source("modules/mod_modelTraining.R")
source("modules/mod_performance.R")
source("modules/mod_rocPlot.R")

# Define the overall UI and server functions by composing modules
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
          tabPanel("Performance", mod_performance_ui("perf")),
          tabPanel("ROC Curve", mod_rocPlot_ui("rocPlot"))
        )
      )
    )
  )
}

app_server <- function(input, output, session) {
  # Data Upload Module
  dataset <- mod_dataUpload_server("dataUpload")
  
  # Preprocessing Module (e.g., CV folds, imputation options)
  preproc_data <- mod_preprocessing_server("preproc", dataset)
  
  # Model Training Module (variable selection, model training)
  modelResults <- mod_modelTraining_server("modelTrain", preproc_data)
  
  # Performance and ROC modules (display results)
  mod_performance_server("perf", dataset, modelResults)
  mod_rocPlot_server("rocPlot", dataset, modelResults)
}