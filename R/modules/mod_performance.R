# R/modules/mod_performance.R

mod_performance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("performance_table"))
  )
}

mod_performance_server <- function(id, dataset, modelResults) {
  moduleServer(id, function(input, output, session) {
    # Compute metrics from model results
    performance_metrics <- reactive({
      req(modelResults())
      req(dataset())
      
      models <- modelResults()$models
      predictions <- modelResults()$predictions
      
      if(length(models) == 0 || length(predictions) == 0) {
        return(data.frame(Message = "No models trained yet", stringsAsFactors = FALSE))
      }
      
      # Extract test data
      df <- dataset()
      
      # Check if the outcome column exists and is properly formatted
      if(!"picked_up_any" %in% names(df)) {
        return(data.frame(Message = "Dataset missing 'picked_up_any' column", stringsAsFactors = FALSE))
      }
      
      set.seed(123)
      train_idx <- createDataPartition(df$picked_up_any, p = 0.8, list = FALSE)
      test_data <- df[-train_idx, ]
      
      # Create performance metrics
      metrics_list <- list()
      for(model_name in names(predictions)) {
        pred <- predictions[[model_name]]
        metrics <- tryCatch({
          compute_test_metrics(
            actual = test_data$picked_up_any,
            pred_class = pred$class,
            pred_prob = pred$prob,
            positive = "Yes"
          )
        }, error = function(e) {
          data.frame(
            Accuracy = NA,
            Kappa = NA,
            Sensitivity = NA,
            Specificity = NA,
            Precision = NA,
            F1 = NA,
            AUC = NA
          )
        })
        metrics_list[[model_name]] <- cbind(Model = model_name, metrics)
      }
      
      if(length(metrics_list) == 0) {
        return(data.frame(Message = "No performance metrics available", stringsAsFactors = FALSE))
      }
      
      do.call(rbind, metrics_list)
    })
    
    output$performance_table <- renderTable({
      performance_metrics()
    })
  })
}
