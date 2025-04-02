# R/modules/mod_rocPlot.R

# ROC Plot Module

mod_rocPlot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("roc_plot"))
  )
}

mod_rocPlot_server <- function(id, dataset, modelResults) {
  moduleServer(id, function(input, output, session) {
    # Compute ROC curves
    roc_curves <- reactive({
      req(modelResults())
      req(dataset())
      
      models <- modelResults()$models
      predictions <- modelResults()$predictions
      
      if(length(models) == 0 || length(predictions) == 0) {
        return(NULL)
      }
      
      # Extract test data
      df <- dataset()
      
      # Check if the outcome column exists and is properly formatted
      if(!"picked_up_any" %in% names(df)) {
        return(NULL)
      }
      
      set.seed(123)
      train_idx <- createDataPartition(df$picked_up_any, p = 0.8, list = FALSE)
      test_data <- df[-train_idx, ]
      
      # Create ROC curves
      roc_list <- list()
      for(model_name in names(predictions)) {
        pred <- predictions[[model_name]]
        tryCatch({
          roc_obj <- roc(test_data$picked_up_any, pred$prob, levels = c("No", "Yes"))
          roc_list[[model_name]] <- roc_obj
        }, error = function(e) {
          # Skip this model if there's an error
        })
      }
      
      roc_list
    })
    
    output$roc_plot <- renderPlot({
      roc_list <- roc_curves()
      
      if(is.null(roc_list) || length(roc_list) == 0) {
        # Create an empty plot with a message
        plot(0, 0, type = "n", xlab = "", ylab = "", 
             xlim = c(0, 1), ylim = c(0, 1), 
             main = "No ROC curves available")
        text(0.5, 0.5, "Train models to see ROC curves")
        return()
      }
      
      # Plot all ROC curves
      plot(roc_list[[1]], col = 1, lwd = 2, 
           main = "ROC Curves", 
           xlab = "False Positive Rate", 
           ylab = "True Positive Rate")
      
      if(length(roc_list) > 1) {
        for(i in 2:length(roc_list)) {
          plot(roc_list[[i]], col = i, lwd = 2, add = TRUE)
        }
      }
      
      # Add legend
      legend("bottomright", 
             legend = names(roc_list), 
             col = 1:length(roc_list), 
             lwd = 2)
    })
  })
}
