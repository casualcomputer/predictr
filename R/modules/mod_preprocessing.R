# R/modules/mod_preprocessing.R

mod_preprocessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("cv_folds"), "Number of CV Folds (k):", value = 5, min = 2),
    selectInput(ns("imputation_method"), "Missing Value Imputation Method:",
                choices = c("None" = "none",
                            "Median" = "median",
                            "KNN" = "knn"),
                selected = "none"),
    checkboxInput(ns("xgb_impute"), "Apply numeric imputation to XGBoost as well?", value = FALSE)
  )
}

mod_preprocessing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    preprocessed <- reactive({
      # Make sure data exists
      req(data())
      
      # Validate that the outcome column exists
      df <- data()
      if(!"picked_up_any" %in% names(df)) {
        showNotification("Dataset is missing the 'picked_up_any' column. Data preprocessing failed.", type = "error")
        return(NULL)
      }
      
      # Ensure the outcome is a factor with proper levels
      if(!is.factor(df$picked_up_any)) {
        if(is.numeric(df$picked_up_any)) {
          df$picked_up_any <- factor(ifelse(df$picked_up_any == 1, "Yes", "No"), levels = c("No", "Yes"))
        } else {
          df$picked_up_any <- factor(df$picked_up_any, levels = c("No", "Yes"))
        }
      } else if(!all(c("No", "Yes") %in% levels(df$picked_up_any))) {
        # Make sure the factor has the correct levels
        df$picked_up_any <- factor(df$picked_up_any, levels = c("No", "Yes"))
      }
      
      # Create train/test split
      set.seed(123)
      tryCatch({
        train_idx <- createDataPartition(df$picked_up_any, p = 0.8, list = FALSE)
        train_data <- df[train_idx, ]
        test_data <- df[-train_idx, ]
        
        # Apply imputation
        imp_result <- impute_data(train_data, test_data, method = input$imputation_method)
        
        return(list(
          train = imp_result$train_data,
          test = imp_result$test_data,
          cv_folds = input$cv_folds,
          xgb_impute = input$xgb_impute,
          imputation_method = input$imputation_method
        ))
      }, error = function(e) {
        showNotification(paste("Error in preprocessing:", e$message), type = "error")
        return(NULL)
      })
    })
    
    return(preprocessed)
  })
}
