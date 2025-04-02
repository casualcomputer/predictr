# R/modules/mod_preprocessing.R

mod_preprocessing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("cv_folds"), "Number of CV Folds (k):", value = 5, min = 2),
    selectInput(ns("imputation_method"), "Missing Value Imputation Method:",
      choices = c(
        "None" = "none",
        "Median" = "median",
        "KNN" = "knn"
      ),
      selected = "none"
    ),
    checkboxInput(ns("xgb_impute"), "Apply numeric imputation to XGBoost as well?", value = FALSE)
  )
}

mod_preprocessing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    preprocessed <- reactive({
      req(data())
      df <- data()

      # Check if a default outcome column exists.
      # If present, use createDataPartition; otherwise, do a simple random split.
      set.seed(123)
      if ("picked_up_any" %in% names(df)) {
        train_idx <- createDataPartition(df$picked_up_any, p = 0.8, list = FALSE)
      } else {
        train_idx <- sample(seq_len(nrow(df)), size = floor(0.8 * nrow(df)))
      }
      train_data <- df[train_idx, ]
      test_data <- df[-train_idx, ]

      # Apply imputation (for numeric columns and labeling unknowns in categoricals)
      imp_result <- impute_data(train_data, test_data, method = input$imputation_method)

      list(
        train = imp_result$train_data,
        test = imp_result$test_data,
        cv_folds = input$cv_folds,
        xgb_impute = input$xgb_impute,
        imputation_method = input$imputation_method
      )
    })
    return(preprocessed)
  })
}
