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
      req(data())
      set.seed(123)
      train_idx <- createDataPartition(data()$picked_up_any, p = 0.8, list = FALSE)
      train_data <- data()[train_idx, ]
      test_data <- data()[-train_idx, ]
      list(
        train = impute_data(train_data, test_data, method = input$imputation_method)$train_data,
        test  = impute_data(train_data, test_data, method = input$imputation_method)$test_data,
        cv_folds = input$cv_folds,
        xgb_impute = input$xgb_impute
      )
    })
    return(preprocessed)
  })
}
