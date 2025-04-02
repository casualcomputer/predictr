# app.R

# Install packages if not already installed
list_of_packages <- c("shiny", "glmnet", "xgboost", "randomForest")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(glmnet)
library(xgboost)
library(randomForest)

# Default simulated dataset (if no file is uploaded)
set.seed(123)
n <- 300
default_df <- data.frame(
  X1 = rnorm(n),
  X2 = rnorm(n),
  X3 = sample(letters[1:3], n, replace = TRUE)
)
default_df$X3 <- as.factor(default_df$X3)
default_df$Class <- ifelse(default_df$X1 + default_df$X2 + rnorm(n) > 0, "Good", "Bad")
default_df$Class <- factor(default_df$Class, levels = c("Bad", "Good"))  # "Bad" as positive

# Custom function to compute AUC using the rank-sum method
compute_auc <- function(actual, predicted, positive = "Bad") {
  ord <- order(predicted)
  actual <- actual[ord]
  n_pos <- sum(actual == positive)
  n_neg <- sum(actual != positive)
  if(n_pos == 0 || n_neg == 0) return(NA)
  ranks <- rank(predicted)
  rank_sum <- sum(ranks[actual == positive])
  auc <- (rank_sum - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
  return(auc)
}

# UI
ui <- fluidPage(
  titlePanel("Cross-Validated Model Comparison"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Dataset (CSV file only, PDF not supported)", 
                accept = c(".csv", ".pdf")),
      numericInput("k_folds", "Number of folds (k):", value = 5, min = 2),
      uiOutput("predictor_ui"),
      actionButton("train_btn", "Train All Models")
    ),
    mainPanel(
      h3("Performance Comparison"),
      tableOutput("performance_table"),
      h3("Detailed Model Summaries"),
      verbatimTextOutput("detailed_output")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to load the dataset
  dataset <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      # No file uploaded; use default dataset
      return(default_df)
    } else {
      ext <- tools::file_ext(file$datapath)
      if(tolower(ext) == "csv") {
        df <- read.csv(file$datapath, stringsAsFactors = TRUE)
        # Check if "Class" exists; if not, throw an error message.
        if(!"Class" %in% names(df)){
          showNotification("Uploaded dataset must have a column named 'Class'. Using default dataset.", type = "error")
          return(default_df)
        }
        # Ensure outcome is factor with "Bad" as first level
        df$Class <- factor(df$Class)
        if(!("Bad" %in% levels(df$Class))) {
          showNotification("Outcome variable 'Class' should contain 'Bad' as one of its levels.", type = "error")
        }
        return(df)
      } else {
        showNotification("Only CSV files are supported. Using default dataset.", type = "error")
        return(default_df)
      }
    }
  })
  
  # Dynamically generate predictor selection UI (exclude outcome "Class")
  output$predictor_ui <- renderUI({
    df <- dataset()
    choices <- setdiff(names(df), "Class")
    checkboxGroupInput("predictors", "Select Predictors:", 
                       choices = choices, 
                       selected = choices)
  })
  
  # Main training and cross-validation process
  observeEvent(input$train_btn, {
    req(input$predictors)
    df <- dataset()
    
    # Ensure outcome variable "Class" is present
    if(!"Class" %in% names(df)) {
      showNotification("Dataset does not contain a 'Class' column.", type = "error")
      return(NULL)
    }
    
    # Use only the selected predictors and the outcome variable
    selected_vars <- c(input$predictors, "Class")
    df <- df[, selected_vars, drop = FALSE]
    
    # Ensure outcome is a factor with levels ordered as: "Bad", "Good"
    df$Class <- factor(df$Class)
    if(!("Bad" %in% levels(df$Class))) {
      showNotification("Outcome variable 'Class' should contain 'Bad' as one of its levels.", type = "error")
      return(NULL)
    }
    # Reorder levels so that "Bad" is first (positive class)
    df$Class <- factor(df$Class, levels = c("Bad", setdiff(levels(df$Class), "Bad")))
    
    k <- input$k_folds
    n <- nrow(df)
    # Create k-fold indices
    set.seed(123)
    folds <- sample(rep(1:k, length.out = n))
    
    # List to store average performance for each model
    model_names <- c("Logistic", "Ridge", "XGBoost", "Random Forest")
    acc_results <- numeric(length(model_names))
    auc_results <- numeric(length(model_names))
    
    # For detailed summaries (we concatenate printed outputs)
    detail_text <- ""
    
    # Loop over model types
    for (model_idx in seq_along(model_names)) {
      model_type <- tolower(model_names[model_idx])
      acc_vec <- c()
      auc_vec <- c()
      
      # For collecting model details from the first fold (as example)
      fold_details <- ""
      
      for (fold in 1:k) {
        train_data <- df[folds != fold, , drop = FALSE]
        test_data  <- df[folds == fold, , drop = FALSE]
        # Build formula
        formula_str <- paste("Class ~", paste(input$predictors, collapse = " + "))
        formula <- as.formula(formula_str)
        
        pred_probs <- NULL
        pred_class <- NULL
        
        if(model_type == "logistic") {
          # Logistic regression using glm (base R)
          model_obj <- glm(formula, data = train_data, family = binomial)
          # predict() returns probability for level 2 ("Good") so probability for "Bad" is 1 - that
          probs_good <- predict(model_obj, newdata = test_data, type = "response")
          pred_probs <- 1 - probs_good
          pred_class <- ifelse(pred_probs > 0.5, "Bad", "Good")
          # Save summary from first fold
          if(fold == 1) fold_details <- paste(fold_details, "Logistic Regression Summary:\n", 
                                              capture.output(summary(model_obj)), "\n\n")
          
        } else if(model_type == "ridge") {
          # Ridge regression using glmnet (alpha = 0)
          X_train <- model.matrix(formula, data = train_data)[, -1, drop = FALSE]
          X_test <- model.matrix(formula, data = test_data)[, -1, drop = FALSE]
          y_train <- ifelse(train_data$Class == "Bad", 1, 0)
          cv_model <- cv.glmnet(X_train, y_train, alpha = 0, family = "binomial")
          probs <- predict(cv_model, newx = X_test, type = "response", s = "lambda.min")
          pred_probs <- as.vector(probs)
          pred_class <- ifelse(pred_probs > 0.5, "Bad", "Good")
          if(fold == 1) fold_details <- paste(fold_details, "Ridge Regression (glmnet) Details:\n", 
                                              capture.output(cv_model), "\n\n")
          
        } else if(model_type == "xgboost") {
          # XGBoost requires numeric matrices and binary labels (1 = "Bad", 0 = "Good")
          X_train <- model.matrix(formula, data = train_data)[, -1, drop = FALSE]
          X_test <- model.matrix(formula, data = test_data)[, -1, drop = FALSE]
          y_train <- ifelse(train_data$Class == "Bad", 1, 0)
          dtrain <- xgb.DMatrix(data = X_train, label = y_train)
          dtest <- xgb.DMatrix(data = X_test)
          params <- list(
            objective = "binary:logistic",
            eval_metric = "auc"
          )
          xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 50, verbose = 0)
          pred_probs <- predict(xgb_model, newdata = dtest)
          pred_class <- ifelse(pred_probs > 0.5, "Bad", "Good")
          if(fold == 1) fold_details <- paste(fold_details, "XGBoost Model Details:\n", 
                                              capture.output(xgb_model), "\n\n")
          
        } else if(model_type == "random forest") {
          # Random Forest using randomForest package
          rf_model <- randomForest(formula, data = train_data)
          pred_probs_mat <- predict(rf_model, newdata = test_data, type = "prob")
          pred_probs <- pred_probs_mat[, "Bad"]
          pred_class <- predict(rf_model, newdata = test_data, type = "response")
          if(fold == 1) fold_details <- paste(fold_details, "Random Forest Model Details:\n", 
                                              capture.output(rf_model), "\n\n")
        }
        
        # Compute accuracy and AUC for this fold
        test_actual <- test_data$Class
        fold_accuracy <- mean(test_actual == pred_class)
        fold_auc <- compute_auc(test_actual, pred_probs, positive = "Bad")
        
        acc_vec <- c(acc_vec, fold_accuracy)
        auc_vec <- c(auc_vec, fold_auc)
      }  # end fold loop
      
      # Average metrics over folds
      avg_acc <- mean(acc_vec, na.rm = TRUE)
      avg_auc <- mean(auc_vec, na.rm = TRUE)
      acc_results[model_idx] <- avg_acc
      auc_results[model_idx] <- avg_auc
      
      detail_text <- paste(detail_text, "====", model_names[model_idx], "====\n",
                           fold_details,
                           "Average Accuracy:", round(avg_acc, 3), "\n",
                           "Average AUC:", round(avg_auc, 3), "\n\n")
    }  # end model loop
    
    # Create a summary table
    performance_df <- data.frame(
      Model = model_names,
      Accuracy = round(acc_results, 3),
      AUC = round(auc_results, 3)
    )
    
    output$performance_table <- renderTable({
      performance_df
    })
    
    output$detailed_output <- renderPrint({
      cat(detail_text)
    })
  })
}

shinyApp(ui, server)
