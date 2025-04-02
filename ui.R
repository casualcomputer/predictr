# app.R

# 1) Install required packages if not already installed
list_of_packages <- c("shiny", "caret", "MASS", "lme4", 
                      "randomForest", "xgboost", "glmnet", "pROC")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(shiny)
library(caret)         # for confusionMatrix, cross-validation, preProcess, createDataPartition
library(MASS)          # for stepAIC (stepwise regression)
library(lme4)          # for mixed-effects logistic regression
library(randomForest)  # for random forest
library(xgboost)       # for gradient boosting
library(glmnet)        # for ridge regression
library(pROC)          # for ROC curves and AUC

# --------------------------------------------------------------------------------
# Helper Functions
# --------------------------------------------------------------------------------

# Label unknowns in categorical variables (if any missing)
label_unknown <- function(df) {
  cat_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  for (var in cat_vars) {
    if (any(is.na(df[[var]]))) {
      if (is.character(df[[var]])) {
        df[[var]] <- as.factor(df[[var]])
      }
      df[[var]] <- addNA(df[[var]])
      levels(df[[var]])[is.na(levels(df[[var]]))] <- "Unknown"
      df[[var]][is.na(df[[var]])] <- "Unknown"
    }
  }
  return(df)
}

# A wrapper to apply caret pre-processing for numeric imputation.
# method can be "none", "median", or "knn". 
impute_data <- function(train_data, test_data, method = "none") {
  train_data <- label_unknown(train_data)
  test_data  <- label_unknown(test_data)
  
  if(tolower(method) == "none") {
    return(list(train_data = train_data, test_data = test_data))
  }
  
  pp_method <- if(tolower(method) == "median") {
    "medianImpute"
  } else if(tolower(method) == "knn") {
    "knnImpute"
  } else {
    NULL
  }
  
  if(is.null(pp_method)) return(list(train_data = train_data, test_data = test_data))
  
  num_vars <- names(train_data)[sapply(train_data, is.numeric)]
  if(length(num_vars) == 0) return(list(train_data = train_data, test_data = test_data))
  
  preproc <- preProcess(train_data[, num_vars, drop=FALSE], method = pp_method)
  train_data[, num_vars] <- predict(preproc, train_data[, num_vars, drop=FALSE])
  test_data[, num_vars]  <- predict(preproc, test_data[, num_vars, drop=FALSE])
  
  list(train_data = train_data, test_data = test_data)
}

# Compute AUC using pROC
compute_auc <- function(actual, probs, positive="Yes") {
  roc_obj <- roc(actual, probs, levels = rev(levels(actual)))
  return(auc(roc_obj))
}

# Create a summary of performance metrics on the test set.
compute_test_metrics <- function(actual, pred_class, pred_prob, positive="Yes") {
  cm <- confusionMatrix(
    data = factor(pred_class, levels = levels(actual)),
    reference = actual,
    positive = positive
  )
  auc_val <- compute_auc(actual, pred_prob, positive=positive)
  data.frame(
    Accuracy    = round(cm$overall["Accuracy"], 3),
    Kappa       = round(cm$overall["Kappa"], 3),
    Sensitivity = round(cm$byClass["Sensitivity"], 3),
    Specificity = round(cm$byClass["Specificity"], 3),
    Precision   = round(cm$byClass["Pos Pred Value"], 3),
    F1          = round(cm$byClass["F1"], 3),
    AUC         = round(auc_val, 3)
  )
}

buildFormula <- function(y, xvars) {
  as.formula(paste(y, "~", paste(xvars, collapse=" + ")))
}

# --------------------------------------------------------------------------------
# UI
# --------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Binary Outcome Analytics Pipeline (With Extended Metrics)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload Dataset (CSV only)", accept = c(".csv", ".pdf")),
      helpText("If no file is uploaded, a default simulated dataset will be used."),
      numericInput("cv_folds", "Number of CV Folds (k):", value = 5, min = 2),
      selectInput("imputation_method", "Missing Value Imputation Method:",
                  choices = c("None" = "none",
                              "Median" = "median",
                              "KNN" = "knn"),
                  selected = "none"),
      checkboxInput("xgb_impute", "Apply numeric imputation to XGBoost as well?", value = FALSE),
      br(),
      h4("Classify Variables"),
      helpText("Select which variables belong to each category."),
      selectInput("var_outcome", "Outcome Variable", choices = NULL),
      checkboxGroupInput("var_cont", "Continuous Variables", choices = NULL),
      checkboxGroupInput("var_cat", "Categorical Variables", choices = NULL),
      checkboxGroupInput("var_id", "ID Variables (Exclude from Modeling)", choices = NULL),
      checkboxGroupInput("var_exclude", "Exclude Variables", choices = NULL),
      br(),
      h4("Models to Train"),
      checkboxGroupInput("model_select", "Select Models:",
                         choices = list("Logistic Regression" = "logistic",
                                        "Stepwise Logistic (AIC)" = "stepwise",
                                        "Mixed-Effects Logistic" = "mixed",
                                        "Ridge Regression" = "ridge",
                                        "XGBoost" = "xgboost",
                                        "Random Forest" = "rf"),
                         selected = c("logistic", "ridge", "xgboost", "rf")),
      br(),
      actionButton("train_btn", "Train Models")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Performance Summary", tableOutput("performance_table")),
        tabPanel("Model Details", verbatimTextOutput("model_details")),
        tabPanel("ROC Curves", plotOutput("roc_plot"))
      )
    )
  )
)

# --------------------------------------------------------------------------------
# Server
# --------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive: load dataset or create default
  dataset <- reactive({
    file <- input$datafile
    if (is.null(file)) {
      set.seed(123)
      n <- 300
      df <- data.frame(
        age = round(rnorm(n, mean = 40, sd = 12)),
        Income = round(rnorm(n, mean = 50000, sd = 15000)),
        Province = sample(c("A", "B", "C"), n, replace = TRUE),
        NaicsTwoDigit = sample(1:5, n, replace = TRUE),
        days_request_to_call1 = sample(1:10, n, replace = TRUE),
        days_call1_to_call2 = sample(1:10, n, replace = TRUE),
        days_call2_to_call3 = sample(1:10, n, replace = TRUE)
      )
      df$picked_up_any <- ifelse(df$age + df$Income/10000 + rnorm(n) > 45, "Yes", "No")
      df$picked_up_any <- factor(df$picked_up_any, levels = c("No", "Yes"))
      return(df)
    } else {
      ext <- tools::file_ext(file$datapath)
      if(tolower(ext) == "csv") {
        df <- read.csv(file$datapath, stringsAsFactors = FALSE)
        df <- as.data.frame(lapply(df, function(x) {
          if(is.character(x)) as.factor(x) else x
        }))
        if(!"picked_up_any" %in% names(df)) {
          showNotification("Dataset must contain a column 'picked_up_any'. Using default dataset.", type = "error")
          return(isolate(dataset()))
        } else {
          df$picked_up_any <- factor(df$picked_up_any)
          return(df)
        }
      } else {
        showNotification("Only CSV files are supported. Using default dataset.", type = "error")
        return(isolate(dataset()))
      }
    }
  })
  
  # Update UI for variable classification
  observeEvent(dataset(), {
    df <- dataset()
    vars <- names(df)
    if("picked_up_any" %in% vars){
      updateSelectInput(session, "var_outcome", choices = vars, selected = "picked_up_any")
    } else {
      updateSelectInput(session, "var_outcome", choices = vars, selected = vars[1])
    }
    numeric_vars <- vars[sapply(df, is.numeric)]
    factor_vars  <- vars[sapply(df, is.factor)]
    updateCheckboxGroupInput(session, "var_cont", choices = vars, selected = numeric_vars)
    updateCheckboxGroupInput(session, "var_cat", choices = vars, selected = factor_vars)
    updateCheckboxGroupInput(session, "var_id", choices = vars, selected = character(0))
    updateCheckboxGroupInput(session, "var_exclude", choices = vars, selected = character(0))
  })
  
  # Main training process
  observeEvent(input$train_btn, {
    req(input$model_select)
    df <- dataset()
    
    outcome_var  <- input$var_outcome
    cont_vars    <- input$var_cont
    cat_vars     <- input$var_cat
    id_vars      <- input$var_id
    exclude_vars <- input$var_exclude
    
    pred_vars <- setdiff(c(cont_vars, cat_vars), c(id_vars, exclude_vars))
    
    if(length(outcome_var) != 1) {
      showNotification("Please select exactly one outcome variable.", type = "error")
      return(NULL)
    }
    if(!outcome_var %in% names(df)) {
      showNotification("Selected outcome variable not in dataset.", type = "error")
      return(NULL)
    }
    
    model_df <- df[, c(outcome_var, pred_vars), drop = FALSE]
    names(model_df)[1] <- "Outcome"
    
    if(!all(c("No","Yes") %in% levels(model_df$Outcome))) {
      if(is.numeric(model_df$Outcome)) {
        model_df$Outcome <- factor(ifelse(model_df$Outcome==1, "Yes", "No"), levels=c("No","Yes"))
      } else {
        showNotification("Outcome must have two classes labeled 'No' and 'Yes'.", type = "error")
        return(NULL)
      }
    }
    
    set.seed(123)
    train_idx <- createDataPartition(model_df$Outcome, p = 0.8, list = FALSE)
    train_data <- model_df[train_idx, ]
    test_data  <- model_df[-train_idx, ]
    
    method <- tolower(input$imputation_method)
    do_impute_xgb <- input$xgb_impute
    
    imp_list <- impute_data(train_data, test_data, method=method)
    train_data_imp <- imp_list$train_data
    test_data_imp  <- imp_list$test_data
    
    if("xgboost" %in% input$model_select && !do_impute_xgb) {
      xgb_list <- impute_data(train_data, test_data, method="none")
      train_data_xgb <- xgb_list$train_data
      test_data_xgb  <- xgb_list$test_data
    } else {
      train_data_xgb <- train_data_imp
      test_data_xgb  <- test_data_imp
    }
    
    for(col in names(train_data_imp)) {
      if(is.factor(train_data_imp[[col]])) {
        test_data_imp[[col]] <- factor(test_data_imp[[col]], levels=levels(train_data_imp[[col]]))
      }
    }
    for(col in names(train_data_xgb)) {
      if(is.factor(train_data_xgb[[col]])) {
        test_data_xgb[[col]] <- factor(test_data_xgb[[col]], levels=levels(train_data_xgb[[col]]))
      }
    }
    
    performance <- data.frame(
      Model       = character(),
      Accuracy    = numeric(),
      Kappa       = numeric(),
      Sensitivity = numeric(),
      Specificity = numeric(),
      Precision   = numeric(),
      F1          = numeric(),
      AUC         = numeric(),
      stringsAsFactors = FALSE
    )
    
    details <- list()
    roc_list <- list()
    
    all_pred_vars <- setdiff(names(train_data_imp), "Outcome")
    
    # 1. Logistic Regression
    if("logistic" %in% input$model_select) {
      logi_form <- buildFormula("Outcome", all_pred_vars)
      mod_final <- glm(logi_form, data=train_data_imp, family=binomial)
      details[["Logistic Regression"]] <- paste(
        "Logistic Regression (full training set) Summary:\n",
        paste(capture.output(summary(mod_final)), collapse="\n"),
        "\n"
      )
      probs_test <- predict(mod_final, newdata=test_data_imp, type="response")
      preds_test <- ifelse(probs_test > 0.5, "Yes", "No")
      met <- compute_test_metrics(actual=test_data_imp$Outcome,
                                  pred_class=preds_test,
                                  pred_prob=probs_test,
                                  positive="Yes")
      performance <- rbind(performance, cbind(Model="Logistic Regression", met))
      roc_list[["Logistic Regression"]] <- roc(test_data_imp$Outcome, probs_test, levels=c("No","Yes"))
    }
    
    # 2. Stepwise Logistic Regression
    if("stepwise" %in% input$model_select) {
      step_form <- buildFormula("Outcome", all_pred_vars)
      full_mod <- glm(step_form, data=train_data_imp, family=binomial)
      # Fix: Update the stored call so that it embeds the actual training data
      full_mod$call$data <- train_data_imp
      step_mod <- stepAIC(full_mod, direction="both", trace=FALSE)
      details[["Stepwise Logistic"]] <- paste(
        "Stepwise Logistic Regression Summary:\n",
        paste(capture.output(summary(step_mod)), collapse="\n"),
        "\n"
      )
      preds_prob <- predict(step_mod, newdata=test_data_imp, type="response")
      preds <- ifelse(preds_prob > 0.5, "Yes", "No")
      met <- compute_test_metrics(actual=test_data_imp$Outcome,
                                  pred_class=preds,
                                  pred_prob=preds_prob,
                                  positive="Yes")
      performance <- rbind(performance, cbind(Model="Stepwise Logistic", met))
      roc_list[["Stepwise Logistic"]] <- roc(test_data_imp$Outcome, preds_prob, levels=c("No","Yes"))
    }
    
    # 3. Mixed-Effects Logistic Regression
    if("mixed" %in% input$model_select) {
      all_cat <- setdiff(names(train_data_imp)[sapply(train_data_imp, is.factor)], "Outcome")
      if(length(all_cat) == 0) {
        showNotification("No categorical variable for random intercept; skipping mixed model.", type="error")
      } else {
        re_var <- all_cat[1]
        mix_form <- as.formula(paste("Outcome ~", paste(all_pred_vars, collapse=" + "), "+ (1|", re_var, ")"))
        mod_mixed <- tryCatch({
          glmer(mix_form, data=train_data_imp, family=binomial)
        }, error=function(e) NULL)
        if(is.null(mod_mixed)) {
          showNotification("Error fitting mixed-effects model; skipping.", type="error")
        } else {
          details[["Mixed-Effects Logistic"]] <- paste(
            "Mixed-Effects Logistic Regression Summary:\n",
            paste(capture.output(summary(mod_mixed)), collapse="\n"),
            "\n"
          )
          preds_prob <- predict(mod_mixed, newdata=test_data_imp, type="response", allow.new.levels=TRUE)
          preds <- ifelse(preds_prob > 0.5, "Yes", "No")
          met <- compute_test_metrics(actual=test_data_imp$Outcome,
                                      pred_class=preds,
                                      pred_prob=preds_prob,
                                      positive="Yes")
          performance <- rbind(performance, cbind(Model="Mixed-Effects Logistic", met))
          roc_list[["Mixed-Effects Logistic"]] <- roc(test_data_imp$Outcome, preds_prob, levels=c("No","Yes"))
        }
      }
    }
    
    # 4. Ridge Regression
    if("ridge" %in% input$model_select) {
      ridge_form <- buildFormula("Outcome", all_pred_vars)
      X_train <- model.matrix(ridge_form, data=train_data_imp)[,-1, drop=FALSE]
      X_test  <- model.matrix(ridge_form, data=test_data_imp)[,-1, drop=FALSE]
      y_train <- ifelse(train_data_imp$Outcome=="Yes", 1, 0)
      cv_ridge <- cv.glmnet(X_train, y_train, family="binomial", alpha=0, type.measure="auc")
      details[["Ridge Regression"]] <- paste(
        "Ridge Regression (cv.glmnet) Details:\n",
        paste(capture.output(cv_ridge), collapse="\n"),
        "\n"
      )
      preds_prob <- predict(cv_ridge, newx=X_test, s="lambda.min", type="response")
      preds <- ifelse(preds_prob > 0.5, "Yes", "No")
      met <- compute_test_metrics(actual=test_data_imp$Outcome,
                                  pred_class=preds,
                                  pred_prob=as.vector(preds_prob),
                                  positive="Yes")
      performance <- rbind(performance, cbind(Model="Ridge Regression", met))
      roc_list[["Ridge Regression"]] <- roc(test_data_imp$Outcome, as.vector(preds_prob), levels=c("No","Yes"))
    }
    
    # 5. XGBoost
    if("xgboost" %in% input$model_select) {
      xgb_form <- buildFormula("Outcome", all_pred_vars)
      X_train_xgb <- model.matrix(xgb_form, data=train_data_xgb)[,-1, drop=FALSE]
      X_test_xgb  <- model.matrix(xgb_form, data=test_data_xgb)[,-1, drop=FALSE]
      y_train_xgb <- ifelse(train_data_xgb$Outcome=="Yes", 1, 0)
      dtrain <- xgb.DMatrix(data=X_train_xgb, label=y_train_xgb, missing=NA)
      dtest  <- xgb.DMatrix(data=X_test_xgb, missing=NA)
      params <- list(objective="binary:logistic", eval_metric="auc")
      xgb_mod <- xgb.train(params=params, data=dtrain, nrounds=50, verbose=0)
      details[["XGBoost"]] <- paste(
        "XGBoost Model Details:\n",
        paste(capture.output(xgb_mod), collapse="\n"),
        "\n"
      )
      preds_prob <- predict(xgb_mod, newdata=dtest)
      preds <- ifelse(preds_prob > 0.5, "Yes", "No")
      met <- compute_test_metrics(actual=test_data_xgb$Outcome,
                                  pred_class=preds,
                                  pred_prob=preds_prob,
                                  positive="Yes")
      performance <- rbind(performance, cbind(Model="XGBoost", met))
      roc_list[["XGBoost"]] <- roc(test_data_xgb$Outcome, preds_prob, levels=c("No","Yes"))
    }
    
    # 6. Random Forest
    if("rf" %in% input$model_select) {
      rf_form <- buildFormula("Outcome", all_pred_vars)
      rf_mod <- randomForest(rf_form, data=train_data_imp)
      details[["Random Forest"]] <- paste(
        "Random Forest Model Details:\n",
        paste(capture.output(rf_mod), collapse="\n"),
        "\n"
      )
      preds_prob <- predict(rf_mod, newdata=test_data_imp, type="prob")[, "Yes"]
      preds <- predict(rf_mod, newdata=test_data_imp, type="response")
      met <- compute_test_metrics(actual=test_data_imp$Outcome,
                                  pred_class=preds,
                                  pred_prob=preds_prob,
                                  positive="Yes")
      performance <- rbind(performance, cbind(Model="Random Forest", met))
      roc_list[["Random Forest"]] <- roc(test_data_imp$Outcome, preds_prob, levels=c("No","Yes"))
    }
    
    output$performance_table <- renderTable({ performance })
    output$model_details <- renderPrint({
      for(nm in names(details)){
        cat("====================================\n")
        cat(nm, "\n")
        cat("====================================\n")
        cat(details[[nm]], "\n\n")
      }
    })
    output$roc_plot <- renderPlot({
      if(length(roc_list) == 0) return(NULL)
      model_names <- names(roc_list)
      plot(roc_list[[1]], col=1, lwd=2, main="ROC Curves (Test Set)")
      if(length(roc_list) > 1){
        for(i in 2:length(roc_list)){
          plot(roc_list[[i]], col=i, lwd=2, add=TRUE)
        }
      }
      legend("bottomright", legend=model_names, col=1:length(roc_list), lwd=2)
    })
    
  })
}

# --------------------------------------------------------------------------------
# Run the Shiny App
# --------------------------------------------------------------------------------
shinyApp(ui, server)
