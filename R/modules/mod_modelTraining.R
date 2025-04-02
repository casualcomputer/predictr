# R/modules/mod_modelTraining.R

mod_modelTraining_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Classify Variables"),
    helpText("Select which variables belong to each category."),
    selectInput(ns("var_outcome"), "Outcome Variable", choices = NULL),
    checkboxGroupInput(ns("var_cont"), "Continuous Variables", choices = NULL),
    checkboxGroupInput(ns("var_cat"), "Categorical Variables", choices = NULL),
    checkboxGroupInput(ns("var_id"), "ID Variables (Exclude from Modeling)", choices = NULL),
    checkboxGroupInput(ns("var_exclude"), "Exclude Variables", choices = NULL),
    br(),
    h4("Models to Train"),
    checkboxGroupInput(ns("model_select"), "Select Models:",
      choices = list(
        "Logistic Regression" = "logistic",
        "Stepwise Logistic (AIC)" = "stepwise",
        "Mixed-Effects Logistic" = "mixed",
        "Ridge Regression" = "ridge",
        "XGBoost" = "xgboost",
        "Random Forest" = "rf"
      ),
      selected = c("logistic", "ridge", "xgboost", "rf")
    ),
    br(),
    actionButton(ns("train_btn"), "Train Models")
  )
}

mod_modelTraining_server <- function(id, preprocessed_data) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values to store models and predictions
    models <- reactiveVal(list())
    predictions <- reactiveVal(list())

    # Update variable classification UI based on data
    observe({
      req(preprocessed_data()$train)
      df <- preprocessed_data()$train
      vars <- names(df)

      # Update UI selectors with variables
      if ("picked_up_any" %in% vars) {
        updateSelectInput(session, "var_outcome", choices = vars, selected = "picked_up_any")
      } else {
        updateSelectInput(session, "var_outcome", choices = vars, selected = vars[1])
      }

      numeric_vars <- vars[sapply(df, is.numeric)]
      factor_vars <- vars[sapply(df, is.factor)]
      updateCheckboxGroupInput(session, "var_cont", choices = vars, selected = numeric_vars)
      updateCheckboxGroupInput(session, "var_cat", choices = vars, selected = factor_vars)
      updateCheckboxGroupInput(session, "var_id", choices = vars, selected = character(0))
      updateCheckboxGroupInput(session, "var_exclude", choices = vars, selected = character(0))
    })

    # Training process
    observeEvent(input$train_btn, {
      req(input$model_select)
      req(preprocessed_data())

      # Get data from preprocessing module
      train_data <- preprocessed_data()$train
      test_data <- preprocessed_data()$test

      # Validate data
      if (is.null(train_data) || is.null(test_data)) {
        showNotification("No data available for training", type = "error")
        return(NULL)
      }

      # Get user selected variables
      outcome_var <- input$var_outcome
      cont_vars <- input$var_cont
      cat_vars <- input$var_cat
      id_vars <- input$var_id
      exclude_vars <- input$var_exclude

      # Validate selections
      if (length(outcome_var) != 1) {
        showNotification("Please select exactly one outcome variable.", type = "error")
        return(NULL)
      }

      # Filter to only selected predictor variables
      pred_vars <- setdiff(c(cont_vars, cat_vars), c(id_vars, exclude_vars, outcome_var))
      if (length(pred_vars) == 0) {
        showNotification("No predictor variables selected. Please select at least one variable.", type = "error")
        return(NULL)
      }

      # Create model dataset with only selected variables
      model_df_train <- train_data[, c(outcome_var, pred_vars), drop = FALSE]
      model_df_test <- test_data[, c(outcome_var, pred_vars), drop = FALSE]

      # Rename outcome column to "Outcome" for consistent processing
      names(model_df_train)[names(model_df_train) == outcome_var] <- "Outcome"
      names(model_df_test)[names(model_df_test) == outcome_var] <- "Outcome"

      # Ensure outcome is properly formatted
      if (!all(c("No", "Yes") %in% levels(model_df_train$Outcome))) {
        if (is.numeric(model_df_train$Outcome)) {
          model_df_train$Outcome <- factor(ifelse(model_df_train$Outcome == 1, "Yes", "No"), levels = c("No", "Yes"))
          model_df_test$Outcome <- factor(ifelse(model_df_test$Outcome == 1, "Yes", "No"), levels = c("No", "Yes"))
        } else {
          model_df_train$Outcome <- factor(model_df_train$Outcome, levels = c("No", "Yes"))
          model_df_test$Outcome <- factor(model_df_test$Outcome, levels = c("No", "Yes"))
        }
      }

      # Apply imputation based on preprocessing settings
      method <- tolower(preprocessed_data()$imputation_method)
      if (is.null(method)) method <- "none"

      imp_list <- impute_data(model_df_train, model_df_test, method = method)
      train_data_imp <- imp_list$train_data
      test_data_imp <- imp_list$test_data

      # For XGBoost, potentially use different imputation
      do_impute_xgb <- preprocessed_data()$xgb_impute
      if ("xgboost" %in% input$model_select && !do_impute_xgb) {
        xgb_list <- impute_data(model_df_train, model_df_test, method = "none")
        train_data_xgb <- xgb_list$train_data
        test_data_xgb <- xgb_list$test_data
      } else {
        train_data_xgb <- train_data_imp
        test_data_xgb <- test_data_imp
      }

      # Ensure factor levels match between train and test
      for (col in names(train_data_imp)) {
        if (is.factor(train_data_imp[[col]])) {
          test_data_imp[[col]] <- factor(test_data_imp[[col]], levels = levels(train_data_imp[[col]]))
        }
      }
      for (col in names(train_data_xgb)) {
        if (is.factor(train_data_xgb[[col]])) {
          test_data_xgb[[col]] <- factor(test_data_xgb[[col]], levels = levels(train_data_xgb[[col]]))
        }
      }

      # All predictor variables for model formulas
      all_pred_vars <- setdiff(names(train_data_imp), "Outcome")
      if (length(all_pred_vars) == 0) {
        showNotification("No predictor variables available", type = "error")
        return(NULL)
      }

      model_list <- list()
      pred_list <- list()

      withProgress(message = "Training models", value = 0, {
        n_models <- length(input$model_select)
        i_model <- 0

        # 1. Logistic Regression
        if ("logistic" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting logistic regression")

          tryCatch(
            {
              logi_form <- buildFormula("Outcome", all_pred_vars)
              mod_final <- glm(logi_form, data = train_data_imp, family = binomial)
              model_list[["Logistic Regression"]] <- mod_final
              probs_test <- predict(mod_final, newdata = test_data_imp, type = "response")
              preds_test <- ifelse(probs_test > 0.5, "Yes", "No")
              pred_list[["Logistic Regression"]] <- list(
                class = preds_test,
                prob = probs_test
              )
            },
            error = function(e) {
              showNotification(paste("Error in logistic regression:", e$message), type = "error")
            }
          )
        }

        # 2. Stepwise Logistic Regression
        if ("stepwise" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting stepwise regression")

          tryCatch(
            {
              step_form <- buildFormula("Outcome", all_pred_vars)
              full_mod <- glm(step_form, data = train_data_imp, family = binomial)
              full_mod$call$data <- train_data_imp
              step_mod <- stepAIC(full_mod, direction = "both", trace = FALSE)
              model_list[["Stepwise Logistic"]] <- step_mod
              preds_prob <- predict(step_mod, newdata = test_data_imp, type = "response")
              preds <- ifelse(preds_prob > 0.5, "Yes", "No")
              pred_list[["Stepwise Logistic"]] <- list(
                class = preds,
                prob = preds_prob
              )
            },
            error = function(e) {
              showNotification(paste("Error in stepwise regression:", e$message), type = "error")
            }
          )
        }

        # 3. Mixed-Effects Logistic Regression
        if ("mixed" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting mixed-effects model")

          all_cat <- setdiff(names(train_data_imp)[sapply(train_data_imp, is.factor)], "Outcome")
          if (length(all_cat) > 0) {
            re_var <- all_cat[1]
            mix_form <- as.formula(paste("Outcome ~", paste(all_pred_vars, collapse = " + "), "+ (1|", re_var, ")"))
            tryCatch(
              {
                mod_mixed <- glmer(mix_form, data = train_data_imp, family = binomial)
                model_list[["Mixed-Effects Logistic"]] <- mod_mixed
                preds_prob <- predict(mod_mixed, newdata = test_data_imp, type = "response", allow.new.levels = TRUE)
                preds <- ifelse(preds_prob > 0.5, "Yes", "No")
                pred_list[["Mixed-Effects Logistic"]] <- list(
                  class = preds,
                  prob = preds_prob
                )
              },
              error = function(e) {
                showNotification(paste("Error in mixed-effects model:", e$message), type = "error")
              }
            )
          } else {
            showNotification("No categorical variables available for random effects", type = "warning")
          }
        }

        # 4. Ridge Regression
        if ("ridge" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting ridge regression")

          tryCatch(
            {
              ridge_form <- buildFormula("Outcome", all_pred_vars)
              X_train <- model.matrix(ridge_form, data = train_data_imp)[, -1, drop = FALSE]
              X_test <- model.matrix(ridge_form, data = test_data_imp)[, -1, drop = FALSE]
              y_train <- ifelse(train_data_imp$Outcome == "Yes", 1, 0)
              cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0, type.measure = "auc")
              model_list[["Ridge Regression"]] <- cv_ridge
              preds_prob <- predict(cv_ridge, newx = X_test, s = "lambda.min", type = "response")
              preds <- ifelse(preds_prob > 0.5, "Yes", "No")
              pred_list[["Ridge Regression"]] <- list(
                class = preds,
                prob = as.vector(preds_prob)
              )
            },
            error = function(e) {
              showNotification(paste("Error in ridge regression:", e$message), type = "error")
            }
          )
        }

        # 5. XGBoost
        if ("xgboost" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting XGBoost")

          tryCatch(
            {
              xgb_form <- buildFormula("Outcome", all_pred_vars)
              X_train_xgb <- model.matrix(xgb_form, data = train_data_xgb)[, -1, drop = FALSE]
              X_test_xgb <- model.matrix(xgb_form, data = test_data_xgb)[, -1, drop = FALSE]
              y_train_xgb <- ifelse(train_data_xgb$Outcome == "Yes", 1, 0)
              dtrain <- xgb.DMatrix(data = X_train_xgb, label = y_train_xgb, missing = NA)
              dtest <- xgb.DMatrix(data = X_test_xgb, missing = NA)
              params <- list(objective = "binary:logistic", eval_metric = "auc")
              xgb_mod <- xgb.train(params = params, data = dtrain, nrounds = 50, verbose = 0)
              model_list[["XGBoost"]] <- xgb_mod
              preds_prob <- predict(xgb_mod, newdata = dtest)
              preds <- ifelse(preds_prob > 0.5, "Yes", "No")
              pred_list[["XGBoost"]] <- list(
                class = preds,
                prob = preds_prob
              )
            },
            error = function(e) {
              showNotification(paste("Error in XGBoost:", e$message), type = "error")
            }
          )
        }

        # 6. Random Forest
        if ("rf" %in% input$model_select) {
          i_model <- i_model + 1
          incProgress(1 / n_models, detail = "Fitting random forest")

          tryCatch(
            {
              rf_form <- buildFormula("Outcome", all_pred_vars)
              rf_mod <- randomForest(rf_form, data = train_data_imp)
              model_list[["Random Forest"]] <- rf_mod
              preds_prob <- predict(rf_mod, newdata = test_data_imp, type = "prob")[, "Yes"]
              preds <- predict(rf_mod, newdata = test_data_imp, type = "response")
              pred_list[["Random Forest"]] <- list(
                class = preds,
                prob = preds_prob
              )
            },
            error = function(e) {
              showNotification(paste("Error in random forest:", e$message), type = "error")
            }
          )
        }
      })

      # Store the results
      models(model_list)
      predictions(pred_list)

      # Show completion message
      showNotification("Model training complete!", type = "message")
    })

    # Return reactive values wrapped in a reactive expression
    return(reactive({
      list(
        models = models(),
        predictions = predictions()
      )
    }))
  })
}
