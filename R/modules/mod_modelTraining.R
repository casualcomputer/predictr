# R/modules/mod_modelTraining.R

mod_modelTraining_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("var_outcome"), "Outcome Variable", choices = NULL),
    checkboxGroupInput(ns("var_cont"), "Continuous Variables", choices = NULL),
    checkboxGroupInput(ns("var_cat"), "Categorical Variables", choices = NULL),
    checkboxGroupInput(ns("var_id"), "ID Variables (Exclude from Modeling)", choices = NULL),
    checkboxGroupInput(ns("var_exclude"), "Exclude Variables", choices = NULL),
    checkboxGroupInput(ns("model_select"), "Select Models:",
                       choices = list("Logistic Regression" = "logistic",
                                      "Stepwise Logistic (AIC)" = "stepwise",
                                      "Mixed-Effects Logistic" = "mixed",
                                      "Ridge Regression" = "ridge",
                                      "XGBoost" = "xgboost",
                                      "Random Forest" = "rf"),
                       selected = c("logistic", "ridge", "xgboost", "rf")),
    actionButton(ns("train_btn"), "Train Models")
  )
}

mod_modelTraining_server <- function(id, preproc_data) {
  moduleServer(id, function(input, output, session) {
    
    # Update variable classification UI based on training data.
    observe({
      req(preproc_data()$train)
      df <- preproc_data()$train
      vars <- names(df)
      updateSelectInput(session, "var_outcome", choices = vars, selected = "picked_up_any")
      numeric_vars <- vars[sapply(df, is.numeric)]
      factor_vars <- vars[sapply(df, is.factor)]
      updateCheckboxGroupInput(session, "var_cont", choices = vars, selected = numeric_vars)
      updateCheckboxGroupInput(session, "var_cat", choices = vars, selected = factor_vars)
      updateCheckboxGroupInput(session, "var_id", choices = vars, selected = character(0))
      updateCheckboxGroupInput(session, "var_exclude", choices = vars, selected = character(0))
    })
    
    modelResults <- reactive({
      req(preproc_data()$train)
      df <- preproc_data()$train
      outcome_var <- input$var_outcome
      cont_vars <- input$var_cont
      cat_vars <- input$var_cat
      id_vars <- input$var_id
      exclude_vars <- input$var_exclude
      pred_vars <- setdiff(c(cont_vars, cat_vars), c(id_vars, exclude_vars))
      if(length(outcome_var) != 1) {
        showNotification("Select exactly one outcome variable", type = "error")
        return(NULL)
      }
      model_df <- df[, c(outcome_var, pred_vars), drop = FALSE]
      names(model_df)[1] <- "Outcome"
      if(!all(c("No", "Yes") %in% levels(model_df$Outcome))) {
        if(is.numeric(model_df$Outcome)) {
          model_df$Outcome <- factor(ifelse(model_df$Outcome==1, "Yes", "No"), levels=c("No","Yes"))
        } else {
          showNotification("Outcome must have two classes 'No' and 'Yes'", type = "error")
          return(NULL)
        }
      }
      # For demonstration, train a logistic regression model.
      all_pred_vars <- setdiff(names(df), "Outcome")
      logi_form <- buildFormula("Outcome", all_pred_vars)
      mod_final <- glm(logi_form, data = df, family = binomial)
      probs_test <- predict(mod_final, newdata = preproc_data()$test, type = "response")
      preds_test <- ifelse(probs_test > 0.5, "Yes", "No")
      met <- compute_test_metrics(actual = preproc_data()$test$Outcome,
                                  pred_class = preds_test,
                                  pred_prob = probs_test,
                                  positive = "Yes")
      list(model = mod_final, metrics = met)
    })
    
    return(modelResults)
  })
}
