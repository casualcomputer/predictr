# R/helpers.R

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

compute_auc <- function(actual, probs, positive="Yes") {
  roc_obj <- roc(actual, probs, levels = rev(levels(actual)))
  return(auc(roc_obj))
}

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
