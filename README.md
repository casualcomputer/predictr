# Binary Outcome Analytics Pipeline

A modular Shiny application for training and evaluating various binary classification models.

## Features

- Data upload with support for CSV files
- Automatic variable classification (continuous, categorical, ID)
- Multiple imputation methods for handling missing values
- Support for various classification models:
  - Logistic Regression
  - Stepwise Logistic Regression (AIC)
  - Mixed-Effects Logistic Regression
  - Ridge Regression
  - XGBoost
  - Random Forest
- Comprehensive model evaluation metrics:
  - Accuracy
  - Kappa
  - Sensitivity
  - Specificity
  - Precision
  - F1 Score
  - AUC
- ROC curve visualization
- Detailed model summaries

## Project Structure

```
├── R/
│   ├── app.R           # Main launcher script
│   ├── global.R        # Global settings, libraries, and source() calls
│   ├── helpers.R       # Helper functions (e.g. imputation, performance metrics)
│   └── modules/        # Folder for Shiny modules
│       ├── mod_dataUpload.R        # Module for data upload
│       ├── mod_preprocessing.R     # Module for data preprocessing & imputation
│       ├── mod_modelTraining.R     # Module for variable selection and model training
│       ├── mod_performance.R       # Module for showing performance metrics
│       └── mod_rocPlot.R           # Module for plotting ROC curves
├── tests/
│   └── testthat/       # Folder for unit tests
│       ├── test_helpers.R        # Tests for helper functions
│       └── test_modules.R        # Tests for module logic (if desired)
└── README.md           # Overview and instructions
```

## Installation

1. Clone this repository
2. Install R and required packages:
   ```R
   install.packages(c("shiny", "caret", "MASS", "lme4",
                     "randomForest", "xgboost", "glmnet", "pROC"))
   ```

## Usage

1. Run the application:

   ```R
   shiny::runApp("R")
   ```

2. Upload your dataset (CSV format) or use the default simulated dataset
3. Classify your variables
4. Select models to train
5. View performance metrics and ROC curves

## Data Requirements

- CSV file format
- Binary outcome variable named "picked_up_any" with levels "Yes" and "No"
- Missing values are handled automatically
- Categorical variables should be factors or character strings

## Testing

Run the test suite:

```R
devtools::test()
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
