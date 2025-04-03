# # R/modules/mod_dataUpload.R
# 
# mod_dataUpload_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     fileInput(ns("datafile"), "Upload Dataset (CSV only)", accept = c(".csv")),
#     helpText("If no file is uploaded, a default simulated dataset will be used.")
#   )
# }
# 
# mod_dataUpload_server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # Function to create default dataset
#     create_default_dataset <- function() {
#       set.seed(123)
#       n <- 300
#       df <- data.frame(
#         age = round(rnorm(n, mean = 40, sd = 12)),
#         Income = round(rnorm(n, mean = 50000, sd = 15000)),
#         Province = sample(c("A", "B", "C"), n, replace = TRUE),
#         NaicsTwoDigit = sample(1:5, n, replace = TRUE),
#         days_request_to_call1 = sample(1:10, n, replace = TRUE),
#         days_call1_to_call2 = sample(1:10, n, replace = TRUE),
#         days_call2_to_call3 = sample(1:10, n, replace = TRUE)
#       )
#       df$picked_up_any <- ifelse(df$age + df$Income/10000 + rnorm(n) > 45, "Yes", "No")
#       df$picked_up_any <- factor(df$picked_up_any, levels = c("No", "Yes"))
#       return(df)
#     }
#     
#     dataset <- reactive({
#       file <- input$datafile
#       if (is.null(file)) {
#         return(create_default_dataset())
#       } else {
#         ext <- tools::file_ext(file$datapath)
#         if(tolower(ext) == "csv") {
#           tryCatch({
#             df <- read.csv(file$datapath, stringsAsFactors = FALSE)
#             df <- as.data.frame(lapply(df, function(x) {
#               if(is.character(x)) as.factor(x) else x
#             }))
#             if(!"picked_up_any" %in% names(df)) {
#               showNotification("Dataset must contain a column 'picked_up_any'. Using default dataset.", type = "error")
#               return(create_default_dataset())
#             }
#             df$picked_up_any <- factor(df$picked_up_any, levels = c("No", "Yes"))
#             return(df)
#           }, error = function(e) {
#             showNotification(paste("Error reading file:", e$message, ". Using default dataset."), type = "error")
#             return(create_default_dataset())
#           })
#         } else {
#           showNotification("Only CSV files are supported. Using default dataset.", type = "error")
#           return(create_default_dataset())
#         }
#       }
#     })
#     return(dataset)
#   })
# }

mod_dataUpload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("datafile"), "Upload Dataset (CSV only)", accept = c(".csv")),
    helpText("If no file is uploaded, a default simulated dataset will be used.")
  )
}

mod_dataUpload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Function to create a default dataset
    create_default_dataset <- function() {
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
    }
    
    dataset <- reactive({
      file <- input$datafile
      if (is.null(file)) {
        return(create_default_dataset())
      } else {
        ext <- tools::file_ext(file$datapath)
        if (tolower(ext) == "csv") {
          tryCatch({
            df <- read.csv(file$datapath, stringsAsFactors = FALSE)
            # Convert character columns to factors
            df <- as.data.frame(lapply(df, function(x) {
              if (is.character(x)) as.factor(x) else x
            }))
            return(df)
          }, error = function(e) {
            showNotification(paste("Error reading file:", e$message, ". Using default dataset."), type = "error")
            return(create_default_dataset())
          })
        } else {
          showNotification("Only CSV files are supported. Using default dataset.", type = "error")
          return(create_default_dataset())
        }
      }
    })
    
    return(dataset)
  })
}

