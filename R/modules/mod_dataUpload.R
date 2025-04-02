# R/modules/mod_dataUpload.R

mod_dataUpload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("datafile"), "Upload Dataset (CSV only)", accept = c(".csv", ".pdf")),
    helpText("If no file is uploaded, a default simulated dataset will be used.")
  )
}

mod_dataUpload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
            showNotification("Dataset must contain a column 'picked_up_any'.", type = "error")
          }
          df$picked_up_any <- factor(df$picked_up_any)
          return(df)
        } else {
          showNotification("Only CSV files are supported.", type = "error")
          return(NULL)
        }
      }
    })
    return(dataset)
  })
}
