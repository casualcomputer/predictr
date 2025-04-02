# R/modules/mod_performance.R

mod_performance_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tableOutput(ns("performance_table"))
  )
}

mod_performance_server <- function(id, modelResults, testData) {
  moduleServer(id, function(input, output, session) {
    output$performance_table <- renderTable({
      req(modelResults())
      modelResults()$metrics
    })
  })
}
