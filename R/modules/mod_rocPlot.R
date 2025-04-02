# R/modules/mod_rocPlot.R

mod_rocPlot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("roc_plot"))
  )
}

mod_rocPlot_server <- function(id, testData, model) {
  moduleServer(id, function(input, output, session) {
    output$roc_plot <- renderPlot({
      req(testData())
      req(model())
      probs <- predict(model(), newdata = testData(), type = "response")
      roc_obj <- roc(testData()$Outcome, probs, levels = c("No", "Yes"))
      plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
    })
  })
}
