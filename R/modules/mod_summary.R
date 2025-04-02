# R/modules/mod_summary.R

mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("profileSummary"))
  )
}

mod_summary_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$profileSummary <- renderUI({
      req(data())
      # Generate a dfSummary object from the uploaded dataset
      summary_obj <- summarytools::dfSummary(data(), plain.ascii = FALSE, style = "grid")
      # Capture the printed output into a character vector and collapse it into a single string
      html_output <- paste(capture.output(print(summary_obj, method = "render", headings = FALSE)),
                           collapse = "\n")
      HTML(html_output)
    })
  })
}
