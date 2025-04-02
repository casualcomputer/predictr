# R/app.R
source("global.R")

# Run the Shiny app
shinyApp(ui = app_ui(), server = app_server)
