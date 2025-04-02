# R/app.R
source("global.R")

shinyApp(ui = app_ui(), server = app_server)
