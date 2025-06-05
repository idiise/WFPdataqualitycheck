# This is the main app file to keep the app working in inst/app directory
# for both development and when installed as a package

source("ui.R")
source("server.R")

shinyApp(ui = shinyUI, server = shinyServer)