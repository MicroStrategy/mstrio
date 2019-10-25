#  app.R
#' @importFrom shiny htmlTemplate addResourcePath runGadget paneViewer

initialize <- function (){
  package <- "mstrio"
  basePath <- system.file("www/",package=package)
  htmlPath <- system.file("www/index.html",package=package)
  ui <- shiny::htmlTemplate(htmlPath)

  shiny::addResourcePath("static",paste0(basePath,"/static"))
  shiny::addResourcePath("shinyjs", system.file("srcjs", package = "shinyjs"))

  shiny::runGadget(app = ui, server=server, viewer = shiny::paneViewer())
}
