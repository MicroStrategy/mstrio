# server.R

#' @importFrom shinyjs useShinyjs
#' @importFrom shiny stopApp observeEvent

server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)

  sendEnvInfosToGui();
  sendPackageVersionToGui();
  sendRecentProjectsToGui();
  sendInformationAboutBackendToGui();

  shiny::observeEvent(input$triggerFetchDataset, {
    downloadDataset(input);
  })

  shiny::observeEvent(input$verifyColumnsNames, {
    verifyColumnsNames(input$dataFrameToVerify, input$properColNames)
  })

  shiny::observeEvent(input$triggerExportDataframes, {
    exportDataset(input);
  })

  shiny::observeEvent(input$onExportDataMode,{
    sendDataframesToGui();
  })

  shiny::observeEvent(input$onExportGatherDetails,{
    sendDataframesFullDetailsToGui(input$dataframeToGather, input$dataframeNewName);
  })

  shiny::observeEvent(input$newEnvSuggestions, {
    updateEnvSuggestions(input$newEnvSuggestions)
  })

  shiny::observeEvent(input$newRecentProjects, {
    updateRecentProjects(input$newRecentProjects)
  })

  shiny::observeEvent(input$triggerDataModelingSteps, {
    applyDataModeling(input$dataModelingSteps, input$selectedObjects)
  })

  shiny::observeEvent(input$triggerCubeUpdate, {
    updateCube(input)
  })

  shiny::observeEvent(input$generatedCode, {
    displayGeneratedCode(input$generatedCode)
  })

  shiny::observeEvent(input$msgToConsole, {
    print(input$msgToConsole)
  })

  shiny::observeEvent(input$triggerClose, {
    shiny::stopApp();
  })
}
