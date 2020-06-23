# server.R

#' @importFrom shinyjs useShinyjs
#' @importFrom shiny stopApp observeEvent

server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)

  sendEnvInfosToGui();
  sendPackageVersionToGui();
  sendRecentProjectsToGui();
  sendInformationAboutBackendToGui();

  shiny::observeEvent(input$triggerImport, {
    importDataset(input$baseUrl, input$projectId, input$identityToken,
                  input$datasetId, input$originalName, input$saveAsName, input$datasetType,
                  input$datasetBody, input$instanceId);
  })

  shiny::observeEvent(input$triggerExportDataframes, {
    exportDataframes(input$baseUrl, input$projectId, input$identityToken, input$saveAsName,
                    input$description, input$selectedDataframes, input$folderId, input$certify)
  })

    shiny::observeEvent(input$triggerCubeUpdate, {
    updateCube(input$baseUrl, input$projectId, input$identityToken, input$cubeId, input$cubeName, input$updatePolicies)
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

  shiny::observeEvent(input$triggerGenerateCode, {
    displayGeneratedCode(input$generatedCode)
  })

  shiny::observeEvent(input$msgToConsole, {
    print(input$msgToConsole)
  })

  shiny::observeEvent(input$triggerClose, {
    shiny::stopApp();
  })
}
