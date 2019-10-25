# server.R

#' @importFrom shinyjs useShinyjs
#' @importFrom shiny stopApp observeEvent

server <- function(input, output, session) {
  shinyjs::useShinyjs(html = TRUE)

  sendEnvInfosToGui();
  sendPackageVersionToGui();
  sendRecentProjectsToGui();
  sendDatasetPropertiesToGui();
  sendInformationAboutBackendToGui();

  shiny::observeEvent(input$triggerDatasetInfo, {
    downloadDataset(input);
  })

  shiny::observeEvent(input$verifyColumnsNames, {
    verifyColumnsNames(input$dataFrameToVerify, input$properColNames)
  })

  shiny::observeEvent(input$triggerExportDataframe, {
    exportDataset(input);
  })

  shiny::observeEvent(input$onExportDataMode,{
    sendDataframesToGui();
  })

  shiny::observeEvent(input$onExportGatherDetails,{
    sendDataframesFullDetailsToGui(input$dataframeToGather);
  })

  shiny::observeEvent(input$onWrangleColNameChange,{
    updateColumnName(input$dataframeForColNameChange, input$previousColName, input$newColName)
  })

  shiny::observeEvent(input$onWrangleReorderRequest,{
    reorderColumns(input$dataframeForColReorder, input$columnsForReorder, input$reorderStartIndex)
  })

  shiny::observeEvent(input$newEnvSuggestions, {
    updateEnvSuggestions(input$newEnvSuggestions)
  })

  shiny::observeEvent(input$newRecentProjects, {
    updateRecentProjects(input$newRecentProjects)
  })

  shiny::observeEvent(input$updateDatasetProperties, {
    updateDatasetProperties(input$updateDatasetProperties)
  })

  shiny::observeEvent(input$msgToConsole, {
    print(input$msgToConsole)
  })

  shiny::observeEvent(input$triggerClose, {
    shiny::stopApp();
  })
}
