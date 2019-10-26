#' @importFrom utils head type.convert

downloadDataset <- function(input) {
  displayFetchStartMessage(input$datasetName, input$datasetType);
    tryCatch({
      con <- mstrio::connect_mstr(base_url = input$envUrl , username = input$username, password = input$password, project_id=input$projectId, login_mode = as.numeric(input$loginMode))
      fetchDataset(con, input$datasetType, input$datasetId, input$datasetName, limit=25000, body=input$datasetBody)
      mstrio::close(con)
    }, error = function(e) {
      displayErrorMessage('RfetchError')
      print(e$message)
    });
}

fetchDataset <- function(conn, datasetType, datasetId, datasetName, body={}, limit=25000) {
  limit <- strtoi(limit, base = 0L)
  t1 <- unclass(Sys.time())
  if(body=='null') body <- {}

  tryCatch({
    if(datasetType=='dataset') {
      instance <- mstrio::Cube$new(conn,datasetId)
    }
    else {
      instance <- mstrio::Report$new(conn, datasetId)
    }
    instance$apply_filters(body=body)
    dataset <- instance$to_dataframe(limit=limit, callback=displayFetchLoadingMessage)
    t2 <- unclass(Sys.time())
    time <- round(t2-t1,2)
    saveDatasetToEnv(dataset,datasetName)
    type <- firstUp(datasetType)
    displayFetchSuccessMessage(type, datasetName, time)
  },
  error = function(e) {
    print(e$message)
    if(stringIntersects('is not published',e$message)){
      displayErrorMessage('RcubeNotPublishedError')
    }
    else if(stringIntersects("'data' must be of a vector type, was 'NULL'",e$message)){
      displayErrorMessage('RemptyDatasetError')
    }
    else {
      displayErrorMessage('RfetchError')
    }
  })
}

saveDatasetToEnv <- function(dataset, datasetName, applyBestGuess=TRUE){
  if (applyBestGuess) {
    appliedBestGuessTypes <- utils::type.convert(dataset, as.is = TRUE)
    dataset <- appliedBestGuessTypes
  }
  assign(
    x =  datasetName,
    value = dataset,
    envir = mstrio_env
  )
  properColumnsNames <- utils::head(get(datasetName, mstrio_env), n=1L)
  properColumnsNames[1,] <- "placeholder"
  clearJSON <- gsub('`', '_', jsonlite::toJSON(properColumnsNames))
  cmd <- paste0("window.Shiny.setInputValue('properColNames', Object.keys(JSON.parse(`",clearJSON,"`)[0]));",
                "window.Shiny.setInputValue('dataFrameToVerify', '",datasetName,"');",
                "window.Shiny.setInputValue('verifyColumnsNames', Date.now());")
  shinyjs::runjs(cmd)
  tryCatch({
    myView(x=dataset,title=datasetName)
  }, error = function(err) {

  })
}

exportDataframes <- function(con, names, folderId, saveAsName, wrangle) {
  displayExportStartMessage(saveAsName);
  tryCatch({
    t1 <- unclass(Sys.time())
    newDs <- mstrio::Dataset$new(connection=con, name=saveAsName);
    wrangle_data <- jsonlite::fromJSON(wrangle)
    for(i in 1:length(names)) {
      this.data <- tryCatch({
        get(names[i], wrangle_data)
      }, error = function(e) {
        NULL
      })
      if (is.null(this.data)) {
        to_m <- NULL
        to_a <- NULL
      } else {
        if (length(this.data$toMetrics) > 0) {to_m <- this.data$toMetrics} else {to_m <- NULL}
        if (length(this.data$toAttributes) > 0) {to_a <- this.data$toAttributes} else {to_a <- NULL}
      }
      newDs$add_table(names[i], mstrio_env[[names[i]]], "add", to_m, to_a)
    }
    newDs$create(folderId)
    t2 <- unclass(Sys.time())
    time <- round(t2-t1,2)
    displayExportSuccessMessage(saveAsName, time)
    reloadCurrentFolder();
  },
  error = function(e) {
    print(e$message)
    if(stringIntersects('Cannot overwrite a non-cube report with a cube report',e$message)){
      displayErrorMessage('RreportOverwriteError')
    }
    else {
      displayErrorMessage('RexportError')
    }
  })
}

exportDataset <- function(input) {
    tryCatch({
      con <- mstrio::connect_mstr(input$envUrl , input$username, input$password, project_id=input$projectId, login_mode = as.numeric(input$loginMode))
      exportDataframes(con=con, input$names, input$folderId, input$saveAsName, input$wrangle)
      mstrio::close(con)
    }, error = function(e) {
      displayErrorMessage('RexportError')
      print(e$message)
    });
}
