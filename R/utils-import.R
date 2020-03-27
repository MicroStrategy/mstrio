#' @importFrom utils head type.convert

downloadDataset <- function(input) {
  displayFetchStartMessage(input$datasetName, input$datasetType);
    tryCatch({
      con <- mstrio::connect_mstr(base_url = input$envUrl , username = input$username, password = input$password, project_id=input$projectId, login_mode = as.numeric(input$loginMode))
      fetchDataset(con, input$datasetType, input$datasetId, input$datasetName, body=input$datasetBody)
      mstrio::close(con)
    }, error = function(e) {
      displayErrorMessage('RfetchError', e$message)
      print(e$message)
    });
}

fetchDataset <- function(conn, datasetType, datasetId, datasetName, body) {
  t1 <- unclass(Sys.time())

  body <- jsonlite::fromJSON(body)

  if (length(body$attributes) + length(body$metrics) + length(body$filters) == 0) {
    attr <- NULL
    metr <- NULL
    filtr <- NULL
  } else {
    attr <- body$attributes
    metr <- body$metrics
    if (length(body$filters) == 0) {
      filtr <- NULL
    } else {
      filtr <- body$filters
    }
  }

  tryCatch({
    if(datasetType=='dataset') {
      instance <- mstrio::Cube$new(conn,datasetId)
    }
    else {
      instance <- mstrio::Report$new(conn, datasetId)
    }
    instance$apply_filters(
      attributes=attr,
      metrics=metr,
      attr_elements=filtr
    )
    dataset <- instance$to_dataframe(callback=displayFetchLoadingMessage)
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

saveDatasetToEnv <- function(dataset, datasetName, applyBestGuess=FALSE){
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
