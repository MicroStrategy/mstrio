#' @importFrom utils head type.convert

importDataset <- function (base_url, project_id, identity_token, dataset_id, original_name, save_as_name, dataset_type, raw_body, instance_id) {
  displayFetchStartMessage(original_name, dataset_type);
  tryCatch({
    connection <- mstrio::Connection$new(base_url = base_url, identity_token = identity_token, project_id = project_id, verbose=FALSE)

    body <- jsonlite::fromJSON(raw_body)
    if (length(body$attributes) + length(body$metrics) + length(body$filters) == 0) {
      attributes <- NULL
      metrics <- NULL
      filters <- NULL
    } else {
      attributes <- body$attributes
      metrics <- body$metrics
      if (length(body$filters) == 0) {
        filters <- NULL
      } else {
        filters <- body$filters
      }
    }

    if(dataset_type=='dataset') {
      instance <- mstrio::Cube$new(connection, dataset_id, instance_id)
    }
    else {
      instance <- mstrio::Report$new(connection, dataset_id, instance_id)
    }

    instance$apply_filters(attributes, metrics, filters)
    dataset <- instance$to_dataframe(callback=displayFetchLoadingMessage)
    saveDatasetToEnv(dataset, save_as_name)
    type <- firstUp(dataset_type)
    displayFetchSuccessMessage(type, original_name)
  },
  error = function(error) {
    print(error$message)
    if(stringIntersects('is not published', error$message)){
      displayErrorMessage('RcubeNotPublishedError')
    }
    else if(stringIntersects("'data' must be of a vector type, was 'NULL'", error$message)){
      displayErrorMessage('RemptyDatasetError')
    }
    else {
      displayErrorMessage('RfetchError', error$message)
    }
  });
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
  }, error = function(error) {
    print(error$message)
  })
}
