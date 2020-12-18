#' @importFrom shinyjs runjs
#' @importFrom jsonlite toJSON
#' @importFrom rstudioapi versionInfo
#' @importFrom stats na.omit
#' @importFrom utils head packageDescription

displayFetchLoadingMessage <- function(offset,total) {
  if(is.numeric(offset) && is.numeric(total)) {
    msg = paste0("backendManager.displayFetchLoadingMessage(",offset,",",total,");")
    shinyjs::runjs(msg);
  }
}

displayFetchStartMessage <- function(name, type) {
  msg = paste0("backendManager.displayFetchStartMessage('",name,"','",type,"');")
  shinyjs::runjs(msg);
}

displayFetchSuccessMessage <- function(datasetType, datasetName) {
  msg = paste0("backendManager.displayFetchSuccessMessage('",datasetType,"','",datasetName,"');");
  shinyjs::runjs(msg);
}

displayExportStartMessage <- function(datasetName) {
  msg = paste0("backendManager.displayExportStartMessage('",datasetName,"');")
  shinyjs::runjs(msg);
}

displayExportSuccessMessage <- function(datasetName) {
  msg = paste0("backendManager.displayExportSuccessMessage('",datasetName,"');")
  shinyjs::runjs(msg);
}

displayErrorMessage <- function(error_type, error_message = '') {
  parsed_error_message <- parse_error_message(error_message);
  msg = paste0("backendManager.displayErrorMessage('",error_type,"', '",parsed_error_message,"');");
  shinyjs::runjs(msg);
  toggleImportOrExportProcess(0);
}

reloadCurrentFolder <- function() {
  cmd <- "folderContent.loadFolder(true)"
  shinyjs::runjs(cmd)
}

toggleImportOrExportProcess <- function(state) {
  cmd <- paste0("backendManager.toggleImportOrExportInProgress(",state,");");
  shinyjs::runjs(cmd);
}

sendEnvInfosToGui <- function() {
  lines <- loadLinesFromFile('environments.txt')
  if(length(lines)>0) {
    for(i in 1:length(lines)){
      cmd <- paste0("backendManager.addEnvToSuggestions('",lines[i],"');")
      shinyjs::runjs(cmd)
    }
  }
}

sendRecentProjectsToGui <- function() {
  lines <- loadLinesFromFile('recentProjects.txt')
  if(length(lines)>0) {
    for(i in 1:length(lines)){
      cmd <- paste0("backendManager.addRecentProjects('",lines[i],"');")
      shinyjs::runjs(cmd)
    }
  }
}

sendDataframesToGui <- function() {
  unlisted <- unlist(eapply(mstrio_env, function(x) is_exactly_data_frame(x) & nrow(x) > 0))
  cmd <- 'backendManager.updateDataFramesList("[]");'
  if(!is.null(unlisted)){
    name <- names(which(unlisted))
    rows <- unlist(lapply(name, function(x) nrow(mstrio_env[[x]])), use.names=FALSE)
    columns <- unlist(lapply(name, function(x) ncol(mstrio_env[[x]])), use.names=FALSE)
    df <- data.frame(name,rows,columns)
    json <- jsonlite::toJSON(df)
    cmd <- paste0("backendManager.updateDataFramesList('",json,"');")
  }
  shinyjs::runjs(cmd)
}

sendDataframesFullDetailsToGui <- function(dataframe_org_name, dataframe_new_name = dataframe_org_name, max_rows = 10) {
  content <- utils::head(na.omit(get(dataframe_org_name, mstrio_env)), n=max_rows)
  if (nrow(content) < 1) {content <- utils::head(get(dataframe_org_name, mstrio_env), n=max_rows)}
  argForModel <- list(list("table_name" = "selected_df", "data_frame" = content))
  model <- Model$new(tables = argForModel, name = "preview_table_types")
  content[] <- lapply(content, function(x) gsub("\r?\n|\r", " (ENTER) ", x)) # remove line breaks from each cell, if exists, for Preview Table display
  toJSONify <- list("content" = content, "types" = model$get_model()$raw, "originalName" = dataframe_org_name)
  json <- jsonlite::toJSON(toJSONify)
  json <- gsub('\\\\', '\\\\\\\\', json) # double backslashes
  json <- gsub("`", "\\\\`", json) # escape backticks
  cmd <- paste0("backendManager.updateDataFrameContent(`",json,"`, '",dataframe_new_name,"', true);")
  shinyjs::runjs(cmd)
}

sendInformationAboutBackendToGui <- function() {
  parameters = list();
  parameters['RVersion'] <- R.version$version.string;
  parameters['RStudioVersion'] <- toString(rstudioapi::versionInfo()$version);
  parameters <- jsonlite::toJSON(parameters, auto_unbox = TRUE)
  cmd <- paste0("backendManager.updateBackendParameters('",parameters,"');")
  shinyjs::runjs(cmd)
}

sendPackageVersionToGui <- function() {
  version = utils::packageDescription('mstrio')['Version'];
  cmd <- paste0("backendManager.updatePackageVersionNumber('",version,"');")
  shinyjs::runjs(cmd)
}

finishDataModeling <- function(result, error_message = '') {
  parsed_error_message <- parse_error_message(error_message);
  cmd <- paste0("backendManager.finishDataModeling(",result,", '",parsed_error_message,"');")
  shinyjs::runjs(cmd)
}

finishCubeUpdate <- function(result, dataset_name, error_message = '') {
  parsed_error_message <- parse_error_message(error_message);
  cmd <- paste0("backendManager.finishCubeUpdate(",result,", '",dataset_name,"', '", parsed_error_message,"');")
  shinyjs::runjs(cmd)
}

displayUpdateLoadingMessage <- function(dataset_name) {
  cmd <- paste0("backendManager.displayUpdateLoadingMessage('",dataset_name,"');")
  shinyjs::runjs(cmd)
}

displayPublishLoadingMessage <- function(dataset_name) {
  cmd <- paste0("backendManager.displayPublishLoadingMessage('",dataset_name,"');")
  shinyjs::runjs(cmd)
}

displayGeneratedCode <- function(code) {
  rstudioapi::documentNew(code)
}
