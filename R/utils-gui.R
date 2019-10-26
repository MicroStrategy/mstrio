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

displayFetchSuccessMessage <- function(datasetType, datasetName, time) {
  msg = paste0("backendManager.displayFetchSuccessMessage('",datasetType,"','",datasetName,"',",time,");")
  shinyjs::runjs(msg);
}

displayExportStartMessage <- function(datasetName) {
  msg = paste0("backendManager.displayExportStartMessage('",datasetName,"');")
  shinyjs::runjs(msg);
}

displayExportSuccessMessage <- function(datasetName, time) {
  msg = paste0("backendManager.displayExportSuccessMessage('",datasetName,"',",time,");")
  shinyjs::runjs(msg);
}

displayErrorMessage <- function(text) {
  msg = paste0("backendManager.displayErrorMessage('",text,"');")
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

sendDatasetPropertiesToGui <- function() {
  lines <- loadLinesFromFile('datasetProperties.txt')
    if(length(lines)>0) {
    for(i in 1:length(lines)){
      cmd <- paste0("backendManager.addDatasetProperties('",lines[i],"');")
      shinyjs::runjs(cmd)
    }
  }
}

sendDataframesToGui <- function() {
  unlisted <- unlist(eapply(mstrio_env, function(x) is.data.frame(x) & nrow(x) > 0))
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

sendDataframesFullDetailsToGui <- function(dataframe_name, max_rows = 10) {
  content <- utils::head(na.omit(get(dataframe_name, mstrio_env)), n=max_rows)
  if (nrow(content) < 1) {content <- utils::head(get(dataframe_name, mstrio_env), n=max_rows)}
  argForModel <- list(list("table_name" = "selected_df", "data_frame" = content))
  model <- Model$new(tables = argForModel, name = "preview_table_types")
  content[] <- lapply(content, function(x) gsub("\r?\n|\r", " (ENTER) ", x)) # remove line breaks from each cell, if exists, for Preview Table display
  toJSONify <- list("actual" = content, "types" = model$get_model()$raw)
  json <- jsonlite::toJSON(toJSONify)
  json <- gsub("`", "_", json) # remove special quotes
  cmd <- paste0("backendManager.updateDataFrameContent(`",json,"`, '",dataframe_name,"', true);")
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
