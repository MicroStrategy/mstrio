#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames

updateColumnName <- function(df_name, prev_name, new_name) {
    new_names <- names(get(df_name, mstrio_temp_env))
    new_names[new_names == prev_name] <- new_name
    assign(df_name, stats::setNames(get(df_name, mstrio_temp_env), new_names), mstrio_temp_env)
}

reorderColumns <- function(df_name, cols_for_reorder, start_index) {
    cols <- jsonlite::fromJSON(cols_for_reorder)
    df <- get(df_name, mstrio_temp_env)
    instr <- c((start_index):(length(cols)+(start_index-1)))
    names(instr) <- cols
    assign(df_name, arrange.col(df, instr), mstrio_temp_env)
}

applyDataModeling <- function(steps, selected_objects) {
    tryCatch({
        clearTemporaryEnv();
        parsed_steps <- jsonlite::parse_json(steps)
        parsed_sel_objs <- jsonlite::parse_json(selected_objects)
        for(step in parsed_steps) {
            if(step$type == 'RENAME_DF') {
                renameDataframe(step$oldName, step$newName)
            } else if (step$type == 'RENAME_OBJ') {
                updateColumnName(step$dfName, step$oldName, step$newName)
            }
        }
        for(selected_df in parsed_sel_objs) {
            cropDataframe(selected_df$dfName, selected_df$selectedObjects)
        }
        finishDataModeling(1)
    },
    error = function(e){
        print(e$message)
        finishDataModeling(0)
    });
}

renameDataframe <- function(oldName, newName) {
    oldDf <- getDfFromTempEnv(oldName)
    assign(
        x =  newName,
        value = oldDf,
        envir = mstrio_temp_env
    )
    remove(list = c(oldName), envir = mstrio_temp_env)
}

clearTemporaryEnv <- function() {
    rm(list=ls(all.names=TRUE, envir=mstrio_temp_env), envir = mstrio_temp_env)
}

cloneDataframe <- function(dataframeToClone) {
  originalDataframe <- mstrio_env[[dataframeToClone]]
  assign(
    x =  dataframeToClone,
    value = originalDataframe,
    envir = mstrio_temp_env
  )
}

cropDataframe <- function(df_name, selected_objects) {
    df <- getDfFromTempEnv(df_name)
    if(length(selected_objects) == 1) {
      croppedDf <- data.frame(df[selected_objects[[1]]])
      names(croppedDf) <- c(selected_objects[[1]])
    } else {
      croppedDf <- data.frame(df[,unlist(selected_objects)])
      names(croppedDf) <- unlist(selected_objects)
    }
    assign(
        x =  df_name,
        value = croppedDf,
        envir = mstrio_temp_env
    )
}

getListOfDataframes <- function(envir) {
    unlisted <- unlist(eapply(mstrio_temp_env, function(x) is.data.frame(x) & nrow(x) > 0))
    names <- names(which(unlisted))
    names
}

getDfFromTempEnv <- function(dfName) {
    existsInTempEnv <- !is.null(mstrio_temp_env[[dfName]])
    if(!existsInTempEnv) {
        cloneDataframe(dfName)
    }
    df <- mstrio_temp_env[[dfName]]
    df
}

updateCube <- function(base_url, project_id, identity_token, cube_id, cube_name, update_policies) {
  tryCatch({
    displayUpdateLoadingMessage(cube_name)
    connection <- mstrio::Connection$new(base_url, project_id=project_id, identity_token=identity_token, verbose=FALSE)
    dataset <- Dataset$new(connection, dataset_id=cube_id)
    parsed_update_policies <- jsonlite::fromJSON(update_policies)
    for(i in 1:nrow(parsed_update_policies)) {
        table_name = parsed_update_policies[i,]$tableName
        update_policy = parsed_update_policies[i,]$updatePolicy
        df <- getDfFromTempEnv(table_name)
        dataset$add_table(table_name, df, update_policy)
    }
    dataset$update(auto_publish=FALSE)
    displayPublishLoadingMessage(cube_name)
    dataset$publish()
    clearTemporaryEnv()
    finishCubeUpdate(1, cube_name)
  },
  error = function(error){
    print(error$message)
    finishCubeUpdate(0, cube_name)
  });
}

exportDataframes <- function (base_url, project_id, identity_token, save_as_name, description, selected_dataframes_json, folder_id, certify) {
  displayExportStartMessage(save_as_name);
  tryCatch({
    connection <- mstrio::Connection$new(base_url, project_id=project_id, identity_token=identity_token, verbose=FALSE)

    new_dataset <- mstrio::Dataset$new(connection, save_as_name, description);

    selected_dataframes <- jsonlite::fromJSON(selected_dataframes_json)
    for(i in 1:nrow(selected_dataframes)) {
    df_name = selected_dataframes[i, 'name']
      df <- getDfFromTempEnv(df_name)
      metrics = unlist(selected_dataframes[i, 'metrics'])
      attributes = unlist(selected_dataframes[i, 'attributes'])
      new_dataset$add_table(df_name, df, "add", metrics, attributes)
    }

    new_dataset$create(folder_id)

    if(certify) {
      new_dataset$certify();
    }
    reloadCurrentFolder();
    displayExportSuccessMessage(save_as_name)
  }, error = function(error){
    print(error$message)
    if(stringIntersects('Cannot overwrite a non-cube report with a cube report', error$message)){
      displayErrorMessage('RreportOverwriteError', error$message)
    }
    else if(stringIntersects('The object with the given identifier is not an object of the expected type', error$message)){
      displayErrorMessage('RexportUnexpectedObjectTypeError', error$message)
    }
    else {
      displayErrorMessage('RexportError', error$message)
    }
  });
}
