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

updateCube <- function(input) {
  tryCatch({
    displayUpdateLoadingMessage(input$datasetName)
    con <- mstrio::connect_mstr(base_url = input$envUrl , username = input$username, password = input$password, project_id=input$projectId, login_mode = as.numeric(input$loginMode))
    dataset <- Dataset$new(con, dataset_id=input$cubeId)
    parsed_update_policies <- jsonlite::fromJSON(input$updatePolicies)
    for(i in 1:nrow(parsed_update_policies)) {
        tableName = parsed_update_policies[i,]$tableName
        updatePolicy = parsed_update_policies[i,]$updatePolicy
        df <- getDfFromTempEnv(tableName)
        dataset$add_table(tableName, df, updatePolicy)
    }
    dataset$update()
    displayPublishLoadingMessage(input$datasetName)
    dataset$publish()
    clearTemporaryEnv()
    finishCubeUpdate(1, input$datasetName)
  },
  error = function(e){
    print(e$message)
    finishCubeUpdate(0, input$datasetName)
  });
}

exportDataset <- function(input) {
    displayExportStartMessage(input$saveAsName);
    tryCatch({
      con <- mstrio::connect_mstr(input$envUrl , input$username, input$password, project_id=input$projectId, login_mode = as.numeric(input$loginMode))
      exportDataframes(con=con, input$selectedDataframes, input$folderId, input$saveAsName, input$certify, input$description)
      mstrio::close(con)
    }, error = function(e) {
      displayErrorMessage('RexportError', e$message)
      print(e$message)
    });
}

exportDataframes <- function(con, selectedDataframesJSON, folderId, saveAsName, certify, description) {
  tryCatch({
    t1 <- unclass(Sys.time())
    newDs <- mstrio::Dataset$new(connection=con, name=saveAsName, description=description);
    selectedDataframes<- jsonlite::fromJSON(selectedDataframesJSON)
    for(i in 1:nrow(selectedDataframes)) {
      df_name = selectedDataframes[i, 'name']
      df <- getDfFromTempEnv(df_name)
      metrics = unlist(selectedDataframes[i, 'metrics'])
      attributes = unlist(selectedDataframes[i, 'attributes'])
      newDs$add_table(df_name, df, "add", metrics, attributes)
    }
    newDs$create(folderId)
    if(certify) {
      newDs$certify();
    }
    t2 <- unclass(Sys.time())
    time <- round(t2-t1,2)
    displayExportSuccessMessage(saveAsName, time)
    reloadCurrentFolder();
  },
  error = function(e) {
    print(e$message)
    if(stringIntersects('Cannot overwrite a non-cube report with a cube report', e$message)){
      displayErrorMessage('RreportOverwriteError', e$message)
    }
    else if(stringIntersects('The object with the given identifier is not an object of the expected type', e$message)){
      displayErrorMessage('RexportUnexpectedObjectTypeError', e$message)
    }
    else {
      displayErrorMessage('RexportError', e$message)
    }
  })
}
