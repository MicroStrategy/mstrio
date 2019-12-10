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

applyDataModeling <- function(steps) {
    parsed_steps <- jsonlite::parse_json(steps)

    for(step in parsed_steps) {
        if(step$type == 'RENAME_DF') {
            renameDataframe(step$oldName, step$newName)
        }
    }
}

renameDataframe <- function(oldName, newName) {
    existsInTempEnv <- !is.null(mstrio_temp_env[[oldName]])
    if(!existsInTempEnv) {
        cloneDataframe(oldName)
    }
    oldDf <- mstrio_temp_env[[oldName]]
    assign(
        x =  newName,
        value = oldDf,
        envir = mstrio_temp_env
    )
    remove(list = c(oldName), envir = mstrio_temp_env)
}

cloneDataframe <- function(dataframeToClone) {
  originalDataframe <- mstrio_env[[dataframeToClone]]
  assign(
    x =  dataframeToClone,
    value = originalDataframe,
    envir = mstrio_temp_env
  )
}

getListOfDataframes <- function(envir) {
    unlisted <- unlist(eapply(mstrio_temp_env, function(x) is.data.frame(x) & nrow(x) > 0))
    names <- names(which(unlisted))
    names
}
