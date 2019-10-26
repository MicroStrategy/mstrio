#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames

updateColumnName <- function(df_name, prev_name, new_name) {
    new_names <- names(get(df_name, mstrio_env))
    new_names[new_names == prev_name] <- new_name
    assign(df_name, stats::setNames(get(df_name, mstrio_env), new_names), mstrio_env)
}

reorderColumns <- function(df_name, cols_for_reorder, start_index) {
    cols <- jsonlite::fromJSON(cols_for_reorder)
    df <- get(df_name, mstrio_env)
    instr <- c((start_index):(length(cols)+(start_index-1)))
    names(instr) <- cols
    assign(df_name, arrange.col(df, instr), mstrio_env)
}
