# api-projects.R
#' @import httr

# get a list of projects which the authenticated user has access to.
projects <- function(connection, verbose=FALSE){
  response <- httr::GET(url = paste0(connection$base_url, "/api/projects"),
                        add_headers("X-MSTR-AuthToken" = connection$auth_token),
                        set_cookies(connection$cookies))
  if (verbose){
    print(response$url)
  }
  if (!is.null(connection$project_name)) {
    proj_msg <- paste0(" with name: ", connection$project_name, ". Check project name")
  } else if (!is.null(connection$project_id)) {
    proj_msg <- paste0(" with id: ", connection$project_id, ". Check project id")
  } else {
    proj_msg = "Check project name and id"
  }

  error_msg <- paste("Error connecting to project", proj_msg, " and try again.")
  response_handler(response, error_msg)
  return(response)
}
