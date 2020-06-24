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
  error_msg <- paste("Error connecting to project", connection$project_name, ". Check project name and try again.")
  response_handler(response, error_msg)
  return(response)
}
