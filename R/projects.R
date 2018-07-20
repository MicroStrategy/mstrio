# projects.R
#' @import httr

# get a list of projects which the authenticated user has access to.
projects <- function(connection, verbose=FALSE){
  response <- httr::GET(url = paste0(connection@base_url, '/projects'),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}
