# api-misc.R
#' @importFrom httr GET

# Get information about the version of iServer and web.
server_status <- function(base_url, verbose = FALSE) {
  response <- httr::GET(url = paste0(base_url, '/status'))

  if(verbose){
    print(response$url)
  }
  return(response)
}
