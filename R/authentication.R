# authentication.R
#' @import httr

# Create a session with the MicroStrategy REST API server
login <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection@base_url, "/auth/login"),
                         body=list("username"=connection@username,
                                   "password"=connection@password,
                                   "loginMode"=connection@login_mode),
                         encode="json")
  if(verbose){
    print(response$url)
  }
  return(response)
}


# Terminate the user's session with the MicroStrategy REST API server
logout <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection@base_url, "/auth/logout"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token),
                         encode="json")
  if(verbose){
    print(response$url)
  }
  return(response)
}


# Check that the user's session is still active, and renews the authentication token on the server side
sessions <- function(connection, verbose=FALSE){
  response <- httr::PUT(url=paste0(connection@base_url, "/sessions"),
                        add_headers("X-MSTR-AuthToken"=connection@auth_token),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}
