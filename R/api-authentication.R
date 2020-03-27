# api-authentication.R
#' @import httr

# Create a session with the MicroStrategy REST API server
login <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection@base_url, "/api/auth/login"),
                         body=list("username"=connection@username,
                                   "password"=connection@password,
                                   "loginMode"=connection@login_mode,
                                   "applicationType"=connection@application_code),
                         encode="json")
  if(verbose){
    print(response$url)
  }
  error_msg <- "Authentication error. Check user credentials or REST API URL and try again."
  response_handler(response, error_msg)

  return(response)
}


# Terminate the user's session with the MicroStrategy REST API server
logout <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection@base_url, "/api/auth/logout"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token),
                         set_cookies(connection@cookies),
                         encode="json")
  if(verbose){
    print(response$url)
  }
  error_msg <- "Error attempting to terminate session connection. The session may have been terminated by the Intelligence Server."
  response_handler(response, error_msg)

  return(response)
}


# Check that the user's session is still active, and renews the authentication token on the server side
sessions <- function(connection, verbose=FALSE){
  response <- httr::PUT(url=paste0(connection@base_url, "/api/sessions"),
                        add_headers("X-MSTR-AuthToken"=connection@auth_token),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}
