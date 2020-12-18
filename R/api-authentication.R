# api-authentication.R
#' @import httr

# Create a session with the MicroStrategy REST API server
login <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection$base_url, "/api/auth/login"),
                         body=list("username"=connection$username,
                                   "password"=connection$password,
                                   "loginMode"=connection$login_mode,
                                   "applicationType"=35),
                         encode="json")
  if (verbose){
    print(response$url)
  }
  error_msg <- "Authentication error. Check user credentials or REST API URL and try again."
  response_handler(response, error_msg)

  return(response)
}


# Terminate the user's session with the MicroStrategy REST API server
logout <- function(connection, verbose=FALSE){
  response <- httr::POST(url=paste0(connection$base_url, "/api/auth/logout"),
                         add_headers("X-MSTR-AuthToken"=connection$auth_token),
                         set_cookies(connection$cookies),
                         encode="json")
  if (verbose){
    print(response$url)
  }
  error_msg <- "Error attempting to terminate session connection. The session may have been terminated by the
                Intelligence Server."
  response_handler(response, error_msg)

  return(response)
}


# Renews the authentication token on the server side
session_renew <- function(connection, verbose=FALSE){
  response <- httr::PUT(url=paste0(connection$base_url, "/api/sessions"),
                        add_headers("X-MSTR-AuthToken" = connection$auth_token),
                        set_cookies(connection$cookies))
  if (verbose){
    print(response$url)
  }
  return(response)
}


# Checks session status
session_status <- function(connection, verbose=FALSE){
  response <- httr::GET(url=paste0(connection$base_url, "/api/sessions"),
                        add_headers("X-MSTR-AuthToken"=connection$auth_token),
                        set_cookies(connection$cookies))
  if (verbose){
    print(response$url)
  }
  return(response)
}

# Create a new identity token. An identity token is used to share an existing session with another application,
# based on the authorization token for the existing session
identity_token <- function(connection, verbose=FALSE) {
  response <- httr::POST(url=paste0(connection$base_url, "/api/auth/identityToken"),
                         add_headers("X-MSTR-AuthToken"=connection$auth_token))
  if (verbose){
    print(response$url)
  }
  return(response)
}

# Validate identity token X-MSTR-IdentityToken
validate_identity_token <- function (connection, identity_token, verbose=FALSE) {
  response <- httr::GET(url=paste0(connection$base_url, "/api/auth/identityToken"),
                                   add_headers("X-MSTR-IdentityToken"=identity_token))
  if (verbose){
    print(response$url)
  }
  return(response)
}

# Create a new Web server session for an application, using the identity token associated with the Web server session
# of a different application. Both Web server sessions share the same IServer session.
delegate <- function(connection, verbose=FALSE) {
  response <- httr::POST(url = paste0(connection$base_url, "/api/auth/delegate"),
                         body=toJSON(list("identityToken"=connection$identity_token,
                                          "loginMode"=-1), auto_unbox = TRUE),
                         content_type_json())
  if (verbose){
    print(response$url)
  }
  error_msg <- "Error creating a new Web server session that shares an existing IServer session."
  response_handler(response, error_msg)

  return(response)
}