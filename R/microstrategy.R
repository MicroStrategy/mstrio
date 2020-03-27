# microstrategy.R
# Interface for connecting to the MicroStrategy REST API server, creating datasets, updating datasets, and extracting data from reports and cubes


#' @import httr
#' @importFrom jsonlite toJSON
#' @importFrom openssl base64_encode
#' @importFrom utils tail
#' @importFrom methods new

#' @title Connection class
#'
#' @description Base S4 class object containing connection parameters
#' @slot username Username
#' @slot password Password
#' @slot base_url URL for the REST API server
#' @slot project_name Name of the project to connect to (e.g. "MicroStrategy Tutorial")
#' @slot project_id Project ID corresponding to the chosen project name. This is determined when 
#' connecting to the project by name.
#' @slot application_code Code used to identify the client with MicroStrategy.
#' @slot web_version web version.
#' @slot iserver_version iServer version.
#' @slot VRCH Current minimum version supported.
#' @slot login_mode Authentication option. Standard (1) or LDAP (16).
#' @slot web_version web version.
#' @slot iserver_version iServer version.
#' @slot version_ok Both iServer and web version are supported.
#' @slot ssl_verify Default TRUE. Attempts to verify SSL certificates with each request.
#' @slot auth_token Token provided by the I-Server after a successful log in.
#' @slot cookies Cookies returned by the I-Server after a successful log in.
#' @exportClass connection
.connection <- setClass("connection",
                        slots = c(username = 'character',
                                  password = 'character',
                                  base_url = 'character',
                                  project_name = 'character',
                                  project_id = 'character',
                                  login_mode = 'numeric',
                                  application_code = 'numeric',
                                  web_version = 'character',
                                  iserver_version = 'character',
                                  version_ok = 'logical',
                                  VRCH = "character",
                                  ssl_verify = 'logical',
                                  auth_token = 'character',
                                  cookies = 'character'),
                        prototype = list(application_code = 65, VRCH = "11.1.0400"))

#' @title Create a MicroStrategy REST API connection
#'
#' @description Establishes and creates a connection with the MicroStrategy REST API.
#' @param base_url URL of the MicroStrategy REST API server
#' @param username Username
#' @param password Password
#' @param project_name Name of the project you intend to connect to. Case-sensitive
#' @param project_id ID of the project you intend to connect to
#' @param login_mode Specifies the authentication mode to use. Supported authentication modes
#'                   are Standard (1) (default) or LDAP (16)
#' @param ssl_verify If \code{TRUE} (default), verifies the server's SSL certificates with each request
#' @return A connection object to use in subsequent requests
#' @name connect_mstr
#' @rdname connect_mstr
#' @examples
#' \donttest{
#' # Connect to a MicroStrategy environment
#' con <- connect_mstr(base_url = "https://demo.microstrategy.com/MicroStrategyLibrary/api",
#'                     username = "user",
#'                     password = "password",
#'                     project_name = "Financial Reporting")
#'
#' # A good practice is to disconnect once you're done
#' # In case you forget, the server will disconnect the session after some time has passed
#' close(con)
#' }
#' @export
connect_mstr <- function(base_url, username, password, project_name = NULL, project_id = NULL, login_mode = 1, ssl_verify = TRUE) {

  # Basic error checking for input types
  if (class(base_url) != "character") stop("'base_url' must be a character; try class(base_url)")
  if (class(username) != 'character') stop("'username' must be a character; try class(username)")
  if (class(password) != 'character') stop("'password' must be a character; try class(password)")
  if (is.null(project_id) && is.null(project_name)) stop("Specify 'project_name' or 'project_id'.")
  if (!(login_mode %in% c(1, 8, 16))) stop("Invalid login mode. Only '1' (normal), '8' (guest), or '16' (LDAP) are supported.")
  if (class(ssl_verify) != 'logical') stop("'ssl_verify' must be TRUE or FALSE")

  base_url <- url_check(base_url)
  # Creates a new connection object
  if (!is.null(project_id)) {
    if (class(project_id) != 'character') stop("'project_id' must be a character; try class(project_id)")
    con <- .connection(base_url = base_url, username = username, password = password,
                       project_id = project_id, login_mode = login_mode)
  }
  else {
    if (class(project_name) != 'character') stop("'project_name' must be a character; try class(project_name)")
    con <- .connection(base_url = base_url, username = username, password = password,
                       project_name = project_name, login_mode = login_mode)
  }
  # Check if iServer and Web version are supported by MSTRIO
  info <- check_version(con@base_url, con@VRCH)
  con@version_ok <- info$is_ok
  con@web_version <- info$web_version
  con@iserver_version <- info$iserver_version

  if (con@version_ok) { # nocov start
    if (!ssl_verify) {
      httr::set_config(config(ssl_verifypeer = FALSE))
      con@ssl_verify <- FALSE
    } else {
      con@ssl_verify <- ssl_verify
    }

    # Makes connection
    tmp_con <- connect(connection = con)

    # Add authentication token and cookies to connection object
    con@auth_token <- tmp_con$auth_token
    con@cookies <- tmp_con$cookies

    if (is.null(project_id)) {
      # Connect to the project and set object's project id property
      con@project_id <- select_project(connection = con)
    }

    # Return connection object
    return(con)
  }
  else {
    stop(sprintf("This version of mstrio is only supported on MicroStrategy %s or higher.
    Current Intelligence Server version: %s
    Current MicroStrategy Web version: %s", con@VRCH, con@web_version, con@iserver_version), call. = FALSE)
  } # nocov end
}

# TODO: Document internal-only (non-exported) function and method
setGeneric("connect", function(connection) standardGeneric("connect"))

# TODO: Document internal-only (non-exported) function and method
setMethod("connect", "connection", function(connection){

  # Create session
  response <- login(connection = connection)

  return(list(auth_token = as.character(response$headers['x-mstr-authtoken']),
              cookies = as.character(response$headers['set-cookie'])))
})



#' @title Closes a connection with MicroStrategy REST API
#'
#' @description Closes a connection with MicroStrategy REST API.
#' @param connection MicroStrategy REST API connection object returned by \code{connect_mstr()}
#' @name close
#' @examples
#' \donttest{
#' # Connect to a MicroStrategy environment
#' con <- connect_mstr(base_url = "https://demo.microstrategy.com/MicroStrategyLibrary/api",
#'                     username = "user",
#'                     password = "password",
#'                     project_name = "Financial Reporting")
#'
#' # A good practice is to disconnect once you're done
#' # However, the server will disconnect the session after some time has passed
#' close(con)
#' }
#' @export close
setGeneric("close", function(connection) standardGeneric("close"))

#' @rdname close
setMethod("close", "connection", function(connection){

  # Terminate the connection
  response <- logout(connection = connection)

})


# TODO: Document internal-only (non-exported) function and method
setGeneric("select_project", function(connection) standardGeneric("select_project"))

# TODO: Document internal-only (non-exported) function and method
setMethod("select_project", "connection", function(connection){

  response <- projects(connection=connection)

  projs <- content(response)
  for(proj in projs){
    if(proj$name == connection@project_name){
      return(proj$id)
    }
  }

  # If executing the below, it means the project was not found in the result set. Possible typo in project name parameter.
  # Close the session. Assuming the user will attempt to re-authenticate, this prevents excess sessions
  on.exit(close(connection=connection))

  # Raises server error message
  status <- http_status(response)
  errors <- content(response)
  usrmsg <- paste0("Project '", connection@project_name, "' not found. Check project name and try again.")

  stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
               usrmsg, response$status_code, status$reason, status$message, errors$code, errors$message),
       call.=FALSE)

})
