# Connection.R

#' @title Connect to the MicroStrategy environment.
#' @description
#' Allows to establish, renew, check status and close the connection with MicroStrategy Intelligence Server.
#' @field base_url URL of the MicroStrategy REST API server.
#' @field username Your username.
#' @field password Your password.
#' @field project_name Name of the connected MicroStrategy Project. One of project name or project id is necessary.
#' @field project_id ID of the connected MicroStrategy Project. One of project name or project id is necessary.
#' @field login_mode Specifies authentication mode to use. Standard = 1 (default) or LDAP = 16.
#' @field ssl_verify If True (default), verifies the server's SSL certificates with each request.
#' @field web_version The current web version
#' @field iserver_version The current I-Server version
#' @field auth_token The authentication token returned by the I-Server
#' @field cookies Cookies
#' @field identity_token Identity token for delegated session. Used for connection initialized by GUI.
#' @field verbose If True (default), displays additional messages.
#' @examples
#' \dontrun{
#' # Create a connection object.
#' connection = Connection$new(base_url, username, password, project_name)
#'
#' # Connect or renew connection.
#' connection$connect()
#'
#' # Check connection status.
#' connection$status()
#'
#' # Renew connection to reset timeout counter.
#' connection$renew()
#'
#' # Close connection.
#' connection$close()
#' }
#' @docType class
#' @importFrom R6 R6Class
#' @export
Connection <- R6Class("Connection",

  public = list(

#instance variables
    base_url = NULL,
    username = NULL,
    password = NULL,
    project_name = NULL,
    project_id = NULL,
    login_mode = 1,
    ssl_verify = NULL,
    web_version = NULL,
    iserver_version = NULL,
    auth_token = NULL,
    cookies = NULL,
    identity_token = NULL,
    verbose = NULL,

#' @description
#' Establishes new connection with MicroStrategy Intelligence Server.
#' @param base_url URL of the MicroStrategy REST API server.
#' @param username Your username.
#' @param password Your password.
#' @param project_name Name of the connected MicroStrategy Project. One of project name or project id is necessary.
#' @param project_id ID of the connected MicroStrategy Project. One of project name or project id is necessary.
#' @param login_mode Specifies authentication mode to use. Standard = 1 (default) or LDAP = 16.
#' @param ssl_verify If True (default), verifies the server's SSL certificates with each request.
#' @param proxies If NULL (default) proxy is not defined. To set proxy use formula: (<username>:<password>@)<ip_address>:<port> ()-optional
#' @param identity_token Identity token for delegated session. Used for connection initialized by GUI.
#' @param verbose If True, displays additional messages. FALSE by default.
#' @return A new "Connection" object.
    initialize = function(base_url, username = NULL, password = NULL, project_name = NULL, project_id = NULL, login_mode = 1,
                          ssl_verify = TRUE, proxies = NULL, identity_token = NULL, verbose = TRUE) {

      # Basic error checking for input types
      if (class(base_url) != "character") stop("'base_url' must be a character; try class(base_url)")
      if (is.null(identity_token)) {
        if (!is.null(username) && class(username) != "character") stop("'username' must be a character; try class(username)")
        if (!is.null(password) && class(password) != "character") stop("'password' must be a character; try class(password)")
      }
      if (is.null(project_id) && is.null(project_name)) stop("Specify 'project_name' or 'project_id'.")
      if (!(login_mode %in% c(1, 8, 16))) stop("Invalid login mode. Only '1' (normal), '8' (guest),
                                                  or '16' (LDAP are supported.")
      if (class(ssl_verify) != "logical") stop("'ssl_verify' must be TRUE or FALSE")
      if (!is.null(project_id)) {
        if (class(project_id) != "character") stop("'project_id' must be a character; try class(project_id)")
      }
      else {
        if (class(project_name) != "character") stop("'project_name' must be a character; try class(project_name)")
      }
      base_url <- url_check(base_url)

      self$base_url <- base_url
      self$username <- username
      self$password <- password
      self$project_id <- project_id
      self$project_name <- project_name
      self$login_mode <- login_mode
      self$ssl_verify <- ssl_verify
      self$identity_token <- identity_token
      self$verbose <- verbose

      # reset last used config
      reset_config()
      if (!is.null(proxies))
        private$set_proxy(proxies)

      # Check if iServer and Web version are supported by MSTRIO
      info <- check_version(self$base_url, private$VRCH)
      private$version_ok <- info$is_ok
      self$web_version <- info$web_version
      self$iserver_version <- info$iserver_version

      if (private$version_ok) {
        if (!ssl_verify) {
          httr::set_config(config(ssl_verifypeer = FALSE))
          self$ssl_verify <- FALSE
        } else {
          self$ssl_verify <- ssl_verify
        }

        if (is.null(identity_token))
          # Makes connection
          self$connect()
        else
          # Delegate identity token
          self$delegate()

        private$select_project()

      }
      else {
        stop(sprintf("This version of mstrio is only supported on MicroStrategy %s or higher.
          Current Intelligence Server version: %s
          Current MicroStrategy Web version: %s", self$VRCH, self$web_version, self$iserver_version), call. = FALSE)
      }
    },

#' @description
#' Establishes new connection with MicroStrategy Intelligence Server, or renews active connection.
    connect = function() {
      response <- session_renew(connection = self, verbose = private$debug)
      if (response$status_code == 204) {
        if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server was renewed.") }
      } else {
        # Create session
        response <- login(connection = self, verbose = private$debug)
        # Add authentication token and cookies to connection object
        self$auth_token <- response$headers[["x-mstr-authtoken"]]
        self$cookies <- response$headers[["set-cookie"]]
        if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server has been established.") }
      }
    },

#' @description
#' Delegates identity token to get authentication token and connect to MicroStrategy Intelligence Server
    delegate = function() {
      response <- delegate(connection = self, verbose = private$debug)
      self$auth_token <- response$headers[["x-mstr-authtoken"]]
      self$cookies <- response$headers[["set-cookie"]]
      if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server has been delegated.") }
    },

#' @description
#' Gets identity token using existing authentication token
    get_identity_token = function() {
      response <- identity_token(connection = self, verbose = private$debug)
      return(response$headers$`x-mstr-identitytoken`)
    },

#' @description
#' Closes a connection with MicroStrategy REST API.
    close = function() {
      # Terminate the connection
      response <- logout(connection = self, verbose = private$debug)
      if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server has been closed.") }
    },

#' @description
#' Renews connection with MicroStrategy REST API.
    renew = function() {
      response <- session_renew(connection = self, verbose = private$debug)
      if (response$status_code == 204) {
        if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server was renewed.") }
      } else {
        # Create session
        response <- login(connection = self, verbose = private$debug)
        # Add authentication token and cookies to connection object
        self$auth_token <- response$headers[["x-mstr-authtoken"]]
        self$cookies <- response$headers[["set-cookie"]]
        if (self$verbose == TRUE) { print("Connection with MicroStrategy Intelligence Server was not active. New connection has been established.") }
      }
    },

#' @description
#' Displays status of the connection with MicroStrategy REST API.
    status = function() {
      response <- session_status(connection = self, verbose = private$debug)
      if (response$status_code == 200) {
        print("Connection with MicroStrategy Intelligence Server is active.")
      } else {
        print("Connection with MicroStrategy Intelligence Server is not active.")
      }
    }

  ),

  private = list(
# private instance variables
    VRCH = "11.1.0400",
    version_ok = NULL,
    debug = FALSE,

    select_project = function() {
      response <- projects(connection = self, verbose = private$debug)
      projs <- content(response)

      proj_msg <- " "

      if (!is.null(self$project_name)) {
        for (proj in projs) {
          if (proj$name == self$project_name) {
            self$project_id = proj$id
            return()
          }
        }
        proj_msg <- paste0(" with name: ", self$project_name, " ")
      } else {
        for (proj in projs) {
          if (proj$id == self$project_id) {
            self$project_name = proj$name
            return()
          }
        }
        proj_msg <- paste0(" with id: ", self$project_id, " ")
      }
      # If executing the below, it means the project was not found in the result set.
      # Possible typo in project name parameter.
      # Close the session. Assuming the user will attempt to re-authenticate, this prevents excess sessions
      on.exit(self$close(connection = self))

      # Raises server error message
      status <- http_status(response)
      errors <- content(response)
      usrmsg <- paste0("Project", proj_msg, "not found. Check project name and try again.")

      stop(sprintf("%s\n HTTP Error: %s %s %s\n I-Server Error: %s %s",
                  usrmsg, response$status_code, status$reason, status$message, errors$code, errors$message),
          call. = FALSE)

    },

    set_proxy = function(proxy) {
      #check if proxies are passed in proper formatting
      index <- gregexpr('@', proxy)[[1]][length(gregexpr('@', proxy)[[1]])]
      username <- NULL
      password <- NULL
      if (index > 0) {
        credentials <- substr(proxy, 1, index - 1)
        username <- substr(credentials, 1, gregexpr(':', credentials)[[1]][1] - 1)
        password <- substr(credentials, gregexpr(':', credentials)[[1]][1] + 1, nchar(credentials))
        host <- substr(proxy, index + 1, nchar(proxy))
      }
      else {
        host <- proxy
      }
      if (grepl(':', host)) {
        ip <- substr(host, 1, gregexpr(':', host)[[1]][1] - 1)
        port <- as.numeric(substr(host, gregexpr(':', host)[[1]][1] + 1, nchar(host)))
        #set config for httr requests
        set_config(use_proxy(url = ip,
                             port = port,
                             username = username,
                             password = password),
                    override = TRUE)
      }
      else {
        stop(print("Passed proxy has wrong formula. Proper formula is: (user:password@)url:port. ()-optional"), call. = FALSE)
      }
    }

  )
)
