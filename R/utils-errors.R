MSTR_ERROR_CLASS <- "mstr_error"
MSTR_TIMEOUT_ERROR_CLASS <- "mstr_timeout_error"
ISERVER_CODE_TIMEOUT <- -2147206497 # MSI_REQUEST_TIMEOUT on server-side

gen_custom_error <- function(.subclass, message = NULL, call = NULL, ...) {
  structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
}

gen_mstr_error <- function(response, custom_message) {
  http_code <- response$status_code
  content <- httr::content(response)
  error_code <- content$code
  iserver_code <- content$iServerCode
  server_message <- content$message
  ticket_id <- content$ticketId
  error_subclass <- detect_error_subclass(http_code, error_code, iserver_code)
  error_class <- ifelse(!is.null(error_subclass), c(error_subclass, MSTR_ERROR_CLASS), MSTR_ERROR_CLASS)
  error_message <- sprintf("%s: %s (Ticket ID: %s, iServerCode: %i)", error_code, server_message, ticket_id, iserver_code)
  full_message <- ifelse(!is.null(custom_message), sprintf("%s\n%s", custom_message, error_message), error_message)
  gen_custom_error(.subclass = error_class, message = full_message, http_code = http_code, error_code = error_code, iserver_code = iserver_code, server_message = server_message, ticket_id = ticket_id, custom_message = custom_message)
}

detect_error_subclass <- function(http_code, error_code, iserver_code = NULL) {
  if (iserver_code == ISERVER_CODE_TIMEOUT) {
    return(MSTR_TIMEOUT_ERROR_CLASS)
  }
  else {
    return(NULL)
  }
}
