# api-datasets.R
#' @import httr

dataset_definition <- function(connection, dataset_id, verbose=FALSE){
  # Get the definition of a dataset

  response <- httr::GET(url = paste0(connection@base_url, "/datasets/", dataset_id, "?fields=tables&fields=columns"),
                        add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                    "X-MSTR-ProjectID"=connection@project_id,
                                    "Content-Type"="application/json",
                                    "Accept"="application/json"),
                        set_cookies(connection@cookies),
                        encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

.create_dataset <- function(connection, body, verbose=FALSE){

  response <- httr::POST(url=paste0(connection@base_url, "/datasets"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                     "X-MSTR-ProjectID"=connection@project_id,
                                     "Content-Type"="application/json",
                                     "Accept"="application/json"),
                         body=body,
                         set_cookies(connection@cookies),
                         encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

.update_dataset <- function(connection, dataset_id, table_name, update_policy, body, table_id=NULL, verbose=FALSE){
  response <- httr::PATCH(url=paste0(connection@base_url, "/datasets/", dataset_id, "/tables/", table_name),
                          add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                      "X-MSTR-ProjectID"=connection@project_id,
                                      "updatePolicy"=update_policy,
                                      "Content-Type"="application/json",
                                      "Accept"="application/json"),
                          body=body,
                          set_cookies(connection@cookies),
                          encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

delete_dataset <- function(connection, dataset_id, verbose=FALSE){
  # Delete a dataset previously created using the REST API
  response <- httr::DELETE(url=paste0(connection@base_url, "/datasets/", dataset_id, "?type=3"),
                           add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                       "X-MSTR-ProjectID"=connection@project_id,
                                       "Content-Type"="application/json",
                                       "Accept"="application/json"),
                           set_cookies(connection@cookies),
                           encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

create_multitable_dataset <- function(connection, body, verbose=FALSE){
  # Create the definition of a multi-table dataset
  response <- httr::POST(url=paste0(connection@base_url, "/datasets/models"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                     "X-MSTR-ProjectID"=connection@project_id,
                                     "Content-Type"="application/json",
                                     "Accept"="application/json"),
                         body=body,
                         set_cookies(connection@cookies),
                         encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

upload_session <- function(connection, dataset_id, body, verbose=FALSE){
  # Create a multi-table dataset upload session
  response <- httr::POST(url=paste0(connection@base_url, "/datasets/", dataset_id, "/uploadSessions"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                     "X-MSTR-ProjectID"=connection@project_id,
                                     "Content-Type"="application/json",
                                     "Accept"="application/json"),
                         body=body,
                         set_cookies(connection@cookies),
                         encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

upload <- function(connection, dataset_id, session_id, body, verbose=FALSE){
  # Upload data to a multi-table dataset
  response <- httr::PUT(url=paste0(connection@base_url, "/datasets/", dataset_id, "/uploadSessions/", session_id),
                        add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                    "X-MSTR-ProjectID"=connection@project_id,
                                    "Content-Type"="application/json",
                                    "Accept"="application/json"),
                        body=body,
                        set_cookies(connection@cookies),
                        encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

publish <- function(connection, dataset_id, session_id, verbose=FALSE){
  # Publish a multi-table dataset
  response <- httr::POST(url=paste0(connection@base_url, "/datasets/", dataset_id, "/uploadSessions/", session_id, "/publish"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                     "X-MSTR-ProjectID"=connection@project_id,
                                     "Content-Type"="application/json",
                                     "Accept"="application/json"),
                         set_cookies(connection@cookies),
                         encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

publish_status <- function(connection, dataset_id, session_id, verbose=FALSE){
  # Get multi-table dataset publication status
  response <- httr::GET(url=paste0(connection@base_url, "/datasets/", dataset_id, "/uploadSessions/", session_id, "/publishStatus"),
                        add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                    "X-MSTR-ProjectID"=connection@project_id,
                                    "Content-Type"="application/json",
                                    "Accept"="application/json"),
                        set_cookies(connection@cookies),
                        encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

publish_cancel <- function(connection, dataset_id, session_id, verbose=FALSE){
  # Delete a multi-table dataset upload session and cancel publication
  response <- httr::DELETE(url=paste0(connection@base_url, "/datasets/", dataset_id, "/uploadSessions/", session_id),
                           add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                       "X-MSTR-ProjectID"=connection@project_id,
                                       "Content-Type"="application/json",
                                       "Accept"="application/json"),
                           set_cookies(connection@cookies),
                           encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}
