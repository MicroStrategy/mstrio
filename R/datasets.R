# datasets.R
#' @import httr

# TODO: A better name here, but 'create_dataset' conflicts with class-level method
.create_dataset <- function(connection, json_body, verbose=FALSE){

  response <- httr::POST(url=paste0(connection@base_url, "/datasets"),
                         add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                     "X-MSTR-ProjectID"=connection@project_id,
                                     "Content-Type"="application/json",
                                     "Accept"="application/json"),
                         body=json_body,
                         set_cookies(connection@cookies),
                         encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}

# TODO: A better name here, but 'update_dataset' conflicts with class-level method
# TODO: 10.11 defect where table_name is used in place of table_id (not used)
.update_dataset <- function(connection, dataset_id, table_name, update_policy, json_body, table_id=NULL, verbose=FALSE){
  response <- httr::PATCH(url=paste0(connection@base_url, "/datasets/", dataset_id, "/tables/", table_name),
                          add_headers("X-MSTR-AuthToken"=connection@auth_token,
                                      "X-MSTR-ProjectID"=connection@project_id,
                                      "updatePolicy"=update_policy,
                                      "Content-Type"="application/json",
                                      "Accept"="application/json"),
                          body=json_body,
                          set_cookies(connection@cookies),
                          encode='json')
  if(verbose){
    print(response$url)
  }
  return(response)
}
