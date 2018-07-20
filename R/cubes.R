# cubes.R
#' @import httr

# Get the definition of a specific cube, including attributes and metrics. The cube can be either an Intelligent Cube or a Direct Data Access (DDA)/MDX cube. The in-memory cube definition provides information about all available objects without actually running any data query/report. The results can be used by other requests to help filter large datasets and retrieve values dynamically, helping with performance and scalability.
cube <- function(connection, cube_id, verbose=FALSE){
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/', cube_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}


# Create a new instance of a specific cube. This in-memory instance can be used by other requests.
cube_instance <- function(connection, cube_id, offset=0, limit=1000, verbose=FALSE){
  response <- httr::POST(url = paste0(connection@base_url, '/cubes/', cube_id, '/instances'),
                         add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                     'X-MSTR-ProjectID' = connection@project_id),
                         query=list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                    limit = format(limit, scientific=FALSE, trim=TRUE)),
                         set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}


# Get the results of a previously created instance for a specific cube, using the in-memory instance created by cube_instance()
cube_instance_id <- function(connection, cube_id, instance_id, offset=0, limit=1000, verbose=FALSE){
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/', cube_id, '/instances/', instance_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        query=list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                   limit = format(limit, scientific=FALSE, trim=TRUE)),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  return(response)
}
