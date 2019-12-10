# api-cubes.R
#' @import httr

# Get the definition of a specific cube, including attributes and metrics. The cube can 
# be either an Intelligent Cube or a Direct Data Access (DDA)/MDX cube. The in-memory cube
# definition provides information about all available objects without actually running any
# data query/report. The results can be used by other requests to help filter large datasets
# and retrieve values dynamically, helping with performance and scalability.
# Returns complete HTTP response object.
cube <- function(connection, cube_id, verbose=FALSE) {
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/', cube_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Get information for specific cubes in a specific project. The cubes can be either Intelligent cubes or Direct
# Data Access (DDA)/MDX cubes. This request returns the cube name, ID, size, status, path, last modification date, and owner name and ID.
# Returns complete HTTP response object.
cube_info <- function(connection, cube_id, verbose=FALSE) {
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/?id=', cube_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Get elements of a specific attribute of a specific cube.
# Returns complette HTTP response object.
cube_elements <- function(connection, cube_id, attribute_id, offset=0, limit=25000, verbose=FALSE) {
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/', cube_id, '/attributes/', attribute_id, '/elements'),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        query=list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                   limit = format(limit, scientific=FALSE, trim=TRUE)),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Get the results of a newly created cube instance.
# This in-memory instance can be used by other requests.
# Returns complete HTTP response object.
cube_instance <- function(connection, cube_id, body=NULL, offset=0, limit=1000, verbose=FALSE) {
  response <- httr::POST(url = paste0(connection@base_url, '/cubes/', cube_id, '/instances'),
                         add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                     'X-MSTR-ProjectID' = connection@project_id),
                         body = body,
                         content_type_json(),
                         query = list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                      limit = format(limit, scientific=FALSE, trim=TRUE)),
                         set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Get the results of a previously created instance for a specific cube, using the in-memory instance
# created by  a POST /cubes/{cubesId}/instances request.
# Returns complete HTTP response object.
cube_instance_id <- function(connection, cube_id, instance_id, offset=0, limit=1000, verbose=FALSE) {
  response <- httr::GET(url = paste0(connection@base_url, '/cubes/', cube_id, '/instances/', instance_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        query=list(offset = format(offset, scientific=FALSE, trim=TRUE),
                                   limit = format(limit, scientific=FALSE, trim=TRUE)),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Publish a specific cube in a specific project.
# Returns complette HTTP response object.
publish <- function(connection, cube_id, verbose=FALSE) {
  response <- httr::POST(url = paste0(connection@base_url, '/cubes/', cube_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}

# Get the status of a specific cube in a specific project. The status is returned in HEADER X-MSTR-CubeStatus 
# with a value from EnumDSSCubeStates, which is a bit vector.
# Returns complette HTTP response object.
status <- function(connection, cube_id, verbose=FALSE) {
  response <- httr::HEAD(url = paste0(connection@base_url, '/cubes/', cube_id),
                        add_headers('X-MSTR-AuthToken' = connection@auth_token,
                                    'X-MSTR-ProjectID' = connection@project_id),
                        set_cookies(connection@cookies))
  if(verbose){
    print(response$url)
  }
  response
}
