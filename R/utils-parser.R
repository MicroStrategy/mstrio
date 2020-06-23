# utils-parser.R
# Used to parse the JSON response from cube/report v2 download api and convert the data into a R data frame

#' @title Download api v2 parser
#'
#' @description Parse the JSON response from cube/report v2 download api and convert the data into a R data frame
#' @field response JSON response from the cube/report v2 download api
#' @examples
#' \donttest{
#' # Create a parser object
#' p <- Parser$new(response)
#'
#' # Parse the first request
#' p$parse(response)
#'
#' # Return the data frame
#' p$to_dataframe()
#' }
#' @docType class
#' @keywords internal
#' @importFrom R6 R6Class
#' @importFrom data.table rbindlist

Parser <- R6Class("Parser",

  public = list(

    chunk_size = NULL,
    total_rows = NULL,

# metric objects
    metric_columns = NULL,   # list of metric objects from the grid definition
    metric_count = NULL,     # number of all metrics

# row-level metric data
    metric_values = NULL,    # placeholder for the metric values joined from all chunks

# attribute objects
    attribute_names = NULL,       # attribute names
    attribute_form_names = NULL,  # attribute element values
    full_attr_count = NULL,       # count of all attributes and their forms
    mapped_attributes = NULL,     # indices of attributes and corresponding index value

    AF_COL_SEP = "_",  # attribute-form column separator; is traditionally '@' but this is not valid in R data frames

    initialize = function(response) {

      # extract column headers and names if not already done
      self$metric_columns <- private$extract_metric_col_names(response)
      self$metric_count <- length(self$metric_columns)
      self$metric_values <- data.frame(matrix(nrow = 0, ncol = self$metric_count))
      private$extract_paging_info(response)

    # extract attribute forms and column labels
      self$attribute_form_names <- private$extract_attribute_form_names(response)

    # extract attribute names
      self$attribute_names <- private$extract_attribute_names(response)
      self$mapped_attributes <- matrix(nrow = 0, ncol = self$full_attr_count)
    },

    parse = function(response) {
      # with each chunk, extract information from the rows
      # extract attribute values into 2d matrix if attributes exist in the response
      if (self$total_rows > 0) {
        if (length(self$attribute_names))
          self$mapped_attributes <- rbind(self$mapped_attributes, private$map_attributes(response))
        # extract metric values if metrics exist in the response
        if (self$metric_count)
          self$metric_values <- rbind(self$metric_values, private$extract_metric_values(response))
      }
    },

    to_dataframe = function() {
      # concatenate metrics and attributes into one data frame

      temp_met <- self$metric_values
      names(temp_met) <- self$metric_columns

      temp_att <- data.frame(self$mapped_attributes, stringsAsFactors = FALSE)
      names(temp_att) <- private$get_attribute_col_names()

      if (length(temp_att) != 0 & length(temp_met) != 0) {
        df <- cbind(temp_att, temp_met)
      } else if (length(temp_att) != 0) {
        df <- temp_att
      } else if (length(temp_met) != 0) {
        df <- temp_met
      } else {
        print("Dataframe looks empty")
      }
      return(df)
    }
  ),


  private = list(

    map_attributes = function(response) {
      label_map <- private$create_attribute_element_map(response)
      row_index <- private$extract_attribute_element_row_index(response)

      vf <- Vectorize(function(attribute_indexes, column) {
        label_map[[column]][attribute_indexes + 1]
      }, vectorize.args = c("attribute_indexes"), SIMPLIFY = FALSE)

      mapped_attr <- matrix(nrow = dim(row_index)[1], ncol = dim(row_index)[2])
      for (col in seq_along(label_map)) {
        mapped_attr[, col] <- unlist(vf(row_index[, col], col))
      }
      return(mapped_attr)
    },

    create_attribute_element_map = function(response) {
      # this maps attribute element labels to the corresponding attribute element row index from the grid headers
      # and is used later to replace the integer-based grid header values with the real attribute element labels

      # unnecessary 2nd call of the function.. Better to just save the result
      # extract attribute form element labels into list of lists
      ae_index_map <- list()
      #Iterate over attributes
      for (attribute in response$definition$grid$rows) {
        # iterate over attribute elements and extract all formvalues into seperate lists of forms
        multiple_forms_list <- lapply(attribute$elements, function(x) {
          x$formValues #returns list of all elements formvalue pairs
        })

        rbinded_list <- do.call(rbind, multiple_forms_list)

        for (i in seq(rbinded_list[1, ])) {
          # append each list corresponding to one attr_form to ae_index_map
          unique_list_forms <- rbinded_list[, i]
          ae_index_map <- append(ae_index_map, list(unique_list_forms))
        }
      }
      return(ae_index_map)
    },

    extract_attribute_element_row_index = function(response) {
      # Extracts attribute index values into matrix and replicates the attribute indexes for additional attribute form
      nrow <- length(response$data$headers$rows)
      attr_index <- matrix(unlist(response$data$headers$rows), nrow = nrow, byrow = TRUE)
      row_index <- matrix(nrow = nrow, ncol = self$full_attr_count)
      form_counts <- sapply(self$attribute_form_names, function(x) { length(x) })

      cum_count_forms <- 0
      for (small_iter in seq_along(form_counts)) {
        attr_form_number <- form_counts[[small_iter]] # attribute form count
        large_iter <- small_iter + cum_count_forms # large_iter is the correct index to access the final matrix
        low_idx <- large_iter
        high_idx <- large_iter + attr_form_number - 1
        row_index[, low_idx:high_idx] <- attr_index[, rep(small_iter, attr_form_number)]
        cum_count_forms <- cum_count_forms + attr_form_number - 1
      }
      return(row_index)
    },

    extract_paging_info = function(response) {
      self$chunk_size <- response$data$paging$limit
      self$total_rows <- response$data$paging$total
    },

    extract_metric_values = function(response) {
      # rbindlist will bind the metric rows to a data.table without coercing the data into one data type it will throw
      # a warning when NULL values will be found (hence suppressWarnings) and change them to NA, which is desired
      # Above statement is actually not true, implemented for loop to replace NULLs, but a better solution would be
      # appreciated.

      for (x in seq(response$data$metricValues$raw)) {
        if (is.null(response$data$metricValues$raw[[x]][[1]])) {
          response$data$metricValues$raw[[x]][[1]] <- NA
        }
      }
      return(as.data.frame(suppressWarnings(rbindlist(response$data$metricValues$raw)), stringsAsFactors=FALSE))
    },

    extract_metric_col_names = function(response) {
      # extracts column names into a list
      if (length(response$definition$grid$columns) != 0) {
        return(lapply(response$definition$grid$columns[[1]]$elements, function(y) {
          y$name
        }))
      }
    },

    get_metric_col_names = function() {
      col_names <- list()
      for (col in self$metric_columns) {
        col_names <- append(col_names, col$name)
      }
      return(col_names)
    },

    extract_attribute_form_names = function(response) {
      # extracts attribute form names as a list
      full_attr_count <- 0
      attr_form_names <- list()
      for (attribute in response$definition$grid$rows) {
        single_attr_form_names <- lapply(attribute$forms, function(x) {
          x$name
        })
        full_attr_count <- full_attr_count + length(single_attr_form_names)
        attr_form_names <- append(attr_form_names, list(single_attr_form_names))
      }
      self$full_attr_count <- full_attr_count
      return(attr_form_names)
    },

    extract_attribute_names = function(response) {
      # extracts attribute names from grid definition
      return(lapply(response$definition$grid$rows, function(x) {
        x$name
      }))
    },

    get_attribute_col_names = function() {
      # extract and format attribute column labels, including attribute forms
      col_names <- list()
      for (index in seq(self$attribute_form_names)) {
        if (length(self$attribute_form_names[[index]]) == 1) {
          col_names <- append(col_names, self$attribute_names[index])
        }
        else {
          tmp <- sapply(self$attribute_form_names[index], function(x) {
            paste(self$attribute_names[index], x, sep = self$AF_COL_SEP)
          })
          col_names <- append(col_names, tmp)
        }
      }
      return(col_names)
    }
  )
)
