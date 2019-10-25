# utils-encoder.R
# Internal method for converting a R data.frame to MicroStrategy compliant base64 encoded JSON

# TODO (srigney): documentation w/ roxy; ref. https://github.com/r-lib/R6/issues/3

library(R6)

#' @importFrom R6 R6Class
#' @importFrom jsonlite toJSON
#' @importFrom openssl base64_encode
#'
Encoder <- R6Class("Encoder",

  public = list(

    initialize = function(data_frame, dataset_type) {
      # Inits Encoder with given data_frame and type.

      private$data_frame <- data_frame

      # Sets the proper orientation
      if(!dataset_type %in% names(private$table_type_orient_map)) {
        stop("Dataset type should be 'single' or 'multi' for single-table and multi-table sources, respectively.")
      } else {
        private$orientation <- private$table_type_orient_map[[dataset_type]]
      }
    },

    encode = function() {
      # Encode data in base 64.

      private$b64_data <- base64_encode(toJSON(x = private$data_frame,
                                               dataframe = private$orientation,
                                               raw = "base64",
                                               Date = "ISO8601",
                                               factor = "string"))

      # return base 64 encoded data to calling environment
      return(private$b64_data)

    }
  ),

  private = list(

    data_frame = NULL,
    b64_data = NULL,
    orientation = NULL,
    table_type_orient_map = c("single" = "rows",
                              "multi" = "values")
  )
)
