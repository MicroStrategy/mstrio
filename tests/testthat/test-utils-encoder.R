context("test-utils-encoder.R")

library(jsonlite)
library(openssl)

make_df <- function(){
  # helper function for use in unit tests
  df <- data.frame("id_int" = c(1, 2, 3, 4, 5),
                   "id_str" = c("1", "2", "3", "4", "5"),
                   "first_name" = c("Jason", "Molly", "Tina", "Jake", "Amy"),
                   "last_name" = c("Miller", "Jacobson", "Turner", "Milner", "Cooze"),
                   "age" = c(42, 52, 36, 24, 73),
                   "weight" = c(100.22, 210.2, 175.1, 155.9, 199.9),
                   "state" = c("VA", "NC", "WY", "CA", "CA"),
                   "salary" = c(50000, 100000, 75000, 85000, 250000),
                   stringsAsFactors = FALSE)
  return(df)
}

test_that("type_mapping_error", {
  # Test that returns an error if input is not one of single or multi

  expect_error(Encoder$new(data_frame=make_df(), dataset_type="notsupportedtype"))

})

test_that("type_mapping_single", {
  # Test that derived orientation for single table matches desired value for jsonlite which is 'rows'

  enc = Encoder$new(data_frame=make_df(), dataset_type="single")
  expect_equal(enc$.__enclos_env__$private$orientation, "rows")

})

test_that("type_mapping_multi", {
  # Test that derived orientation for multi table matches desired value for jsonlite which is 'values'

  enc = Encoder$new(data_frame=make_df(), dataset_type="multi")
  expect_equal(enc$.__enclos_env__$private$orientation, "values")

})

test_that("single_table_encoding", {
  # Test validity of encoding for single-table

  enc = Encoder$new(data_frame=make_df(), dataset_type="single")
  b64_enc <- enc$encode()
  df = fromJSON(rawToChar(base64_decode(b64_enc)))
  expect_equal(sum(df$salary), 560000)

})

test_that("multi_table_encoding", {
  # Test validity of encoding of multi-table

  enc = Encoder$new(data_frame=make_df(), dataset_type="multi")
  b64_enc <- enc$encode()
  df = data.frame(fromJSON(rawToChar(base64_decode(b64_enc))))
  names(df) <- names(make_df())
  expect_equal(sum(as.numeric(as.character.factor(df$salary))), 560000)

})

test_that("chunked_table_encoding_each_row", {
  # Test encoded result of a large dataframe which is broken up before upload
  # Specifically break the df up by each row

  df <- make_df()

  chunksize <- 1
  chunks <- split(df, rep(1:ceiling(nrow(df)/chunksize), each=chunksize, length.out=nrow(df)))

  b64_dec <- lapply(chunks, function(x){
    enc <- Encoder$new(x, "multi")
    b64_enc <- enc$encode()
    fromJSON(rawToChar(base64_decode(b64_enc)))
  })

  df <- data.frame(do.call("rbind", b64_dec))
  names(df) <- names(make_df())
  expect_equal(sum(as.numeric(as.character.factor(df$salary))), 560000)

})

test_that("chunked_table_encoding_uneven_row", {
  # Test encoded result of a large dataframe which is broken up before upload
  # Specifically breaking the df up into chunks of uneven lengths

  df <- make_df()

  chunksize <- 3
  chunks <- split(df, rep(1:ceiling(nrow(df)/chunksize), each=chunksize, length.out=nrow(df)))

  b64_dec <- lapply(chunks, function(x){
    enc <- Encoder$new(x, "multi")
    b64_enc <- enc$encode()
    fromJSON(rawToChar(base64_decode(b64_enc)))
  })

  df <- data.frame(do.call("rbind", b64_dec))
  names(df) <- names(make_df())
  expect_equal(sum(as.numeric(as.character.factor(df$salary))), 560000)

})

