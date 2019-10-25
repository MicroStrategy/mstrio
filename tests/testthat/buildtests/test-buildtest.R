# buildtest.R
# tests end-to-end workflows against live environment
# is not re-run by CRAN because this code is kept in a sub-directory of /tests/testthat
#
# To run the test, run the following from the console in R and follow the on-screen prompts
# testthat::test_dir("tests/testthat/buildtests/")


library(testthat)
context("test-buildtest.R")

cat("Run end-to-end workflow tests against a live MicroStrategy connection? (Y or N):")
rune2e <- ifelse(toupper(readline())=="Y", TRUE, FALSE)

if(rune2e) {
  USERNAME <- readline("user id: ")
  PASSWORD <- readline("password: ")
  PROJ_NAME <- readline("project name: ")
  BASE_URL <- readline("base url: ")

  # or if running manually, set/update these:
  USERNAME <- "mstr"
  PASSWORD <- "bslSlZ3EdF0D"
  PROJ_NAME <- "MicroStrategy Tutorial"
  BASE_URL <- "https://env-125127.customer.cloud.microstrategy.com/MicroStrategyLibrary/api"

}



######################
## Helper functions ##
######################

get_data <- function(data_url){
  df <- read.csv(file=data_url, stringsAsFactors=FALSE)
  names(df) <- paste0("V", seq(1:length(names(df))))
  names(df)[1] <- "ID"
  df[, "ID"] <- as.character(seq(1:nrow(df)))  # force col ID to be integer sequence
  return(df)
}

get_numeric_cols <- function(df){
  num_cols <- names(df)[which(sapply(df, is.numeric))]  # detect numeric columns for checksum
  return(num_cols)
}

get_col_sums <- function(df, col_names){
  col_sums <- sapply(col_names, function(x){
    if(is.character(df[, x])){
      sum(as.numeric(df[,x]), na.rm=TRUE)
    } else {
      sum(as.numeric(df[, x]), na.rm=TRUE)  # TODO: necessary? check on this...
    }
  })
  return(col_sums)
}



################
## Unit tests ##
################

test_that("create_dataset_single_table_api", {

  #skip_on_cran() # always skip this test if being executed by CRAN
  skip_if_not(rune2e, "User opted out of end-to-end workflow testing.")

  # test-level params
  dataset_name <- "R_create_dataset"
  table_name <- "R_create_dataset"
  update_policy <- NULL

  ##################
  ## Get the data ##
  ##################
  base_df <- get_data("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/iris.csv")
  base_num_cols <- get_numeric_cols(base_df)
  base_checksum <- get_col_sums(base_df, base_num_cols)

  ##########################
  ## Create the base cube ##
  ##########################
  conn <- connect_mstr(base_url = BASE_URL, username = USERNAME,
                       password = PASSWORD, project_name = PROJ_NAME)

  cube <- create_dataset(connection = conn, data_frame = base_df,
                         dataset_name = dataset_name, table_name = table_name)

  ###########################
  # Get data from the cube ##
  ###########################
  test_df <- get_cube(conn, cube$datasetID)
  test_checksum <- get_col_sums(test_df, base_num_cols)

  # close connection
  close(conn)

  # this is the actual test
  expect_equal(as.numeric(base_checksum),
               as.numeric(test_checksum))

})

# test_that("update_dataset", {})
# test_that("replace_dataset", {})
# test_that("upsert_dataset", {})
# test_that("add_dataset", {})
