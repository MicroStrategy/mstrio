# test-model.R
context("test-utils-model.R")

# globals
DATASET_NAME <- "Employees"
DATASET_DESC <- "Table of employee data."

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

test_that("test_model_structure", {
  # test that model object contains needed structural components

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw
  model_json <- model_list$json

  # check that model is a list
  expect_type(model_raw, "list")

  # check that model_json is a json string
  expect_type(model_json, "character")  ## is technically 'json' but R reads as 'character'

  # check that list elements are named as expected
  expect_named(model_raw,
               expected=c("name", "description", "folderId", "tables", "metrics", "attributes"),
               ignore.order=TRUE)

  # check that these elements of the model are lists
  expect_type(model_raw$table, "list")
  expect_type(model_raw$metrics, "list")
  expect_type(model_raw$attributes, "list")

  # check that name and description match
  expect_equal(model_raw$name, DATASET_NAME)
  expect_equal(model_raw$description, DATASET_DESC)

  # check that column names are unaffected and match the original data frame
  ch <- unlist(lapply(seq_along(model_raw$tables[[1]]$columnHeaders), function(x){
    model_raw$tables[[1]]$columnHeaders[[x]]$name
  }))
  expect_equal(ch, names(df))

})

test_that("test_single_table", {
  # checks length and structure of a single table source

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_length(model_raw$table, 1)
  expect_length(model_raw$attributes, 4)
  expect_length(model_raw$metrics, 4)

})

test_that("test_multi_table", {
  # checks length and structure of a multi table source

  df <- make_df()
  tables = list(list("table_name" = "employee1",
                     "data_frame" = df),
                list("table_name" = "employee2",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_length(model_raw$table, 2)
  expect_length(model_raw$attributes, 8)
  expect_length(model_raw$metrics, 8)

})

test_that("test_metric_override", {
  # test ability to convert what should be a attribute into an metric

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df,
                     "to_metric" = c("id_str")))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_length(model_raw$attributes, 3)
  expect_length(model_raw$metrics, 5)


})

test_that("test_attribute_override", {
  # test ability to convert what should be a metric into an attribute

  df <- make_df()
  tables = list(list("table_name" = "employee1",
                     "data_frame" = df,
                     "to_attribute" = c("id_int")),
                list("table_name" = "employee2",
                     "data_frame" = df,
                     "to_attribute" = c("id_int", "salary", "age")))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_length(model_raw$attributes, 12)
  expect_length(model_raw$metrics, 4)

})

test_that("test_both_override", {
  # test ability to convert two types within the same table

  df <- make_df()
  tables = list(list("table_name" = "employee1",
                     "data_frame" = df,
                     "to_metric" = c("id_str"),
                     "to_attribute" = c("id_int", "salary", "age")))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_length(model_raw$attributes, 6)
  expect_length(model_raw$metrics, 2)

})

test_that("test_data_param_no_list", {
  # test that error is raised when no dataframe is provided in data frame parameter of tables

  tables <- make_df()

  expect_error(Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC))

})

test_that("test_data_param_empty_list", {
  # tests that despite a list being passed to tables when creating model, structure of tables is checked

  df <- make_df()
  tables <- list(df)
  expect_error(Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC))

})

test_that("test_data_param_bad_key", {
  # tests for properly named elements in tables

  df <- make_df()
  tables = list(list("wrong1" = "employee",
                     "wrong2" = df))

  expect_error(Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC))

})

test_that("test_long_dataset_description", {
  # test that descriptions longer than 250 chars cause an error

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  long_desc <- paste(c("Lorem ipsum dolor sit amet, consectetur adipiscing elit ",
                       "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. ",
                       "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris ",
                       "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in ",
                       "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla ",
                       "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa ",
                       "qui officia deserunt mollit anim id est laborum."), collapse = "")

  expect_error(Model$new(tables=tables, name=DATASET_NAME, description=long_desc))

})

test_that("test_folder_id_none", {
  # test handling of NULL value passed to folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC)
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_equal(model_raw$folderId, "")

})

test_that("test_folder_id_not_none", {
  # test handling of non-NULL value passed to folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  model_list <- model$get_model()
  model_raw <- model_list$raw

  expect_equal(model_raw$folderId, "folder123456")

})

test_that("test_get_model", {
  # test ability to retrieve model object from model class instance

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  model_list <- model$get_model()

  # names should be 'raw' and 'json'
  expect_named(model_list,
               expected=c("raw", "json"),
               ignore.order=TRUE)
})

test_that("test_get_name", {
  # test ability to retrieve model name

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  model_name <- model$get_name()
  expect_equal(model_name, DATASET_NAME)

})

test_that("test_get_description", {
  # test ability to retrieve model description

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  model_desc <- model$get_description()
  expect_equal(model_desc, DATASET_DESC)

})

test_that("test_get_folder_id", {
  # test ability to retrieve folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  folder_id <- model$get_folder_id()
  expect_equal(folder_id, "folder123456")
})

test_that("test_get_tables", {
  # test handling of non-NULL value passed to folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  tables <- model$get_tables()
  expect_type(tables, "list")
  expect_equal(length(tables), 1)

})

test_that("test_get_attributes", {
  # test handling of non-NULL value passed to folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  attributes <- model$get_attributes()
  expect_type(attributes, "list")
  expect_equal(length(attributes), 4)

})

test_that("test_get_metrics", {
  # test handling of non-NULL value passed to folder_id

  df <- make_df()
  tables = list(list("table_name" = "employee",
                     "data_frame" = df))

  model <- Model$new(tables=tables, name=DATASET_NAME, description=DATASET_DESC, folder_id = "folder123456")
  metrics <- model$get_metrics()
  expect_type(metrics, "list")
  expect_equal(length(metrics), 4)

})


