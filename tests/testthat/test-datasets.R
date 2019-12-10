# test-datasets.R
context("test-datasets.R")


# globals
base_url <- "https://env-167618.customer.cloud.microstrategy.com/MicroStrategyLibrary/api"
username <- "mstr"
password <- "rCSyajne4bBp"
project_name <- "MicroStrategy Tutorial"

conn <- .connection(username=username, password=password, base_url=base_url, project_name=project_name)  # mock connection object

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

with_mock_api({
    test_that("test_init_null_values", {
      # Test that null param values are assigned properly when initiated

      ds <- Dataset$new(connection=conn)

      expect_equal(ds$name, NULL)
      expect_equal(ds$description, NULL)
      expect_equal(ds$dataset_id, NULL)
      expect_equal(ds$.__enclos_env__$private$definition, NULL)
      expect_equal(ds$.__enclos_env__$private$session_id, NULL)
      expect_length(ds$.__enclos_env__$private$tables, 0)

    })

    test_that("test_init_non_null", {
      # Test that non-null param values are assigned properly when initiated

      test_name = "TEST"
      test_desc = "TEST DESCRIPTION"
      test_ds_id = "id1234567890"

      ds <- Dataset$new(connection=conn, name=test_name)
      expect_equal(ds$name, test_name)

      ds <- Dataset$new(connection=conn, description=test_desc)
      expect_equal(ds$description, test_desc)

      ds <- Dataset$new(connection=conn, dataset_id=test_ds_id)
      expect_equal(ds$dataset_id, test_ds_id)

    })

    test_that("test_add_table", {
      # Test that adding a table to the dataset increases length of tables property by one

        ds = Dataset$new(connection=conn)

        ds$add_table(name="TEST1", data_frame=make_df(), update_policy="add")
        expect_equal(!!ds$.__enclos_env__$private$tables[[1]]$table_name, "TEST1")
        expect_length(ds$.__enclos_env__$private$tables, 1)

        ds$add_table(name="TEST2", data_frame=make_df(), update_policy="add")
        expect_equal(!!ds$.__enclos_env__$private$tables[[2]]$table_name, "TEST2")
        expect_length(ds$.__enclos_env__$private$tables, 2)

        ds$add_table(name="TEST3", data_frame=make_df(), update_policy="add", to_attribute="state", to_metric="salary")
        expect_equal(!!ds$.__enclos_env__$private$tables[[3]]$table_name, "TEST3")
        expect_equal(!!ds$.__enclos_env__$private$tables[[3]]$to_attribute, "state")
        expect_equal(!!ds$.__enclos_env__$private$tables[[3]]$to_metric, "salary")
    })

    test_that("test_invalid_update_policy", {
      # Test that invalid update policy values produces an error

      ds <- Dataset$new(connection=conn)
      expect_error(ds$add_table(name="TEST", data_frame=make_df(), update_policy="invalid"))
    })

    test_that("test_invalid_attr_override", {
      # Test that attribute override columns names which dont match the source table produces an error

      ds <- Dataset$new(connection=conn)
      expect_error(ds$add_table(name="TEST", data_frame=make_df(), update_policy="add",
                                to_attribute=c("invalid")))

    })

    test_that("test_invalid_metr_override", {
      # Test that metric override columns names which dont match the source table produces an error

      ds <- Dataset$new(connection=conn)
      expect_error(ds$add_table(name="TEST", data_frame=make_df(), update_policy="add",
                                to_metric=c("invalid")))

    })

    test_that("test_get_upload_body", {
      # Test validity of upload session request body
      ds = Dataset$new(connection=conn)
      ds$add_table(name="TEST", data_frame=make_df(), update_policy="add")

      upload_body <- ds$.__enclos_env__$private$form_upload_body()
      expect_length(upload_body, 2)
      expect_named(upload_body,
                   expected=c("raw", "json"),
                   ignore.order=TRUE)
      expect_named(upload_body$raw$tables[[1]],
                   expected=c("name", "updatePolicy", "columnHeaders"),
                   ignore.order=TRUE)

    })

    test_that("test_create", {
        # conn <- .connection(username=username, password=password, base_url=base_url, project_name=project_name)  # mock connection object
        # ds = Dataset$new(connection=conn, name="test_name")
        # ds$add_table(name="TEST", data_frame=make_df(), update_policy="add")
        # ds$create(folder_id="12345", auto_upload=FALSE)
        # expect_equal(!!ds$folder_id, "12345")
        #
        # ds = Dataset$new(connection=conn, name="test_name")
        # ds$add_table(name="TEST", data_frame=make_df(), update_policy="add")
        # ds$create(auto_upload=FALSE)
        # expect_true(is.null(!!ds$folder_id))
    })
})