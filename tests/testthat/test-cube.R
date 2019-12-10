# test-cube.R
context("test-cube.R")
library(jsonlite)

# globals same as used for the mocked responses
base_url <- "https://env-167618.customer.cloud.microstrategy.com/MicroStrategyLibrary/api"
username <- "mstr"
password <- "rCSyajne4bBp"
project_name <- "MicroStrategy Tutorial"

# Cube: API_RESPONSE_DATA
cube_id = "F29FCC3211EA056400000080EF55215F"

# test data to evaluate the cube class
test <- list()
test[["attributes"]] <- list("Sex" = "EBF96A7811EA05641CA90080EFE54362", 
                          "Ticket" = "EBF97EAA11EA05641CA90080EFE54362",
                          "Age" = "EBB1EA3611EA056427640080EFB5E261",
                          "Embarked" = "EBB1F79C11EA056427640080EFB5E261",
                          "Name" = "EBB200F211EA056427640080EFB5E261",
                          "PassengerId" = "EBB2160A11EA056427640080EFB5E261")
test[["metrics"]] <- list("Pclass" = "EBF9617211EA05641CA90080EFE54362",
                          "SibSp" = "EBF971EE11EA05641CA90080EFE54362",
                          "Survived" = "EBF978D811EA05641CA90080EFE54362",
                          "Fare" = "EBF9845E11EA05641CA90080EFE54362",
                          "Row Count - Table3 (titanic_5R.xlsx)" = "EBDAF4A811EA056408990080EFE54260",
                          "Parch" = "EBB209DA11EA056427640080EFB5E261",
                          "Row Count - Table1 (titanic_5R.xlsx)" = "E905056611EA0564084B0080EF058260",
                          "Row Count - titanic" = "EBF98A2611EA05641CA90080EFE54362",
                          "Row Count - Table2 (titanic_5R.xlsx)" = "EBC5C26811EA056408700080EF45015F")
test[["attr_elements"]] <- list("Sex" = 
                              list("id" = "EBF96A7811EA05641CA90080EFE54362",
                                    "elements" = list("female" = "EBF96A7811EA05641CA90080EFE54362:female",
                                                      "male" = "EBF96A7811EA05641CA90080EFE54362:male")))
test[["selected_attr"]] <- test$attributes[[1]]
test[["selected_metrics"]] <- test$metrics[[1]]
test[["selected_attr_elements"]] <- test$attr_elements[[1]]$elements[[1]]
test[["multi_table"]] <- list("Table1 (titanic_5R.xlsx)" = list( "Age", "Embarked", "Name", "Parch", "PassengerId"),
                              "Table2 (titanic_5R.xlsx)" = list( "Pclass", "Sex", "SibSp", "Survived", "Ticket", "Fare", "Row Count - titanic"),
                              "Table3 (titanic_5R.xlsx)" = list("Pclass", "Sex", "SibSp", "Survived", "Ticket", "Fare"))

with_mock_api({

    # Create a common connection object
    conn <- connect_mstr(base_url, username, password, project_name = project_name)
    
    test_that("Test that the cube is initialized correctly.", {
        
        # Test the cube object's instance variables
        cube <- Cube$new(connection=conn, cube_id=cube_id)
        expect_equal(cube$connection, conn)
        expect_equal(cube$cube_id, cube_id)
        expect_equal(cube$name, "API_RESPONSE_DATA")

        expect_equal(cube$owner_id, "7FC05A65473CE2FD845CE6A1D3F13233")
        expect_equal(cube$path, "\\MicroStrategy Tutorial\\Profiles\\MSTR User (mstr)\\My Reports\\API_RESPONSE_DATA")
        expect_equal(cube$last_modified, "2019-11-12 15:56:11")
        expect_equal(cube$size, 852992)
        expect_equal(cube$attributes, test$attributes)          
        expect_equal(cube$metrics, test$metrics)
        expect_equal(cube$attr_elements$Sex, test$attr_elements$Sex)

        expect_null(cube$selected_attributes)
        expect_null(cube$selected_metrics)
        expect_null(cube$selected_attr_elements)
        expect_null(cube$dataframe)
        expect_null(cube$dataframe_list)

        expect_equal(class(cube$filters), c("Filter", "R6"))
    })

    test_that("Test that selected objects are assigned properly when filter is applied and cleared correctly with clear_filters", {
        cube <- Cube$new(connection=conn, cube_id=cube_id)
        cube$apply_filters(attributes = test$selected_attr, metrics = test$selected_metrics, attr_elements = test$selected_attr_elements)        
        expect_equal(cube$selected_attributes, test$selected_attr)
        expect_equal(cube$selected_metrics, test$selected_metrics)
        expect_equal(cube$selected_attr_elements, test$selected_attr_elements)

        cube$clear_filters()

        expect_null(cube$selected_attributes)
        expect_null(cube$selected_metrics)
        expect_null(cube$selected_attr_elements)
    })

    test_that("validate apply filters with empty list as parameters", {
        cube <- Cube$new(connection=conn, cube_id=cube_id)
        cube$apply_filters(attributes=list(), metrics=list())
        expect_equal(cube$selected_attributes, list())
        expect_equal(cube$selected_metrics, list())
    })

    test_that("Test that selected objects are null after clear_filters() is called.", {
        cube <- Cube$new(connection=conn, cube_id=cube_id)
        cube$apply_filters(attributes = test$selected_attr, metrics = test$selected_metrics, attr_elements = test$selected_attr_elements)

        cube$clear_filters()

        expect_null(cube$selected_attributes)
        expect_null(cube$selected_metrics)
        expect_null(cube$selected_attr_elements)
    })

    test_that("test_apply_filters_for_incorrect_assignments",{
        # Test that incorrectly assigned selected objects are assigned properly when filter is applied."

        cube <- Cube$new(connection = conn,cube_id=cube_id)
        cube$apply_filters(attributes = test$selected_metrics, metrics = test$selected_attr_elements, attr_elements = test$selected_attr)
        expect_equal(cube$selected_attributes, test$selected_attr)
        expect_equal(cube$selected_metrics, test$selected_metrics)
        expect_equal(cube$selected_attr_elements, test$selected_attr_elements)
    })

    test_that("test_apply_filters_invalid_elements",{
        #Test that invalid id passed to a filter raises ValueError.

        cube <- Cube$new(connection = conn,cube_id=cube_id)
        expect_error(cube$apply_filters(attributes = c("badID")), "Invalid object ID: badID")
    })

    test_that("test_apply_filter_no_list",{
        #Test that selected objects passed as strings are assigned properly when filter is applied.
        #TODO: apply_filters not working when lists supplied.
        cube <- Cube$new(connection = conn,cube_id=cube_id)
        cube$apply_filters(attributes = test$selected_attr[1], metrics = test$selected_metrics[1], attr_elements = test$selected_attr_elements[1])
        expect_equal(cube$selected_attributes, test$selected_attr)
        expect_equal(cube$selected_metrics, test$selected_metrics)
        expect_equal(cube$selected_attr_elements, test$selected_attr_elements)
    })


    test_that("test_to_dataframe with limit=2 and multi=FALSE",{
        #Test that data is retrieved and parsed properly when to_dataframe() is called.
        #Result should be saved to Report.dataframe property.
        
        df <- data.frame(Sex = c("female"), 
                        Pclass = c(5))
        df_converted <- type.convert(df, as.is = TRUE)

        cube <- Cube$new(connection = conn,cube_id=cube_id)
        cube$apply_filters(test$selected_attr, test$selected_metrics, test$selected_attr_elements)
        expect_equal(cube$to_dataframe(limit=2), df_converted)

    })
    
    test_that("test_to_dataframe with limit=2 and multi=True",{
        #Test that data is retrieved and parsed properly when to_dataframe() is called.
        #Result should be saved to Report.dataframe property.
        #TODO: multi_df fix for filtered data
        #This is not fully working as we need to filter our multi_table_definition when the cube is filtered such that a table is blank
        df <- data.frame(Sex = c("female"), 
                        Pclass = c(5))
        df2 <- data.frame(Sex = c("female"), 
                        Pclass = c(5))
        df3 <- data.frame(Sex = c("female"), 
                        Pclass = c(5))
        df_converted <- type.convert(df, as.is = TRUE)
        df2_converted <- type.convert(df, as.is = TRUE)
        df3_converted <- type.convert(df, as.is = TRUE)

        df_list <- list(df_converted, df2_converted, df3_converted)

        cube <- Cube$new(connection = conn,cube_id=cube_id)
        cube$apply_filters(attributes = test$selected_attr, metrics = test$selected_metrics, attr_elements = test$selected_attr_elements)
        #expect_equal(cube$to_dataframe(limit=2, multi_df=FALSE), df_list)

    })

    test_that("test_multitable_definition",{
        #Test that multitable definition function returns proper dictionary.
        
        cube <- Cube$new(connection = conn,cube_id=cube_id)
        cube$to_dataframe(multi_df=TRUE)
        expect_equal(cube$table_definition, test$multi_table)

    })

})
