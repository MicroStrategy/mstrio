# test-report.R
context("test-report.R")
library(jsonlite)

# Report: API_RESPONSE_DATA_REPORT
report_id = "1B139A5411EA056500000080EFB5E261"

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
test[["attr_elements"]] <- list("PassengerId" = 
                              list("id" = "EBB2160A11EA056427640080EFB5E261",
                                    "elements" = list("EBB2160A11EA056427640080EFB5E261:115",
                                                      "EBB2160A11EA056427640080EFB5E261:494")))
test[["selected_attr"]] <- list(test$attributes[[6]])
test[["selected_metr"]] <- list(test$metrics[[1]],test$metrics[[3]])
test[["selected_elem"]] <- test$attr_elements[[1]]$elements   

with_mock_api({

    # Create a common connection object
    conn <- connect_mstr(username="mstr", password="rCSyajne4bBp", base_url="https://env-167618.customer.cloud.microstrategy.com/MicroStrategyLibrary/api", project_name="MicroStrategy Tutorial")

    test_that("project_id", {
        expect_equal(conn@project_id,"B7CA92F04B9FAE8D941C3E9B7E0CD754")
    })

    test_that("test_init_report", {
        # Test that definition of the Report is assigned properly when report is initialized.
        report <-Report$new(connection = conn,report_id=report_id)

        expect_equal(report$connection,conn)
        expect_equal(report$report_id,report_id)
        expect_equal(report$name,"API_RESPONSE_DATA_REPORT")

        expect_equal(report$attributes, test$attributes)
        expect_equal(report$metrics,test$metrics)
        
        expect_null(report$selected_attributes)
        expect_null(report$selected_metrics)
        expect_null(report$selected_attr_elements)
        expect_null(report$dataframe)
    })

    test_that("test_apply_filters", {
        #Test that selected objects are assigned properly when filter is applied.
    
        report <- Report$new(connection = conn,report_id=report_id)
        report$apply_filters(test$selected_attr,test$selected_metr,test$selected_elem)

        expect_equal(report$selected_attributes,test$selected_attr)
        expect_equal(report$selected_metrics,unlist(test$selected_metr))
        expect_equal(report$selected_attr_elements,unlist(test$selected_elem))
    })

    test_that("test_clear_filters", {
        #Test that selected objects are assigned with empty lists when filter is cleared.

        report <- Report$new(connection = conn,report_id=report_id)
        report$apply_filters(test$selected_attr,test$selected_metr,test$selected_elem)

        expect_equal(report$selected_attributes,test$selected_attr)
        expect_equal(report$selected_metrics,unlist(test$selected_metr))
        expect_equal(report$selected_attr_elements,unlist(test$selected_elem))

        report$clear_filters()

        expect_null(report$selected_attributes)
        expect_null(report$selected_metrics)
        expect_null(report$selected_attr_elements)
    })

    test_that("validate_apply_filters_with_empty_list_as_parameters", {
        report <- Report$new(connection = conn,report_id=report_id)
        report$apply_filters(attributes=list(), metrics=list())

        expect_equal(report$selected_attributes, list())
        expect_equal(report$selected_metrics, list())
    })

    test_that("test_apply_filters_invalid_elements",{
        #Test that invalid id passed to a filter raises ValueError.
        report <- Report$new(connection = conn,report_id=report_id)

        expect_error(report$apply_filters(attributes = c("badID")), "Invalid object ID: badID")
    })

    test_that("test_to_dataframe",{
        #Test that data is retrieved and parsed properly when to_dataframe() is called.
        #Result should be saved to Report.dataframe property.
        
        df <- data.frame(PassengerId = c(115,494), 
                        Pclass = c(11,11),
                        Survived = c(2,2))
        df_converted <- type.convert(df, as.is = TRUE)

        report <- Report$new(connection = conn,report_id=report_id)
        #report$apply_filters(selected_attr,selected_metrs,selected_elem)
        report$apply_filters(test$selected_attr,test$selected_metr,test$selected_elem)
        expect_equal(report$to_dataframe(limit=1),df_converted)
    })

})

