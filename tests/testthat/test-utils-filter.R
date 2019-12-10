# test-utils-filter.R
context("test-utils-filter.R")

library(jsonlite)

test = list()
test[["attributes"]] <- list("row" = "37B4899E11E996150AC00080EFC58FB4",
                             "state" = "356F7E6411E996150AC00080EFC58FB4",
                             "category" = "356F88B411E996150AC00080EFC58FB4")

test[["metrics"]] <- list("sales" = "356F904811E996150AC00080EFC58FB4",
                          "profit" = "34A8389A11E99615B08E0080EFC50E5E")

test[["elements"]] <- list("row" = list("id" = "37B4899E11E996150AC00080EFC58FB4",
                                        "elements" = list(
                                            "1" = "37B4899E11E996150AC00080EFC58FB4:1",
                                            "2" = "37B4899E11E996150AC00080EFC58FB4:2",
                                            "3" = "37B4899E11E996150AC00080EFC58FB4:3"
                                        )),
                            "state" = list("id" = "356F7E6411E996150AC00080EFC58FB4",
                                        "elements" = list(
                                            "new york" = "356F7E6411E996150AC00080EFC58FB4:new york",
                                            "virginia" = "356F7E6411E996150AC00080EFC58FB4:virginia"
                                        )),
                            "category" = list("id" = "356F88B411E996150AC00080EFC58FB4",
                                        "elements" = list(
                                            "books" = "356F88B411E996150AC00080EFC58FB4:books",
                                            "music" = "356F88B411E996150AC00080EFC58FB4:music"
                                        )))

test[["elements_filter"]] <- list(
    "37B4899E11E996150AC00080EFC58FB4:1" = list("name" = "1",
                                                 "attr_id" = "37B4899E11E996150AC00080EFC58FB4"),
     "37B4899E11E996150AC00080EFC58FB4:2" = list("name" = "2",
                                                 "attr_id" = "37B4899E11E996150AC00080EFC58FB4"),
     "37B4899E11E996150AC00080EFC58FB4:3" = list("name" = "3",
                                                 "attr_id" = "37B4899E11E996150AC00080EFC58FB4"),
     "356F7E6411E996150AC00080EFC58FB4:new york" = list("name" = "new york",
                                                        "attr_id" = "356F7E6411E996150AC00080EFC58FB4"),
     "356F7E6411E996150AC00080EFC58FB4:virginia" = list("name" = "virginia",
                                                        "attr_id" = "356F7E6411E996150AC00080EFC58FB4"),
     "356F88B411E996150AC00080EFC58FB4:books" = list("name" = "books",
                                                     "attr_id" = "356F88B411E996150AC00080EFC58FB4"),
     "356F88B411E996150AC00080EFC58FB4:music" = list("name" = "music",
                                                     "attr_id" = "356F88B411E996150AC00080EFC58FB4")
     )

test[["attribute_sel"]] <- test$attributes[[1]]
test[['attribute_sel_list']] <- unlist(test$attributes, use.names = FALSE)
test[["metric_sel"]] <- test$metrics[[1]]
test[["metric_sel_list"]] <- unlist(test$metrics, use.names = FALSE)
test[["element_sel"]] <- test$elements[[1]]$elements[[1]]

test[["element_sel_list"]] <- c()
for(e in test[["elements"]]) test[["element_sel_list"]] = c(test[["element_sel_list"]], unlist(e[[2]]), use.names = FALSE) # all from each attr

test[["element_sel_same_list"]] <- unlist(test$elements[[1]]$elements[1:2], use.names = FALSE) # two from 1st attr
test[["invalid_id"]] <- "INVALID"


test_that("test_init_filter_structure", {
# Test that init populates the correct instance slots.
  f = Filter$new()

  expect_null(f$attributes)
  expect_null(f$metrics)
  expect_null(f$attr_elems)
  expect_null(f$attr_selected)
  expect_null(f$metr_selected)
  expect_null(f$attr_elem_selected)
})

test_that("test_init_object_ids", {
# Test that each object id is loaded in filter object properties.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  expect_equal(f$attributes, test$attributes)
  expect_equal(f$metrics, test$metrics)
  expect_equal(f$attr_elems, test$elements_filter)
})

test_that("test_init_attribute_elements", {
# Test that when not loading attribute elements, filter's attribute elements are None.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics)
  
  expect_null(f$attr_selected)
  expect_null(f$attr_elem_selected)
})

test_that("test_select_object_id", {
# Test adding an object adds the id to filter property.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel)
  f$select(object_id=test$metric_sel)
  f$select(object_id=test$element_sel)
  
  expect_equal(f$attr_selected, test$attribute_sel)
  expect_equal(f$metr_selected, test$metric_sel)
  expect_equal(f$attr_elem_selected, test$element_sel)
})

test_that("test_select_object_id_list", {
# Test adding an object adds the id to filter property.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel_list)
  f$select(object_id=test$metric_sel_list)
  f$select(object_id=test$element_sel_list)

  expect_equal(f$attr_selected, test$attribute_sel_list)
  expect_equal(f$metr_selected, test$metric_sel_list)
  expect_equal(f$attr_elem_selected, test$element_sel_list)
})

test_that("test_select_duplicate_object_id", {
# Tests adding a duplicate id does not add the second id to the selected filter.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel)
  f$select(object_id=test$attribute_sel)
  f$select(object_id=test$metric_sel)
  f$select(object_id=test$metric_sel)
  f$select(object_id=test$element_sel)
  f$select(object_id=test$element_sel)

  expect_equal(f$attr_selected, test$attribute_sel)
  expect_equal(f$metr_selected, test$metric_sel)
  expect_equal(f$attr_elem_selected, test$element_sel)
})

test_that("test_select_invalid_object_id", {
# Tests adding an invalid object id does not add the object id to selected filters.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  expect_error(f$select(object_id=test$invalid))
})

test_that("test_clear", {
# Test that clearing filters works.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel_list)
  f$select(object_id=test$metric_sel_list)
  f$select(object_id=test$element_sel_list)

  # reset
  f$clear()
  expect_null(f$attr_selected)
  expect_null(f$metr_selected)
  expect_null(f$attr_elem_selected)
})

test_that("test_requested_objects_one_attribute", {
# Test that choosing 1 att should return matching requested object in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel)
  ro_attr = unlist(f$requested_objects()$attributes, use.names = FALSE)

  expect_equal(ro_attr, test$attribute_sel)
  expect_null(f$metr_selected)
  expect_null(f$attr_elem_selected)

})

test_that("test_requested_objects_two_attributes", {
# Test that choosing 2 att should return matching requested object in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel_list[1:2])
  ro_attr = unlist(f$requested_objects()$attributes, use.names = FALSE)

  expect_equal(ro_attr, test$attribute_sel_list[1:2])
  expect_null(f$metr_selected)
  expect_null(f$attr_elem_selected)

})

test_that("test_requested_objects_one_metric", {
# Test that choosing 1 met should return matching requested object in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$metric_sel)
  ro_metr = unlist(f$requested_objects()$metrics, use.names = FALSE)

  expect_equal(ro_metr, test$metric_sel)
  expect_null(f$attr_selected)
  expect_null(f$attr_elem_selected)

})

test_that("test_requested_objects_two_metric", {
# Test that choosing 2 met should return matching requested object in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$metric_sel_list[1:2])
  ro_metr = unlist(f$requested_objects()$metrics, use.names = FALSE)

  expect_equal(ro_metr, test$metric_sel_list[1:2])
  expect_null(f$attr_selected)
  expect_null(f$attr_elem_selected)

})


test_that("test_requested_objects_both_list", {
# Test that adding lists of attributes and metrics yields requested objects with correcty elements.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  f$select(object_id=test$attribute_sel_list)
  f$select(object_id=test$metric_sel_list)

  ro_attr = unlist(f$requested_objects()$attributes, use.names = FALSE)
  ro_metr = unlist(f$requested_objects()$metrics, use.names = FALSE)

  expect_equal(ro_attr, test$attribute_sel_list)
  expect_equal(ro_metr, test$metric_sel_list)
  expect_null(f$attr_elem_selected)

})


test_that("test_view_filter_none", {
# Test that adding no attributes elements yields None view filter.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)
  # should be null if none are obj_id
  vf = f$view_filter()

  expect_null(vf)

})


test_that("test_view_filter_same_attrib_one_element", {
# Test that choosing 1 att elem should return matching view filter in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  f$select(test$element_sel)
  vf = f$view_filter()

  expect_equal(vf$operator, "In")
  expect_equal(vf$operands[[1]]$type, "attribute")
  expect_equal(vf$operands[[1]]$id, f$attr_elems[[test$element_sel]]$attr_id)
  expect_equal(vf$operands[[2]]$type, "elements")
  expect_equal(unlist(vf$operands[[2]]$elements, use.names = FALSE), test$element_sel)

})


test_that("test_view_filter_same_attrib_two_element", {
# Test that choosing 2 att elems from same attribute should return matching view filter in body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  f$select(test$element_sel_same_list)
  vf = f$view_filter()

  expect_equal(vf$operator, "In")
  expect_equal(vf$operands[[1]]$type, "attribute")
  expect_equal(vf$operands[[1]]$id, f$attr_elems[[test$element_sel_same_list[1]]]$attr_id)
  expect_equal(vf$operands[[2]]$type, "elements")
  expect_equal(unlist(vf$operands[[2]]$elements, use.names = FALSE), test$element_sel_same_list)

})


test_that("test_view_filter_multi_attrib_element", {
# Test that choosing att elem across attributes forms a correct view filter body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  f$select(test$element_sel_list)
  vf = f$view_filter()
  vf1 = vf$operands[[1]]
  vf2 = vf$operands[[2]]
  vf3 = vf$operands[[3]]

  expect_equal(vf$operator, "And")

  expect_equal(vf1$operator, "In")
  expect_equal(vf1$operands[[1]]$type, "attribute")
  expect_equal(vf1$operands[[1]]$id, f$attr_elems[[test$element_sel_list[1]]]$attr_id)
  expect_equal(vf1$operands[[2]]$type, "elements")
  expect_equal(unlist(vf1$operands[[2]]$elements, use.names = FALSE), unlist(test$elements[[1]]$elements, use.names = FALSE))

  expect_equal(vf2$operator, "In")
  expect_equal(vf2$operands[[1]]$type, "attribute")
  expect_equal(vf2$operands[[1]]$id, f$attr_elems[[test$element_sel_list[4]]]$attr_id)
  expect_equal(vf2$operands[[2]]$type, "elements")
  expect_equal(unlist(vf2$operands[[2]]$elements, use.names = FALSE), unlist(test$elements[[2]]$elements, use.names = FALSE))

  expect_equal(vf3$operator, "In")
  expect_equal(vf3$operands[[1]]$type, "attribute")
  expect_equal(vf3$operands[[1]]$id, f$attr_elems[[test$element_sel_list[7]]]$attr_id)
  expect_equal(vf3$operands[[2]]$type, "elements")
  expect_equal(unlist(vf3$operands[[2]]$elements, use.names = FALSE), unlist(test$elements[[3]]$elements, use.names = FALSE))

})


test_that("test_filter_body", {
# Test correctness of filter body.
  f = Filter$new(attributes=test$attributes, metrics=test$metrics, attr_elements=test$elements)

  # it should be empty dict
  expect_equal(as.character(f$filter_body()), '{}')

  # Only one attribute in requestedObjects
  f$select(object_id=test$attribute_sel)
  temp = list("requestedObjects" = list("attributes" = list(list("id" = test$attribute_sel))))
  expect_equal(f$filter_body(), toJSON(temp, auto_unbox = TRUE))
  f$clear()
  
  # Only one metric in requestedObjects
  f$select(object_id=test$metric_sel)
  temp = list("requestedObjects" = list("metrics" = list(list("id" = test$metric_sel))))
  expect_equal(f$filter_body(), toJSON(temp, auto_unbox = TRUE))
  f$clear()

  # Only one element in viewFilter
  f$select(object_id=test$element_sel)
  temp = list("viewFilter" = list("operator" = "In", 
                                "operands" = list(list("type" = "attribute", 
                                                       "id" = f$attr_elems[[test$element_sel]]$attr_id), 
                                                  list("type" = "elements", "elements" = list(list("id" = test$element_sel))))))
  toJSON(temp, auto_unbox = TRUE)
  f$clear()

  # All above combined
  f$select(object_id=test$attribute_sel)
  f$select(object_id=test$metric_sel)
  f$select(object_id=test$element_sel)
  temp = list("requestedObjects" = list("attributes" = list(list("id" = test$attribute_sel)),
                                        "metrics" = list(list("id" = test$metric_sel))),
              "viewFilter" = list("operator" = "In", 
                                            "operands" = list(list("type" = "attribute", 
                                                                  "id" = f$attr_elems[[test$element_sel]]$attr_id), 
                                                              list("type" = "elements", "elements" = list(list("id" = test$element_sel)))))
  )

})

