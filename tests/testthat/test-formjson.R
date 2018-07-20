context("test_formjson.R")

test_that('json list contains elements required for cube creation', {
  jsonlist <- form_json(df=iris, table_name='IRIS')
  expect_named(jsonlist, expected=c('columnHeaders', 'attributeList', 'metricList'))
})

test_that('ability to convert what should be a metric into an attribute via to_attribute parameter', {
  dat <- iris
  dat$integer_attribute <- as.integer(row.names(dat))  # coerce to integer
  jsonlist <- form_json(df=dat, table_name='IRIS', to_attribute = c('integer_attribute'))
  expect_true(jsonlist$attributeList[[2]]$name == 'integer_attribute')
})

test_that('ability to convert what should be an attribute into a metric via to_metric parameter', {
  dat <- iris
  dat$integer_metric <- row.names(dat)  # is a character
  jsonlist <- form_json(df=dat, table_name='IRIS', to_metric = c('integer_metric'))
  expect_true(jsonlist$metricList[[5]]$name == 'integer_metric')
})
