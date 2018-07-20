context("test_parsejson.R")

test_that('iris: ability to parse the server response into R data.frame', {
  load(system.file("testdata", "iris.Rdata", package="mstrio"))
  df <- parse_json(response)
  expect_equal(sum(as.numeric(df$Sepal.Length)), 876.5)
})

test_that('diamonds: testing ability to parse server response into R data.frame', {
  load(system.file("testdata", "diamonds.Rdata", package="mstrio"))
  df <- mstrio:::parse_json(response)
  expect_equal(sum(as.integer(df$price)), 212135217)
})

test_that('doctors: testing ability to parse server response into R data.frame', {
  load(system.file("testdata", "doctors.Rdata", package="mstrio"))
  df <- parse_json(response)
  expect_equal(sum(as.integer(df$educdec)), 241412)
})
