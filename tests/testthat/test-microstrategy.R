context("test-microstrategy.R")

USER <- "username"
PASS <- "password"
URL <- "https://acme.bi.com/MicroStrategyLibrary/api"
PROJ <- "Finance"


test_that('connection object is a member of Connection class', {

  # explicitly create a new connection class object
  # this is done to test other functions in the absence of a server connection
  con <- .connection(username=USER, password=PASS, base_url=URL, project_name=PROJ)

  expect_is(con, "connection")
  expect_s4_class(con, "connection")
})


test_that('connection object has properly named slots', {

  # explicitly create a new connection class object
  # this is done to test other functions in the absence of a server connection
  con <- .connection()

  expect_equal(.hasSlot(con, 'username'), TRUE)
  expect_equal(.hasSlot(con, 'password'), TRUE)
  expect_equal(.hasSlot(con, 'base_url'), TRUE)
  expect_equal(.hasSlot(con, 'project_name'), TRUE)
  expect_equal(.hasSlot(con, 'project_id'), TRUE)
  expect_equal(.hasSlot(con, 'login_mode'), TRUE)
  expect_equal(.hasSlot(con, 'ssl_verify'), TRUE)
  expect_equal(.hasSlot(con, 'auth_token'), TRUE)
  expect_equal(.hasSlot(con, 'cookies'), TRUE)

})


test_that('valid connection slots are properly assigned', {

  # explicitly create a new connection class object
  # this is done to test other functions in the absence of a server connection
  con <- .connection(username=USER, password=PASS, base_url=URL, project_name=PROJ)

  expect_match(con@username, USER)
  expect_match(con@password, PASS)
  expect_match(con@base_url, URL)
  expect_match(con@project_name, PROJ)
})


test_that('catch invalid parameter types when creating a new connection', {

  expect_error(.connection(username=1, password=PASS, base_url=URL, project_name=PROJ), regexp = NULL)
  expect_error(.connection(username=USER, password=1, base_url=URL, project_name=PROJ), regexp = NULL)
  expect_error(.connection(username=USER, password=PASS, base_url=1, project_name=PROJ), regexp = NULL)
  expect_error(.connection(username=USER, password=PASS, base_url=URL, project_name=1), regexp = NULL)

})


test_that('assigned slots match class slots', {

  # explicitly create a new connection class object
  # this is done to test other functions in the absence of a server connection
  con <- .connection(username=USER, password=PASS, base_url=URL, project_name=PROJ)

  con@project_id <- "project123"
  con@login_mode <- 16
  con@ssl_verify = TRUE
  con@auth_token = "authtoken123"
  con@cookies <- "cookies123"

  expect_match(con@username, USER)
  expect_match(con@password, PASS)
  expect_match(con@base_url, URL)
  expect_match(con@project_name, PROJ)
  expect_match(con@project_id, "project123")
  expect_equal(con@login_mode, 16)
  expect_equal(con@ssl_verify, TRUE)
  expect_match(con@auth_token, "authtoken123")
  expect_match(con@cookies, "cookies123")

})
