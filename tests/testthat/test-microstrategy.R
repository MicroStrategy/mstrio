context("test-microstrategy.R")

# username <- "username"
# password <- "password"
# base_url <- "https://acme.bi.com/MicroStrategyLibrary/api"
# project_name <- "Finance"

base_url <- "https://env-167618.customer.cloud.microstrategy.com/MicroStrategyLibrary/api"
username <- "mstr"
password <- "rCSyajne4bBp"
project_name <- "MicroStrategy Tutorial"

cube_id <- "DB80F84011E9F65B00000080EF854BEA"



with_mock_api({
  test_that('connection object is a member of Connection class', {

    # explicitly create a new connection class object
    # this is done to test other functions in the absence of a server connection
    con <- .connection(username=username, password=password, base_url=base_url, project_name=project_name)

    expect_is(con, "connection")
    expect_s4_class(con, "connection")
  })


  test_that('connection object has properly named slots and assigned default values', {

    # explicitly create a new connection class object
    # this is done to test other functions in the absence of a server connection
    con <- .connection()

    expect_equal(.hasSlot(con, 'username'), TRUE)
    expect_equal(.hasSlot(con, 'password'), TRUE)
    expect_equal(.hasSlot(con, 'base_url'), TRUE)
    expect_equal(.hasSlot(con, 'project_name'), TRUE)
    expect_equal(.hasSlot(con, 'project_id'), TRUE)
    expect_equal(.hasSlot(con, 'login_mode'), TRUE)
    expect_equal(.hasSlot(con, 'application_code'), TRUE)
    expect_equal(.hasSlot(con, 'ssl_verify'), TRUE)
    expect_equal(.hasSlot(con, 'auth_token'), TRUE)
    expect_equal(.hasSlot(con, 'cookies'), TRUE)
    expect_equal(con@application_code, 65)

  })


  test_that('valid connection slots are properly assigned', {

    # explicitly create a new connection class object
    # this is done to test other functions in the absence of a server connection
    con <- .connection(username=username, password=password, base_url=base_url, project_name=project_name)

    expect_match(con@username, username)
    expect_match(con@password, password)
    expect_match(con@base_url, base_url)
    expect_match(con@project_name, project_name)
  })


  test_that('catch invalid parameter types when creating a new connection', {

    expect_error(.connection(username=1, password=password, base_url=base_url, project_name=project_name), regexp = NULL)
    expect_error(.connection(username=username, password=1, base_url=base_url, project_name=project_name), regexp = NULL)
    expect_error(.connection(username=username, password=password, base_url=1, project_name=project_name), regexp = NULL)
    expect_error(.connection(username=username, password=password, base_url=base_url, project_name=1), regexp = NULL)

  })


  test_that('assigned slots match class slots', {

    # explicitly create a new connection class object
    # this is done to test other functions in the absence of a server connection
    con <- .connection(username=username, password=password, base_url=base_url, project_name=project_name)

    app_code = 99

    con@project_id <- "project123"
    con@login_mode <- 16
    con@ssl_verify = TRUE
    con@auth_token = "authtoken123"
    con@cookies <- "cookies123"
    con@application_code <- app_code

    expect_match(con@username, username)
    expect_match(con@password, password)
    expect_match(con@base_url, base_url)
    expect_match(con@project_name, project_name)
    expect_match(con@project_id, "project123")
    expect_equal(con@login_mode, 16)
    expect_equal(con@application_code, app_code)
    expect_equal(con@ssl_verify, TRUE)
    expect_match(con@auth_token, "authtoken123")
    expect_match(con@cookies, "cookies123")

  })

  test_that('connect_mstr function', {
    con <- connect_mstr(base_url, username, password, project_name)

    app_code = 99

    con@project_id <- "project123"
    con@login_mode <- 16
    con@ssl_verify = TRUE
    con@auth_token = "authtoken123"
    con@cookies <- "cookies123"
    con@application_code <- app_code

    expect_match(con@username, username)
    expect_match(con@password, password)
    expect_match(con@base_url, base_url)
    expect_match(con@project_name, project_name)
    expect_match(con@project_id, "project123")
    expect_equal(con@login_mode, 16)
    expect_equal(con@application_code, app_code)
    expect_match(con@auth_token, "authtoken123")
    expect_match(con@cookies, "cookies123")
    expect_true(con@ssl_verify)

    con <- connect_mstr(base_url, username, password, project_name, ssl_verify=FALSE)
    expect_false(con@ssl_verify)
    expect_false(getOption("httr_config")[['options']]$ssl_verifypeer)
  })
})
