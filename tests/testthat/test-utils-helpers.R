# test-cube.R
context("test-utils-helpers.R")

# globals
base_url <- "https://env-167618.customer.cloud.microstrategy.com/MicroStrategyLibrary/api"
username <- "mstr"
password <- "rCSyajne4bBp"
project_name <- "MicroStrategy Tutorial"

cube_id <- "DB80F84011E9F65B00000080EF854BEA"

test_that("firstUp changes case of the first letter to upper", {
    expect_equal(firstUp('abc'), 'Abc')
    expect_equal(firstUp(''), '')
    expect_false(isTRUE(all.equal(firstUp('Abc'), 'abc')))
})

test_that("stringIntersects return TRUE when one string within another", {
    expect_true(stringIntersects('', ''))
    expect_true(stringIntersects('ab', 'abc'))
    expect_false(stringIntersects('ab', 'bc'))
    expect_false(stringIntersects('abc', 'ab'))
    expect_false(stringIntersects('Ab', 'abc'))
})

test_that("arrange.col returns rearranged dataframe", {
    rearranged_df = arrange.col(data.frame(iris), c(Species=1))
    num_cols = unlist(lapply(rearranged_df, is.numeric))
    num_cols = lapply(rearranged_df, is.numeric)
    expect_equal(names(rearranged_df)[[1]], "Species")
    expect_identical(dim(rearranged_df), dim(data.frame(iris)))
    expect_equal(min(apply(Filter(is.numeric, rearranged_df), 2, min)),
                 min(apply(Filter(is.numeric, data.frame(iris)), 2, min)))
    expect_equal(mean(apply(Filter(is.numeric, rearranged_df), 2, mean)),
                 mean(apply(Filter(is.numeric, data.frame(iris)), 2, mean)))
    expect_equal(median(apply(Filter(is.numeric, rearranged_df), 2, median)),
                 median(apply(Filter(is.numeric, data.frame(iris)), 2, median)))
    expect_equal(max(apply(Filter(is.numeric, rearranged_df), 2, max)),
                 max(apply(Filter(is.numeric, data.frame(iris)), 2, max)))
})

test_that("verifyColumns stops execution when column names are not proper", {
    expect_error(verifyColumnsNames("fake_df_that_should_not_exist", c("Species")))
    expect_error(verifyColumnsNames("tmp_df", c("WrongCol")))
})

with_mock_api({
    test_that("check_version returns proper output and raises server errors", {
        old_version_check = check_version(base_url, "11.1.0.4")
        new_version_check = check_version(base_url, "12.1.0.4")

        expect_true(old_version_check[["is_ok"]])
        expect_equal(old_version_check[["web_version"]], "11.1.0400")
        expect_equal(old_version_check[["iserver_version"]], "11.1.0400")

        expect_false(new_version_check[["is_ok"]])
        expect_equal(new_version_check[["web_version"]], "11.1.0400")
        expect_equal(new_version_check[["iserver_version"]], "11.1.0400")

        expect_error(check_version(paste0(base_url, '/fake'), "11.1.0400"))
    })
})