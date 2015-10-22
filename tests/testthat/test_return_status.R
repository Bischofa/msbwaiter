library(msbwaiter)
context("Testing return_status function")

# tests
test_that("TRUE is returned when successful HTTP request is made", {
  test_response_data = api_do_action(action = GET, url =  "https://msbioscreen-uat.herokuapp.com/api/v1/subjects", results_per_page = 3)
  expect_equal(return_status(test_response_data, print_when_ok = ""), TRUE)
})

test_that("FALSE is returned when successful HTTP request is made", {
  test_response_data = api_do_action(action = GET, url =  "https://msbioscreen-uat.herokuapp.com/api/v1/blahblah", results_per_page = 3)
  expect_equal(return_status(test_response_data), FALSE)
})
