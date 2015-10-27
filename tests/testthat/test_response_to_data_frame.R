library(msbwaiter)
context("Testing response_to_data_frame function")

# initial parameters
url = "https://msbioscreen-uat.herokuapp.com/api/v1/subjects"

# tests
test_that("response_to_data_frame converts an HTTP response to a data frame", {
  test_response_data = api_do_action(action = GET, url = url, results_per_page = 2)
  expect_equal(class(response_to_data_frame(test_response_data)), "data.frame")
  results_per_page = 3
  test_response_data = api_do_action(action = GET, url = url, results_per_page = results_per_page)
  expect_equal(nrow(response_to_data_frame(test_response_data)), results_per_page)
  results_per_page = 24
  test_response_data = api_do_action(action = GET, url = url, results_per_page = results_per_page)
  expect_equal(nrow(response_to_data_frame(test_response_data)), results_per_page)
})
