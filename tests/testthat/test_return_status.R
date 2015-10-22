library(msbwaiter)
context("Testing return_status function")

# initial parameters
url = "https://msbioscreen-uat.herokuapp.com/api/v1/subjects"

# tests
test_that("return_status...", {
  results_per_page = 3
  test_response_data = api_do_action(action = GET, url = url, results_per_page = results_per_page)
  expect_equal(nrow(response_to_data_frame(test_response_data)), results_per_page)
  results_per_page = 56
  test_response_data = api_do_action(action = GET, url = url, results_per_page = results_per_page)
  expect_equal(nrow(response_to_data_frame(test_response_data)), results_per_page)
})
