library(msbwaiter)
context("Testing api_do_action function")

# tests
test_that("api_do_action returns a HTTP response", {
  request = api_do_action(action = GET,
                          url = "https://msbioscreen-uat.herokuapp.com/api/v1/attacks",
                          json_body_data = NULL,
                          results_per_page = NULL)
  expect_equal(class(request), "response")

})
