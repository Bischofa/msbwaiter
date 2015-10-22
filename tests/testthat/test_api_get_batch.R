library(msbwaiter)
context("Testing api_get_batch function")


# initial parameters
base_url = "https://msbioscreen-uat.herokuapp.com/api/v1"
verbose_b = FALSE
number_of_pages = 1
results_per_page = 10

# tests
test_that("correct results per page is returned", {
  expect_equal(nrow(api_get_batch(endpoint = "subjects",
                                  base_url = base_url,
                                  verbose_b = verbose_b,
                                  number_of_pages = number_of_pages,
                                  results_per_page = results_per_page)), results_per_page)
  expect_equal(nrow(api_get_batch(endpoint = "attacks",
                                  base_url = base_url,
                                  verbose_b = verbose_b,
                                  number_of_pages = number_of_pages,
                                  results_per_page = results_per_page)), results_per_page)
  expect_equal(nrow(api_get_batch(endpoint = "treatments",
                                  base_url = base_url,
                                  verbose_b = verbose_b,
                                  number_of_pages = number_of_pages,
                                  results_per_page = results_per_page)), results_per_page)
  expect_equal(nrow(api_get_batch(endpoint = "visits",
                                  base_url = base_url,
                                  verbose_b = verbose_b,
                                  number_of_pages = number_of_pages,
                                  results_per_page = results_per_page)), results_per_page)
})
