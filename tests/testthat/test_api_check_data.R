library(msbwaiter)
context("Testing api_check_data function")

# initial parameters
base_url = "https://msbioscreen-uat.herokuapp.com/api/v1"
verbose_b = FALSE

# tests
test_that("api_check_data returns 'no action' when appropriate", {
  source_id = 1
  external_identifier = 8
  endpoint = "subjects"
  ignore_col_names = c("first_name", "last_name")
  sufl_data = api_get(source_id = source_id, external_identifier = external_identifier, endpoint = endpoint,
                      base_url = base_url, verbose_b = verbose_b)

  # if the data is identical to that in the bioscreen, 'no action' should be returned
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b)
  expect_equal(action, "no action")

  # if one of the ignore_col_names is changed, 'no action' should still be returned
  sufl_data$first_name = "new first name"
  sufl_data$last_name = "new last name"
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b)
  expect_equal(action, "no action")

  # if one of the ignore_col_names is NA but overwrite_na_to_missing = TRUE, action returned should still be 'no action'
  sufl_data$first_name = NA
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b, overwrite_na_to_missing = TRUE)
  expect_equal(action, "no action")

  # if one of the non-ignore_col_names/non-identifer columns is NA, overwrite_na_to_missing must be FALSE for 'no action' to be returned
  sufl_data$dob = NA
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b, overwrite_na_to_missing = FALSE)
  expect_equal(action, "no action")

})

test_that("api_check_data returns 'update' when appropriate", {
  source_id = 1
  external_identifier = 8
  endpoint = "subjects"
  ignore_col_names = c("first_name", "last_name")
  sufl_data = api_get(source_id = source_id, external_identifier = external_identifier, endpoint = endpoint,
                      base_url = base_url, verbose_b = verbose_b)

  # if one of the non-ignore_col_names/non-identifer columns is changed, 'update' should be returned (does not matter whether overwrite_na_to_missing is TRUE or FALSE)
  sufl_data$age_of_onset = 31
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b, overwrite_na_to_missing = FALSE)
  expect_equal(action, "update")

  # if one of the non-ignore_col_names/non-identifer columns is NA, overwrite_na_to_missing must be TRUE for 'update' to be returned
  sufl_data$dob = NA
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b, overwrite_na_to_missing = TRUE)
  expect_equal(action, "update")

})

test_that("api_check_data returns 'create' when appropriate", {
  endpoint = "subjects"
  ignore_col_names = c("first_name", "last_name")
  sufl_data = data.frame(source_id = 4, external_identifier = 8, first_name = "Sarah", last_name = "Smith", msgb = 10, gender = "F", age_of_onset = 33, year_of_onset = 1988)

  # if a data entry that is not found in the bioscreen, 'create' should be returned
  action = api_check_data(sufl_data = sufl_data, ignore_col_names = ignore_col_names, endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b, overwrite_na_to_missing = FALSE)
  expect_equal(action, "create")

})

