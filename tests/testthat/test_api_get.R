library(msbwaiter)
context("Testing api_get function")

# initial parameters
base_url = "https://msbioscreen-uat.herokuapp.com/api/v1"
verbose_b = FALSE
source_id = 1
external_identifier = 8
endpoint = "subjects"

# tests
test_that("api_get returns a single entry of data", {

  # get entry..
  sufl_data = api_get(source_id = source_id, external_identifier = external_identifier, endpoint = endpoint,
                      base_url = base_url, verbose_b = verbose_b)
  have_to_create = length(sufl_data) == 0

  # if entry does not exist, create it and now api_get should return a single entry
  if(have_to_create){
    upload_sufl_data = data.frame(source_id = source_id, external_identifier = external_identifier,
                           first_name = "fakeFirstName", last_name = "fakeLastName",
                           dob = "1965-06-14", age_of_onset = 100)
    api_create(upload_sufl_data, endpoint = endpoint, base_url = base_url, verbose_b = verbose_b)

    sufl_data = api_get(source_id = source_id, external_identifier = external_identifier, endpoint = endpoint,
                        base_url = base_url, verbose_b = verbose_b)
  }

  expect_equal(nrow(sufl_data), 1)

})


# delete the entry that was created if the entry was not previously present
if(have_to_create){
  api_delete(source_id = source_id, external_identifier = external_identifier,
             endpoint = endpoint, base_url = base_url, verbose_b = verbose_b)
}


