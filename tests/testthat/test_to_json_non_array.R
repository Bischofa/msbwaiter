library(msbwaiter)
context("Testing to_json_non_array function")

# initial parameters
test_subjects_data = data.frame(source_id = 2, external_identifier = 19, msgb = NA, first_name = "Sarah", last_name = "Smith")
test_search_by_data_1 = list(list(query = list(external_identifier = 31, source_id = 2)))
test_search_by_data_2 = list(list(query = list(epicid = 34)))
test_search_by_data_3 = list(list(query = list(updated_at = (list("$gt" = "2015-09-24 00:00:00")))))

# tests
test_that("to_json_non_array converts r objects to correct JSON format", {
  expect_equal(as.character(to_json_non_array(test_subjects_data, overwrite_na_to_missing = FALSE)),
               "{\"source_id\":2,\"external_identifier\":19,\"first_name\":\"Sarah\",\"last_name\":\"Smith\"}")
  expect_equal(as.character(to_json_non_array(test_subjects_data, overwrite_na_to_missing = TRUE)),
               "{\"source_id\":2,\"external_identifier\":19,\"msgb\":null,\"first_name\":\"Sarah\",\"last_name\":\"Smith\"}")
  expect_equal(as.character(to_json_non_array(test_search_by_data_1, overwrite_na_to_missing = FALSE)),
               "{\"query\":{\"external_identifier\":31,\"source_id\":2}}")
  expect_equal(as.character(to_json_non_array(test_search_by_data_2, overwrite_na_to_missing = FALSE)),
               "{\"query\":{\"epicid\":34}}")
  expect_equal(as.character(to_json_non_array(test_search_by_data_3, overwrite_na_to_missing = FALSE)),
               "{\"query\":{\"updated_at\":{\"$gt\":\"2015-09-24 00:00:00\"}}}")
})
