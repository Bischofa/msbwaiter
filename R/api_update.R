#' Update entry in MS Bioscreen
#'
#' \code{api_update} updates an entry in the MS Bioscreen.
#'
#' @inheritParams api_check_data
#'
#' @details
#' sufl_data must be a data frame of 1 row with column names that follow the SUFL format. For updating subjects data, sufl_data needs to
#' contain the columns, 'source_id' and 'external_identifier'. For updating attacks, treatments, or visits data, sufl_data needs to
#' contain the columns, 'source_id', 'external_identifier', 'patient_source_id', and 'patient_external_identifier'. If any of these
#' column names are missing, \code{api_update} will return an error message. Also note that if there are NA values in non-identifier
#' columns of sufl_data that are non-missing in the bioscreen, \code{api_update} will only update bioscreen data to missing if
#' \code{overwrite_na_to_missing} is TRUE.
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}

api_update = function(sufl_data, endpoint = "subjects",
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = Sys.getenv("msbwaiter_token"),
                      verbose_b = TRUE, overwrite_na_to_missing = FALSE){

  if (verbose_b) {
    cat(sprintf("Updating entry in %s data (source_id: %s, external_identifier: %s)...", endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

  # getting url of app
  url = paste(base_url, "sources", sufl_data$source_id, endpoint, sufl_data$external_identifier, sep = "/")

  # updating existing data
  response_data = api_do_action(action = PUT, url = url, token = token, json_body_data = to_json_non_array(sufl_data, overwrite_na_to_missing = overwrite_na_to_missing))

  # check status
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 202, print_when_ok = print_when_ok)

  if(!success) {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when updating entry in %s data (source_id: %s, external_identifier: %s)", response_data$status_code, endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

}
