#' Update MS Bioscreen data entry
#'
#' \code{api_update} updates an entry in the bioscreen.
#'
#' @inheritParams api_check
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_delete}}
#' @export

api_update = function(sufl_data, endpoint,
                      ignore_colnames = c("first_name", "last_name"),
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = get_token(),
                      verbose_b = TRUE, keep_na = FALSE){

  if (verbose_b) {
    cat(sprintf("Updating entry in %s data (source_id: %s, external_identifier: %s)...", endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

  # getting url of app
  url = paste(base_url, "sources", sufl_data$source_id, endpoint, sufl_data$external_identifier, sep = "/")

  # updating existing data
  sufl_data = sufl_data[, !(colnames(sufl_data) %in% ignore_colnames)]
  response_data = api_do_action(action = PUT, url = url, token = token, json_body_data = to_json_non_array(sufl_data, keep_na = keep_na))

  # check status
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 202, print_when_ok = print_when_ok)

  if(!success) {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when updating entry in %s data (source_id: %s, external_identifier: %s)", response_data$status_code, endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

}
