#' Create MS Bioscreen data entry
#'
#' \code{api_create} creates a new entry in the bioscreen.
#'
#' @inheritParams api_check
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_update}}, \code{\link{api_delete}}
#' @export

api_create = function(sufl_data, endpoint,
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = get_token(), verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Creating new entry in %s data (source_id: %s, external_identifier: %s)...", endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

  # getting url of app
  url = paste(base_url, endpoint, sep = "/")

  # creating new entry
  response_data = api_do_action(action = POST, url = url, token = token, json_body_data = to_json_non_array(sufl_data))

  # check status
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 201, print_when_ok = print_when_ok)

  if(!success) {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when creating entry in %s data (source_id: %s, external_identifier: %s)", response_data$status_code, endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

}
