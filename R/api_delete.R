#' Delete MS Bioscreen data entry
#'
#' \code{api_delete} deletes the bioscreen data associated with the specified \code{source_id},
#' \code{external_identifier}, and \code{endpoint}.
#'
#' @inheritParams api_check
#' @param source_id source identifer
#' @param external_identifier external identifer
#'
#' @details
#' Deleting 'subjects' data will also delete the associated 'attacks', 'treatments', and 'visits' data.
#' Deleting 'attacks', 'treatments', or 'visits' data will NOT delete the associated data in any of the other
#' endpoints.
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}}
#' @export

api_delete = function(source_id = 1, external_identifier = 100, endpoint,
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = get_token(),
                      verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Deleting entry in %s data (source_id: %s, external_identifier: %s)...", endpoint, source_id, external_identifier))
  }

  # getting url of app
  url = paste(base_url, "sources", source_id, endpoint, external_identifier, sep = "/")

  # deleting data
  response_data = api_do_action(action = DELETE, url = url, token = token)

  # check status
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 200, print_when_ok = print_when_ok)

  if(!success) {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when deleting entry in %s data (source_id: %s, external_identifier: %s)", response_data$status_code, endpoint, source_id, external_identifier))
  }

}
