#' Get specific entry from MS Bioscreen.
#'
#' \code{api_get} returns the bioscreen data associated with the specified \code{source_id}, \code{external_identifier}, and \code{endpoint}.
#'
#' @inheritParams api_delete
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_search_by_epicid}}, \code{\link{api_search_by_updated_at}},
#' \code{\link{api_check_data}}
#' @export

api_get = function(source_id = 1, external_identifier = 100, endpoint = "subjects",
                   base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                   token = Sys.getenv("msbwaiter_token"),
                   verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Getting entry in %s data (source_id: %s, external_identifier: %s)...", endpoint, source_id, external_identifier))
  }

  # getting url of app
  url = paste(base_url, endpoint, "search",sep = "/")

  # searching data
  search_by_list = list(external_identifier = external_identifier, source_id = source_id)
  search_by_list = list(list(query = search_by_list))
  response_data = api_do_action(action = POST, url = url, token = token, json_body_data = to_json_non_array(search_by_list))

  # check status and return data frame if ok
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 200, print_when_ok = print_when_ok)

  if(success) {
    return(response_to_data_frame(response_data))
  } else {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when getting entry in %s data (source_id: %s, external_identifier: %s)", response_data$status_code, endpoint, source_id, external_identifier))
  }

}
