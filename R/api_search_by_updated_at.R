#' Search MS Bioscreen data by when it was updated
#'
#' \code{api_search_by_epicid} searches bioscreen for all entries updated on or after specified \code{updated_at} and returns these entries.
#'
#' @inheritParams api_check_data
#' @param updated_at time stamp in format "yyy-mm-dd hh:mm:ss".
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}}, \code{\link{api_search_by_epicid}}


api_search_by_updated_at = function(updated_at = "2015-09-24 00:00:00", endpoint = "subjects",
                                    base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                                    token = Sys.getenv("msbwaiter_token"), verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Searching %s updated on or after %s...", endpoint, updated_at))
  }

  # getting url of app
  url = paste(base_url, endpoint, "search",sep = "/")

  # searching data
  search_by_list = list(list(query = list(updated_at = (list("$gt" = updated_at)))))
  response_data = api_do_action(action = POST, url = url, token = token, json_body_data = to_json_non_array(search_by_list))

  # check status and return data frame if ok
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 200, print_when_ok = print_when_ok)

  if(success) {
    return(response_to_data_frame(response_data))
  } else {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when searching %s updated after %s", response_data$status_code, endpoint, updated_at))
  }

}
