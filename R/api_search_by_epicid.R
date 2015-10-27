#' Search the MS Bioscreen for a specific EPICID
#'
#' \code{api_search_by_epicid} returns the bioscreen entries associated with the specified \code{epicid}.
#'
#' @inheritParams api_check_data
#' @param epicid EPIC identifier
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}}, \code{\link{api_search_by_updated_at}}
#' @export

api_search_by_epicid = function(epicid = 1, endpoint = "subjects",
                                base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                                token = Sys.getenv("msbwaiter_token"), verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Searching %s for epicid = %s...", endpoint, epicid))
  }

  # getting url of app
  url = paste(base_url, endpoint, "search",sep = "/")

  # searching data
  search_by_list = list(list(query = list(epicid = epicid)))
  response_data = api_do_action(action = POST, url = url, token = token, json_body_data = to_json_non_array(search_by_list))

  # check status and return data frame if ok
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 200, print_when_ok = print_when_ok)

  if(success) {
    return(response_to_data_frame(response_data))
  } else {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when searching %s for epicid = %s", response_data$status_code, endpoint, epicid))
  }

}
