#' General API search function
#'
#' \code{api_search} returns the bioscreen entries that match the specified search query.
#'
#' @inheritParams api_get
#' @param search_by_list list of search parameter names and values
#'
#' @export

api_search = function(search_by_list = list(source_id = 1, external_identifier = 1),
                      endpoint = "subjects",
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = get_token(),
                      verbose_b = TRUE){

  # getting url of app
  url = paste(base_url, endpoint, "search",sep = "/")

  # getting search_by_list in right format
  search_by_list = list(list(query = search_by_list))

  # get the response
  response_data = api_do_action(action = POST, url = url, token = token, json_body_data = to_json_non_array(search_by_list))

  # check status and return data frame if ok
  print_when_ok = ifelse(verbose_b, "Done.\n", "")
  success = return_status(response_data, 200, print_when_ok = print_when_ok)

  if(success) {
    return(response_to_data_frame(response_data))
  } else {
    stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when running query", response_data$status_code))
  }
}
