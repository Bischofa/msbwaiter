#' Fetch MS Bioscreen endpoint data
#'
#' \code{api_get_batch} fetches the specified endpoint data from the bioscreen.
#'
#' @inheritParams api_do_action
#' @inheritParams api_check
#' @param number_of_pages number of pages of data to return. If set to NULL (default), all pages are returned.
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{return_status}}, \code{\link{response_to_data_frame}}
#' @export

api_get_batch = function(endpoint = "subjects",
                         base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                         token = get_token(), verbose_b = TRUE,
                         number_of_pages = NULL, results_per_page = NULL){

  if (verbose_b) {
    cat(sprintf("###### Fetching %s data... \n", endpoint))
  }

  # get the batch of data
  api_search(action = GET, search_by_list = NULL, endpoint = endpoint, base_url = base_url, token = token,
             verbose_b = verbose_b, number_of_pages = number_of_pages, results_per_page = results_per_page)

}



