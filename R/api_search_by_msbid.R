#' Search MS Bioscreen for a specific MSB ID
#'
#' \code{api_search_by_msbid} returns the bioscreen entries associated with the specified \code{msbid}.
#'
#' @inheritParams api_check
#' @param msbid msbioscreen identifier
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}}, \code{\link{api_search_by_updated_at}}
#' @export

api_search_by_msbid = function(msbid = 1, endpoint = "subjects",
                                base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                                token = get_token(), verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Searching %s for msbid = %s...\n", endpoint, msbid))
  }

  # create the search list
  search_by_list = list(epicid = msbid)

  # run the query
  api_search(action = POST, search_by_list = search_by_list, endpoint = endpoint, base_url = base_url, token = token, verbose_b = verbose_b)

}
