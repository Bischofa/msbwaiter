#' Fetch MS Bioscreen data entry
#'
#' \code{api_get} returns the bioscreen data associated with the specified \code{source_id},
#' \code{external_identifier}, and \code{endpoint}.
#'
#' @inheritParams api_delete
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_search_by_epicid}}, \code{\link{api_search_by_updated_at}},
#' \code{\link{api_check}}
#' @export

api_get = function(source_id = 1, external_identifier = 100, endpoint = "subjects",
                   base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                   token = get_token(),
                   verbose_b = TRUE){

  if (verbose_b) {
    cat(sprintf("Getting entry in %s data (source_id: %s, external_identifier: %s)...\n", endpoint, source_id, external_identifier))
  }

  # create the search list
  search_by_list = list(external_identifier = external_identifier, source_id = source_id)

  # run the query
  api_search(action = POST, search_by_list = search_by_list, endpoint = endpoint, base_url = base_url, token = token, verbose_b = verbose_b)

}
