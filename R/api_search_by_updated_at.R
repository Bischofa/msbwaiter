#' Search MS Bioscreen by an updated at timestamp
#'
#' \code{api_search_by_updated_at} returns the bioscreen entries updated on or after the specified date and time.
#'
#' @inheritParams api_check
#' @param updated_at_date date stamp in format "yyy-mm-dd".
#' @param updated_at_time time stamp in format "hh:mm:ss".
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}}, \code{\link{api_search_by_epicid}}
#' @export

api_search_by_updated_at = function(updated_at_date = "2015-09-24", updated_at_time = "00:00:00",
                                    endpoint = "subjects",
                                    base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                                    token = get_token(), verbose_b = TRUE){

  updated_at = paste(updated_at_date, updated_at_time)

  if (verbose_b) {
    cat(sprintf("Searching %s updated on or after %s at %s...\n", endpoint, updated_at_date, updated_at_time))
  }

  # create the search list
  search_by_list = list(updated_at = (list("$gt" = updated_at)))

  # run the query
  api_paginate_action(action = POST, search_by_list = search_by_list, endpoint = endpoint, base_url = base_url, token = token, verbose_b = verbose_b)

}
