#' General API interaction tool
#'
#' \code{api_do_action} is a general API interaction tool that can fetch, delete, update, create, and search
#' bioscreen data.
#'
#' @param action the HTTP method (GET, DELETE, PUT, POST).
#' @param url the url of the page to retrieve.
#' @param token HTTP authorization token. Default is to search global environment and environment for the variable 'msbwaiter_token'.
#' @param json_body_data a non-array JSON. This JSON may be used for searching, updating, or creating data. If NULL (default),
#' the body argument is not part of the HTTP request header.
#' @param results_per_page number of results to return per page. If set to NULL (default), the results_per_page query argument
#' in the HTTP request header is equal to its default value.
#'
#' @seealso \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}},
#' \code{\link{api_search_by_epicid}}, \code{\link{api_search_by_updated_at}}
#' \code{\link{api_check_data}}

api_do_action = function(action = GET,
                         url = "https://msbioscreen-uat.herokuapp.com/api/v1/subjects",
                         token = get_token("msbwaiter_token"),
                         json_body_data = NULL, results_per_page = NULL){
  format_token = paste0("Token token=", "\"", token,"\"")

  argument_list = list(url,
                       add_headers(Authorization = format_token),
                       content_type_json(),
                       body = json_body_data,
                       query = list(results_per_page = results_per_page))
  do.call(action, argument_list)
}
