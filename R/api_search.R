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

  # converting the search_by_list to a json query
  jason_query = to_json_non_array(list(list(query = search_by_list)))

  # set initial parameters
  n = 1
  keep_going = TRUE
  all_data = list()

  while(keep_going) {

    if (verbose_b) {
      cat(sprintf("Fetching page %d of query...", n))
    }

    response_data = api_do_action(action = POST, url = url, token = token, json_body_data = jason_query)

    # check status code
    print_when_ok = ifelse(verbose_b, "Done.\n", "")
    success = return_status(response_data, 200, print_when_ok = print_when_ok)

    # get content of response data and convert to data frame
    if(success) {
      all_data[[n]] = response_to_data_frame(response_data)
    } else {
      stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when running query", response_data$status_code, n))
    }

    # update loop variables
    n = n+1

    # getting url and json query of next page
    url = response_data$headers$`x-next-page`
    jason_query = response_data$headers$`x-next-post-body`

    # if content is on more than 1 page, continue getting data on subsequent pages until max number of pages is reached (if specified)
    keep_going = !is.null(url)
  }

  return(do.call(rbind, all_data))

}


