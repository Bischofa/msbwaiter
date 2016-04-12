#' General API search function
#'
#' \code{api_search} returns the bioscreen entries that match the specified search query.
#'
#' @inheritParams api_get_batch
#' @param action the HTTP method (GET or POST)
#' @param search_by_list list of search parameter names and values
#'
#' @seealso \code{\link{api_get}}, \code{\link{api_get_batch}}, \code{\link{api_search_by_msid}}, \code{\link{api_search_by_updated_at}}
#' @export

api_search = function(action,
                      search_by_list = list(source_id = 1, external_identifier = 1),
                      endpoint = "subjects",
                      base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                      token = get_token(), verbose_b = TRUE,
                      number_of_pages = NULL, results_per_page = NULL){

  t0 = Sys.time()

  # getting url of app
  url = paste(base_url, endpoint, sep = "/")

  # if performing a POST, also add "search" to url
  if(identical(action, POST)){
    url = paste(url, "search", sep = "/")
  }

  # if performing a GET, set search_by_list to NULL and reset handle
  if(identical(action, GET)){

    suppressWarnings(handle_reset(url))

    if(!is.null(search_by_list)){
      warning("Ignoring search_by_list query because action = GET. Performing a batch query instead.\n")
      search_by_list = NULL
    }

  }

  # converting the search_by_list to a json query
  json_query = to_json_non_array(list(list(query = search_by_list)))

  # set initial parameters
  n = 1
  keep_going = TRUE
  all_data = list()

  while(keep_going) {

    if (verbose_b) {
      cat(sprintf("Fetching page %d...", n))
    }

    response_data = api_do_action(action = action, url = url, token = token, json_body_data = json_query, results_per_page = results_per_page)

    # check status code
    print_when_ok = ifelse(verbose_b, "Done.\n", "")
    success = return_status(response_data, 200, print_when_ok = print_when_ok)

    # get content of response data and convert to data frame
    if(success) {
      all_data[[n]] = response_to_data_frame(response_data)
    } else {
      stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when fetching %s at page %d", response_data$status_code, endpoint, n))
    }

    # update loop variables
    n = n+1

    # getting url and json query of next page
    url = response_data$headers$`x-next-page`
    json_query = response_data$headers$`x-next-post-body`

    # if content is on more than 1 page, continue getting data on subsequent pages until max number of pages is reached (if specified)
    keep_going = ifelse(is.null(number_of_pages), !is.null(url), !is.null(url) & n <= number_of_pages)
  }

  timediff = Sys.time() - t0

  if (verbose_b) {
    cat(sprintf("###### Done in %s %s ####\n", round(timediff, 3), units(timediff)))
  }

  return(do.call(rbind, all_data))

}
