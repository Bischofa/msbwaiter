#' Get endpoint data from the MS Bioscreen
#'
#' \code{api_get_batch} fetches the specified endpoint data from the bioscreen.
#'
#' @inheritParams api_do_action
#' @inheritParams api_check_data
#' @param number_of_pages number of pages of data to return. If set to NULL (default), all pages are returned.
#'
#' @seealso \code{\link{api_do_action}}, \code{\link{return_status}}, \code{\link{response_to_data_frame}}
#' @export

api_get_batch = function(endpoint = "subjects",
                         base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                         token = get_token("msbwaiter_token"), verbose_b = TRUE,
                         number_of_pages = NULL, results_per_page = NULL){

  if (verbose_b) {
    cat(sprintf("###### Fetching %s data... \n", endpoint))
  }
  t0 = Sys.time()

  # getting url of app and corresponding handler
  url = paste(base_url, endpoint, sep = "/")
  suppressWarnings(handle_reset(url))

  # set initial parameters
  n = 1
  keep_going = TRUE
  all_endpoint_data = list()

  while(keep_going) {
    if (verbose_b) {
      cat(sprintf("Fetching page %d of %s data...", n, endpoint))
    }
    response_data = api_do_action(action = GET, url = url, token = token, results_per_page = results_per_page)

    # check status code
    print_when_ok = ifelse(verbose_b, "Done.\n", "")
    success = return_status(response_data, 200, print_when_ok = print_when_ok)

    # get content of response data and convert to data frame
    if(success) {
      all_endpoint_data[[n]] = response_to_data_frame(response_data)
    } else {
      stop(sprintf("SUFL_API_ERROR: Wrong http status (%d) when fetching %s at page %d", response_data$status_code, endpoint, n))
    }

    # update loop variables
    n = n+1

    # getting url of next page
    url = response_data$headers$`x-next-page`

    # if content is on more than 1 page, continue getting data on subsequent pages until max number of pages is reached (if specified)
    keep_going = ifelse(is.null(number_of_pages), !is.null(url), !is.null(url) & n <= number_of_pages)

  }

  timediff = Sys.time() - t0
  if (verbose_b) {
  cat(sprintf("###### Done in %s %s ####\n", round(timediff, 3), units(timediff)))
  }
  return(do.call(rbind, all_endpoint_data))
}
