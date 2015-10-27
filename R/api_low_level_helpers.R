#' HTTP request status
#'
#' \code{return_status} checks whether an HTTP request was successful and prints the contents of the error if
#' the request was not successful.
#'
#' @param response_data HTTP response.
#' @param ok_status the status code that indicates the HTTP request was successful.
#' @param print_when_ok the message to print if the HTTP request was successful.
#'
#' @return For successful HTTP requests, \code{return_status} returns TRUE and prints the \code{print_when_ok} message. For
#' unsuccessful HTTP requests, \code{return_status} returns FALSE and prints the content of the response message.
#'
#' @seealso \code{\link{api_get_batch}}, \code{\link{api_create}}, \code{\link{api_update}},
#' \code{\link{api_delete}}, \code{\link{api_get}}, \code{\link{api_search_by_epicid}},
#' \code{\link{api_search_by_updated_at}}, \code{\link{api_check_data}}
#' @export

return_status = function(response_data, ok_status = c(200, 201, 202),
                         print_when_ok = "Done.\n"){
  if (!response_data$status_code %in% ok_status) {
    cat("\n")
    print(unlist(content(response_data)))
    return(FALSE)
  } else {
    cat(print_when_ok)
    return(TRUE)
  }
}

#' Convert R objects to JSON non-array
#'
#' \code{to_json_non_array} converts an R object to a JSON non-array.
#'
#' @param x the object to be encoded
#' @param overwrite_na_to_missing if x contains an NA, should this data be removed from the JSON object
#' or does the NA represent a missing value that should be represented as null in the JSON. The default is set to
#' FALSE so that any NA values will be ignored and not part of the JSON.
#' @param ... arguments passed on to class specific print methods
#'
#' @return
#' a JSON without brackets.
#'
#' @seealso \code{\link[jsonlite]{toJSON}}, \code{\link{api_get}},
#' \code{\link{api_search_by_epicid}}, \code{\link{api_search_by_updated_at}}
#' @export

to_json_non_array = function(x, overwrite_na_to_missing = FALSE, ...){
  if(overwrite_na_to_missing){
    x = jsonlite::toJSON(x, na = "null", ...)
  } else{
    x =  jsonlite::toJSON(x, ...)
  }
  x = gsub("\\[", "", x)
  gsub("\\]", "", x)
}

#' Convert R objects from JSON to a data frame
#'
#' \code{response_to_data_frame} converts a JSON to a data frame
#'
#' @inheritParams return_status
#'
#' @seealso \code{\link[jsonlite]{fromJSON}}, \code{\link{api_get_batch}},
#' \code{\link{api_get}}, \code{\link{api_search_by_epicid}},
#' \code{\link{api_search_by_updated_at}}
#' @export

response_to_data_frame = function(response_data){
  content_data = content(response_data, as = "text")
  jsonlite::fromJSON(content_data)
}

