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
#' \code{\link{api_search_by_updated_at}}, \code{\link{api_check}}

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

to_json_non_array = function(x, overwrite_na_to_missing = FALSE, ...){
  if(overwrite_na_to_missing){
    x = jsonlite::toJSON(x, na = "null", ...)
  } else{
    x =  jsonlite::toJSON(x, ...)
  }
  x = gsub("\\[", "", x)
  gsub("\\]", "", x)
}

#' Convert an HTTP response to a data frame
#'
#' \code{response_to_data_frame} converts an HTTP response to a data frame
#'
#' @inheritParams return_status
#'
#' @seealso \code{\link[jsonlite]{fromJSON}}, \code{\link{api_get_batch}},
#' \code{\link{api_get}}, \code{\link{api_search_by_epicid}},
#' \code{\link{api_search_by_updated_at}}

response_to_data_frame = function(response_data){
  content_data = content(response_data, as = "text")
  jsonlite::fromJSON(content_data)
}

# helper function for comparing entries between a sufl data set and the data in the bioscreen
compare_entries = function(sufl_data, data_from_app, ignore_colnames = c("first_name", "last_name"),
                           endpoint = "subjects", verbose_b = TRUE, overwrite_na_to_missing = FALSE){

  if (verbose_b) {
    cat(sprintf("Checking whether %s data (source_id: %s, external_identifier: %s) needs to be created or updated...",
                endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

  # if data does not exist, data needs to be uploaded
  data_index = (data_from_app$source_id == sufl_data$source_id) & (data_from_app$external_identifier == sufl_data$external_identifier)
  if(sum(data_index) == 0){
    action = "create"
    if (verbose_b) {
      cat("data needs to be created.\n")
    }
  } else{

    # if data exists, check whether all info is the same
    colnames_to_look_at = setdiff(intersect(colnames(sufl_data), colnames(data_from_app)), ignore_colnames)
    sufl_data = sufl_data[, colnames_to_look_at]
    data_from_app = data_from_app[data_index, colnames_to_look_at]

    # convert the data frames into vectors for easier comparison
    sufl_data_values = as.vector(sapply(sufl_data, as.character))
    data_from_app_values = as.vector(sapply(data_from_app, as.character))

    # look at which values differ between sufl_data and data_from_app
    index1 = which(sufl_data_values != data_from_app_values)

    # look at which values are NA in data_from_app but are non-missing in sufl_data
    missing_sufl_data = is.na(sufl_data_values) | as.character(sufl_data_values) == ""
    missing_data_from_app = is.na(data_from_app_values) | as.character(data_from_app_values) == ""
    index2 = which(!missing_sufl_data & missing_data_from_app)

    # If overwrite_na_to_missing = TRUE, look at which values are NA in sufl_data but are non-missing in data_from_app
    if(overwrite_na_to_missing){
      index3 = which(missing_sufl_data & !missing_data_from_app)
    } else{
      index3 = NULL
    }

    # look at which entries need to be updated, if no entries need to be updated, return 'no action'
    entries_to_update = unique(c(index1, index2, index3))

    if(length(entries_to_update) == 0){
      action = "no action"
      if (verbose_b) {
        cat("most up to date data has already been uploaded.\n")
      }
    } else{
      action = "update"
      if(verbose_b){
        cat(sprintf("data needs to be updated for the following fields: %s.\n", paste(colnames_to_look_at[entries_to_update], collapse = ", ")))
      }
    }
  }
  return(action)
}
