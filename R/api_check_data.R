#' Create or update MS Bioscreen data?
#'
#' \code{api_check_data} searches bioscreen for data entry that matches the identifiers of the inputed data frame and indicates
#' whether bioscreen data needs to be updated, created, or left alone based on the inputed data frame.
#'
#' @inheritParams api_do_action
#' @inheritParams to_json_non_array
#' @param sufl_data a data frame of 1 row with appropriate identifier columns; see 'Details'.
#' @param ignore_col_names the column names to ignore when comparing \code{sufl_data} with the data in the bioscreen. Set to NULL to compare all columns.
#' @param endpoint the name of the data endpoint to fetch. Possible values are "subjects", "attacks", "treatments", "visits".
#' @param base_url the API base URL.
#' @param verbose_b print progress messages as function runs?
#' @param overwrite_na_to_missing if sufl_data contains an NA, should this data be ignored or should this value be considered
#' missing? The default is set to FALSE so that any NA values will be ignored; see 'Details'.
#'
#' @details
#' sufl_data must be a data frame of 1 row with column names that follow the SUFL format. For checking subjects data, sufl_data needs to
#' contain the columns, 'source_id' and 'external_identifier'. For checking attacks, treatments, or visits data, sufl_data needs to
#' contain the columns, 'source_id', 'external_identifier', 'patient_source_id', and 'patient_external_identifier'. If any of these
#' column names are missing, \code{api_check_data} will return an error message. Also note that if there are NA values in non-identifier
#' columns of sufl_data that are non-missing in the bioscreen, \code{api_check_data} will only return 'update' if
#' \code{overwrite_na_to_missing} is TRUE.
#'
#' @return
#' \code{api_check_data} returns 'create' if there is no data in the bioscreen that matches the identifiers found in sufl_data. If there is data in
#' the bioscreen that matches the identifiers found in sufl_data, \code{api_check_data} returns either 'update' or 'no action'. \code{api_check_data}
#' returns 'no action' when all sufl_data values match those in the bioscreen entry. Note that if there are missing values in non-identifier columns
#' of the bioscreen that are non-missing in sufl_data, \code{api_check_data} will return 'update'. If there are NA values in non-identifier columns
#' of sufl_data that are non-missing in the bioscreen entry, \code{api_check_data} will return 'update' only if \code{overwrite_na_to_missing} is TRUE.
#'
#' @seealso \code{\link{api_get}}, \code{\link{api_create}}, \code{\link{api_update}}

api_check_data = function(sufl_data, ignore_col_names = c("first_name", "last_name"),
                          endpoint = "subjects",
                          base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                          token = Sys.getenv("msbwaiter_token"), verbose_b = TRUE,
                          overwrite_na_to_missing = FALSE){

  if (verbose_b) {
    cat(sprintf("Checking whether %s data (source_id: %s, external_identifier: %s) needs to be created or updated...", endpoint, sufl_data$source_id, sufl_data$external_identifier))
  }

  # searching data in bioscreen...
  data_from_app = api_get(source_id = sufl_data$source_id, external_identifier = sufl_data$external_identifier,
                          endpoint = endpoint, base_url = base_url, token = token, verbose_b = FALSE)

  #stop if there are duplicate entries in the bioscreen
  if(any(duplicated(paste(data_from_app$source_id, data_from_app$external_identifier, sep = "_")))){
    stop("duplicated entries in ", endpoint, " data set")
  }

  # if data does not exist, data needs to be uploaded
  if(length(data_from_app) == 0){
    action = "create"
    if (verbose_b) {
      cat("data needs to be created.\n")
    }
  } else{

    # option here to check 'date created tag' of data set, if data from app more recent, no action, else go through rest of code ###

    # if data exists, check whether all info is the same
    colnames_to_look_at = setdiff(intersect(colnames(sufl_data), colnames(data_from_app)), ignore_col_names)
    sufl_data = sufl_data[, colnames_to_look_at]
    data_from_app = data_from_app[, colnames_to_look_at]

    # are all non-missing values equal?
    sufl_data = sapply(sufl_data, as.character)
    data_from_app = sapply(data_from_app, as.character)
    no_action_test1 = all(sufl_data == data_from_app, na.rm = T)

    # are all missing/NA values in bioscreen also missing/NA in sufl_data?
    missing_sufl_data = is.na(sufl_data) | as.character(sufl_data) == ""
    missing_data_from_app = is.na(data_from_app) | as.character(data_from_app) == ""
    no_action_test2 = !any(!missing_sufl_data & missing_data_from_app)

    # are all missing/NA values in sufl_data also missing/NA in bioscreen?
    # if no_action_test3 is FALSE but overwrite_na_to_missing is FALSE, then bioscreen should not be updated to missing when sufl_data is missing so no_action_test3 should be TRUE
    no_action_test3 = !any(missing_sufl_data & !missing_data_from_app)
    no_action_test3 = ifelse(no_action_test3, TRUE, !overwrite_na_to_missing)

    if(no_action_test1 & no_action_test2 & no_action_test3){
      action = "no action"
      if (verbose_b) {
        cat("most up to date data has already been uploaded.\n")
      }
    } else{
      action = "update"
      if (verbose_b) {

        index1 = which(sufl_data != data_from_app)
        index2 = which(!missing_sufl_data & missing_data_from_app)
        if(overwrite_na_to_missing){
          index3 = which(missing_sufl_data & !missing_data_from_app)
        } else{
          index3 = NULL
        }

        changing_fields = colnames_to_look_at[unique(c(index1, index2, index3))]
        cat(sprintf("data needs to be updated for the following fields: %s.\n", paste(changing_fields, collapse = ", ")))
      }
    }
  }
  return(action)
}


