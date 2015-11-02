#' Create or update MS Bioscreen data entry?
#'
#' \code{api_check} searches the bioscreen for a data entry that matches the identifiers
#' of the inputed data frame. If no such data entry exists, \code{api_check} will return 'create'.
#' Otherwise, \code{api_check} will return 'update' or 'no action' depending on how closely the
#' values of the inputed data frame match those in the corresponding bioscreen data entry. To check many
#' sufl data entries at once, use \code{api_check_batch}.
#'
#' @inheritParams api_do_action
#' @inheritParams to_json_non_array
#' @param sufl_data a data frame of 1 row with appropriate identifier columns; see 'Details'.
#' @param ignore_colnames the column names to ignore when comparing the sufl data set with the data in the
#' bioscreen. Set to NULL to compare all columns.
#' @param endpoint the name of the data endpoint to fetch. Possible values are "subjects", "attacks",
#' "treatments", and "visits".
#' @param base_url the API base URL.
#' @param verbose_b print progress messages as function runs?
#' @param overwrite_na_to_missing if the sufl data set contains an NA, should this data be ignored or should
#' this value be encoded as a missing value? The default is set to FALSE so that any NA values will be
#' ignored; see 'Details'.
#' @param change should the function proceed to actually update or create data in the bioscreen?
#' Default is set to FALSE.
#'
#' @details
#' sufl_data must be a data frame of 1 row with column names that follow the SUFL format. For checking
#' subjects data, sufl_data needs to contain the identifier columns, 'source_id' and 'external_identifier'.
#' For checking attacks, treatments, or visits data, sufl_data needs to contain the identifier columns,
#' 'source_id', 'external_identifier', patient_source_id', and 'patient_external_identifier'. If any of
#' these columns are missing, \code{api_check} will return an error message. Note that if there are
#' NA values in the non-identifier/non-ignore_colnames columns of sufl_data that are non-missing in the
#' bioscreen, \code{api_check} will return 'update' only if \code{overwrite_na_to_missing} is TRUE.
#'
#' @return
#' \code{api_check} returns 'create' if there is currently no data in the bioscreen that matches the
#' identifier columns of sufl_data. If there is data in the bioscreen that matches the identifier columns of
#' sufl_data, \code{api_check} will return either 'no action' or 'update'. \code{api_check} returns
#' 'no action' when all sufl_data values (minus the ignore_colnames) match those in the corresponding bioscreen
#' entry. Note that if there are missing values in the non-identifier/non-ignore_colnames columns of the bioscreen
#' that are non-missing in sufl_data, \code{api_check} will return 'update'. However, if there are NA values in the
#' non-identifier/non-ignore_colnames columns of sufl_data that are non-missing in the bioscreen,
#' \code{api_check} will return 'update' only if \code{overwrite_na_to_missing} is TRUE.
#'
#' @seealso \code{\link{api_check_batch}}, \code{\link{api_get}}, \code{\link{api_create}},
#' \code{\link{api_update}}, \code{\link{to_json_non_array}}
#' @export

api_check = function(sufl_data, ignore_colnames = c("first_name", "last_name"),
                     endpoint = "subjects",
                     base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                     token = get_token("msbwaiter_token"), verbose_b = TRUE,
                     overwrite_na_to_missing = FALSE, change = FALSE){

  # searching for entry in bioscreen...
  data_from_app = api_get(source_id = sufl_data$source_id, external_identifier = sufl_data$external_identifier,
                          endpoint = endpoint, base_url = base_url, token = token, verbose_b = FALSE)

  # compare sufl_data with bioscreen entry
  action = compare_entries(sufl_data = sufl_data, data_from_app = data_from_app,
                           ignore_colnames = ignore_colnames, endpoint = endpoint, verbose_b = verbose_b,
                           overwrite_na_to_missing = overwrite_na_to_missing)

  # if change = TRUE, proceed to update/create
  if(change){
    switch(action,
           create = {
             api_create(sufl_data = sufl_data, endpoint = endpoint,
                        base_url = base_url, verbose_b = verbose_b)
           },
           update = {
             sufl_data = sufl_data[, !(colnames(sufl_data) %in% ignore_colnames)]
             api_update(sufl_data = sufl_data, endpoint = endpoint,
                        base_url = base_url, verbose_b = verbose_b,
                        overwrite_na_to_missing = overwrite_na_to_missing)
           }
    )
  }
  return(action)
}



