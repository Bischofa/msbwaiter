#' Create or update MS Bioscreen data entry?
#'
#'\code{api_check} fetches the bioscreen entry with the same identifier values as those in the inputed sufl data set.
#'\code{api_check} returns "update" or "no action" depending on whether all other values in the inputed sufl data set match
#'those in the bioscreen data entry. If a corresponding bioscreen data entry does not exist, \code{api_check}
#'returns "create". When \code{change} is set to TRUE, \code{api_check} will proceed to actually create and update entries.
#'To check many data entries at once, use \code{api_check_batch}. See ?\code{api_check_batch} for more details.
#'
#' @inheritParams api_do_action
#' @inheritParams to_json_non_array
#' @param sufl_data a data frame with 1 row and column names that follow the current SUFL specification (1.0). At minimum, sufl_data
#' must contain the identifier columns "source_id" and "external_identifier". For attacks, treatments and visits data, sufl_data
#' must also contain the identifier columns "patient_source_id" and "patient_external_identifier".
#' @param ignore_colnames the names of the variables whose values should not be updated in the bioscreen. Set to NULL to allow any
#' variable to be updated in the bioscreen if its value is different in the inputed sufl data set.
#' @param endpoint the data endpoint of interest. Possible values are "subjects", "attacks",
#' "treatments", and "visits".
#' @param base_url the API base URL.
#' @param verbose_b print progress messages as function runs?
#' @param keep_na when there is an NA value in the sufl data set that is not NA in the
#' corresponding bioscreen data entry, should the value in the bioscreen be overwritten to NA? Default
#' is set to FALSE so that non-missing values in the bioscreen are not overwritten to NA.
#' @param change should the function proceed to actually create and update entries in the bioscreen?
#' Default is set to FALSE.
#'
#' @seealso \code{\link{api_check_batch}}, \code{\link{api_get}}, \code{\link{api_create}},
#' \code{\link{api_update}}, \code{\link{to_json_non_array}}
#' @export

api_check = function(sufl_data, endpoint,
                     ignore_colnames = c("first_name", "last_name"),
                     base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                     token = get_token(), verbose_b = TRUE,
                     keep_na = FALSE, change = FALSE){

  # searching for entry in bioscreen...
  data_from_app = api_get(source_id = sufl_data$source_id, external_identifier = sufl_data$external_identifier,
                          endpoint = endpoint, base_url = base_url, token = token, verbose_b = FALSE)

  # compare sufl_data with bioscreen entry
  action = compare_entries(sufl_data = sufl_data, data_from_app = data_from_app, endpoint = endpoint,
                           ignore_colnames = ignore_colnames,
                           verbose_b = verbose_b, keep_na = keep_na)

  # if change = TRUE, proceed to update/create
  if(change){
    switch(action,
           create = {
             api_create(sufl_data = sufl_data, endpoint = endpoint,
                        base_url = base_url, verbose_b = verbose_b)
           },
           update = {
             api_update(sufl_data = sufl_data, endpoint = endpoint,
                        ignore_colnames = ignore_colnames,
                        base_url = base_url, verbose_b = verbose_b,
                        keep_na = keep_na)
           }
    )
  }
  return(action)
}



