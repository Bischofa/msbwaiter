#' Create or update MS Bioscreen data?
#'
#' \code{api_check_batch} compares the entries of \code{sufl_batch} with those currently in the bioscreen. This
#' function is similar to \code{api_check} except that it can take a sufl data set with many entries. \code{api_check_batch}
#' fetches all the data from the bioscreen and then performs its comparison. See ?\code{api_check} for more details.
#'
#' @inheritParams api_do_action
#' @inheritParams to_json_non_array
#' @inheritParams api_check
#' @param sufl_batch a data frame with SUFL identifier columns; see 'Details'.
#' @param max_update the maximum number of entries that you expect will need to be updated. If there are more than max_update entries to update,
#' \code{api_check_batch} will not proceed to create/update entries in the bioscreen even if \code{change} is equal to TRUE. Set to NA for no restriction
#' on max_update.
#'
#' @details
#' sufl_batch must be a data frame with column names that follow the SUFL format. For checking
#' subjects data, sufl_batch needs to contain the identifier columns, 'source_id' and 'external_identifier'.
#' For checking attacks, treatments, or visits data, sufl_batch needs to contain the identifier columns,
#' 'source_id', 'external_identifier', patient_source_id', and 'patient_external_identifier'. If any of
#' these columns are missing, \code{api_check_batch} will return an error message. Note that if there are
#' NA values in the non-identifier/non-ignore_colnames columns of sufl_batch that are non-missing in the
#' bioscreen, \code{api_check_batch} will return 'update' only if \code{overwrite_na_to_missing} is TRUE.
#'
#' @return
#' \code{api_check_batch} returns a list of length 2. The first entry in the list is a vector the same length as the number
#' of rows in \code{sufl_batch} where each entry of the vector  is either create', 'update', or 'no action'. See ?\code{api_check}
#' for more details. If \code{api_check_batch} proceeded to update/create entries, the second entry in the list is TRUE. Otherwise,
#' the second entry in the list is FALSE.
#'
#' @seealso \code{\link{api_check}}, \code{\link{api_get_batch}}, \code{\link{api_create}},
#' \code{\link{api_update}}, \code{\link{to_json_non_array}}
#' @export

api_check_batch = function(sufl_batch, ignore_colnames = c("first_name", "last_name"),
                           endpoint = "subjects",
                           base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                           token = get_token("msbwaiter_token"), verbose_b = TRUE,
                           overwrite_na_to_missing = FALSE, change = FALSE, max_update = 200){

  # get batch of data from bioscreen...
  data_from_app = api_get_batch(endpoint = endpoint, base_url = base_url, token = token, verbose_b = FALSE)

  # compare sufl_batch with all bioscreen data
  action_list = list()
  for(i in 1:nrow(sufl_batch)){
    action_list[[i]] = compare_entries(sufl_data = sufl_batch[i, ], data_from_app = data_from_app,
                                       ignore_colnames = ignore_colnames, endpoint = endpoint, verbose_b = verbose_b,
                                       overwrite_na_to_missing = overwrite_na_to_missing)
  }

  if(!is.na(max_update)){
    if(sum(unlist(action_list) == "update") > max_update){
      if(change){
        change = FALSE
        warning("There are more than ", max_update, " entries that need to be updated so api_check_batch did not proceed to create/update entries. Increase max_update and run api_check_batch again if you would like to create/update entries.")
      } else{
        warning("There are more than ", max_update, " entries that need to be updated.")
      }
    }
  }

  # if change = TRUE, proceed to update/create
  if(change){
    for(i in 1:nrow(sufl_batch)){
      action = action_list[[i]]
      switch(action,
             create = {
               api_create(sufl_data = sufl_batch[i, ], endpoint = endpoint,
                          base_url = base_url, verbose_b = verbose_b)
             },
             update = {
               api_update(sufl_data = sufl_batch[i, ], endpoint = endpoint,
                          ignore_colnames = ignore_colnames,
                          base_url = base_url, verbose_b = verbose_b,
                          overwrite_na_to_missing = overwrite_na_to_missing)
             }
      )
    }
  }

  return(list(unlist(action_list), did_change = change))
}
