#' Create or update MS Bioscreen data entries?
#'
#' \code{api_check_batch} fetches all the bioscreen data for the specified endpoint. \code{api_check_batch} then compares each entry
#' in the inputed sufl data set with the corresponding entry in the bioscreen data set. For each entry in the inputed sufl data set,
#' \code{api_check_batch} returns "create", "update", or "no action". When \code{change} is set to TRUE, \code{api_check_batch} will
#' proceed to actually create and update entries. \code{api_check_batch} is similar to \code{api_check} except that it can take a
#' sufl data set with multiple entries. When there is only one entry, use \code{api_check} for faster comparison. See ?\code{api_check}
#' for more details.
#'
#' @inheritParams api_do_action
#' @inheritParams api_check
#' @param sufl_batch a data frame with any number of rows and column names that follow the current SUFL specification (1.0). At minimum, sufl_batch
#' must contain the identifier columns "source_id" and "external_identifier". For attacks, treatments and visits data, sufl_batch
#' must also contain the identifier columns "patient_source_id" and "patient_external_identifier".
#' @param destructive if TRUE and change = TRUE, \code{api_check_batch} will delete any data in the bioscreen that is not found in \code{sufl_batch} but that
#' has the same \code{source_id} as \code{sufl_batch}.
#' @param max_update the maximum number of entries that you expect will need to be updated. If there are more than max_update entries to update,
#' \code{api_check_batch} will not proceed to create/update/delete entries in the bioscreen even if \code{change} is equal to TRUE. Set to NA for no restriction
#' on max_update.
#' @param max_delete the maximum number of entries that you expect will need to be deleted from the bioscreen when destructive = TRUE. If there are more
#' than max_delete entries to delete, \code{api_check_batch} will not proceed to create/update/delete entries in the bioscreen even if \code{destructive} and \code{change}
#' are equal to TRUE. Set to NA for no restriction on max_delete.
#'
#' @return
#' \code{api_check_batch} returns a list of length 2. The first entry in the list is a vector the same length as the number
#' of rows in \code{sufl_batch} where each entry of the vector is either create', 'update', or 'no action'. See ?\code{api_check}
#' for more details. The second entry in the list is the number of entries that were found in the bioscreen but not in \code{sufl_batch}.
#'
#' @seealso \code{\link{api_check}}, \code{\link{api_get_batch}}, \code{\link{api_create}},
#' \code{\link{api_update}}, \code{\link{to_json_non_array}}
#' @export

api_check_batch = function(sufl_batch, endpoint,
                           ignore_colnames = c("first_name", "last_name"),
                           base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                           token = get_token(), verbose_b = TRUE,
                           keep_na = FALSE, change = FALSE,
                           destructive = FALSE, max_update = 200, max_delete = 200){

  # get batch of data from bioscreen...
  data_from_app = api_get_batch(endpoint = endpoint, base_url = base_url, token = token, verbose_b = FALSE)

  # compare sufl_batch with all bioscreen data to determine which entries need to be created/updated
  action_list = list()
  for(i in 1:nrow(sufl_batch)){
    action_list[[i]] = compare_entries(sufl_data = sufl_batch[i, ], data_from_app = data_from_app,
                                       ignore_colnames = ignore_colnames, endpoint = endpoint, verbose_b = verbose_b,
                                       keep_na = keep_na)
  }

  # determine which entries are only found in the bioscreen and will need to be deleted if destructive = TRUE and change = TRUE
  destroy_data = data_from_app[data_from_app$source_id %in% unique(sufl_batch$source_id), ]
  unique_sufl_batch_id  = paste(sufl_batch$source_id, sufl_batch$external_identifier, sep = "_")
  unique_destroy_data_id = paste(destroy_data$source_id, destroy_data$external_identifier, sep = "_")
  destroy_data = destroy_data[!unique_destroy_data_id %in% unique_sufl_batch_id, ]
  destroy_data_n = nrow(destroy_data)
  if(verbose_b){
    cat(sprintf("There are %s entries that are found in the bioscreen (source_id = %s) that are not found in sufl_batch.\n",
                destroy_data_n, paste(unique(sufl_batch$source_id), collapse = ",")))
  }

  # safeguard against too many entries in the bioscreen being updated
  if(!is.na(max_update)){
    if(sum(unlist(action_list) == "update") > max_update){
      if(change){
        warning(sprintf("%d entries to update is above the maximum set (%d).", sum(unlist(action_list) == "update"), max_update ))
        stop("MAX_UPDATE error: too many entries to update.")
      } else{
        warning(sprintf("%d entries to update is above the maximum set (%d).", sum(unlist(action_list) == "update"), max_update ))
      }
    }
  }

  # safeguard against too many entries in the bioscreen being deleted
  if(!is.na(max_delete) & destructive){
    if(destroy_data_n > max_delete){
      if(change){
        warning(sprintf("%d entries to delete is above the maximum set (%d).", destroy_data_n, max_delete))
        stop("MAX_DELETE error: too many entries to delete.")
      } else{
        warning(sprintf("%d entries to delete is above the maximum set (%d).", destroy_data_n, max_delete))
      }
    }
  }

  # if change = TRUE, proceed to create/update/delete
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
                          keep_na = keep_na)
             }
      )
    }

    if(destructive){

      if(verbose_b){
        cat("Deleting", destroy_data_n, "entries from the bioscreen.\n")
      }
      if(destroy_data_n != 0){
        for(i in 1:destroy_data_n){
          api_delete(source_id = destroy_data$source_id[i],
                     external_identifier = destroy_data$external_identifier[i],
                     endpoint = endpoint, base_url = base_url,
                     token = token, verbose_b = verbose_b)
        }
      }
    }
  }

  return(list(action_list = unlist(action_list), number_of_entries_only_in_bioscreen = destroy_data_n))
}
