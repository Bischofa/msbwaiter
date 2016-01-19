#' Delete MS Bioscreen data entries
#'
#' \code{api_delete_batch} deletes all the bioscreen data entries associated with the specified \code{source_ids},
#' and \code{endpoint}.
#'
#' @inheritParams api_delete
#' @param source_ids source identifers
#'
#' @details
#' Deleting 'subjects' data will also delete the associated 'attacks', 'treatments', and 'visits' data.
#' Deleting 'attacks', 'treatments', or 'visits' data will NOT delete the associated data in any of the other
#' endpoints.
#'
#' @seealso \code{\link{api_delete}}
#' @export

api_delete_batch = function(source_ids, endpoint,
                            base_url = "https://msbioscreen-uat.herokuapp.com/api/v1",
                            token = get_token(), verbose_b = TRUE){

  batch_data = api_get_batch(endpoint = endpoint, base_url = base_url, token = token, verbose_b = verbose_b)
  batch_data = batch_data[batch_data$source_id %in% source_ids, ]

  for(i in 1:nrow(batch_data)){
    api_delete(source_id = batch_data$source_id[i],
               external_identifier = batch_data$external_identifier[i], endpoint = endpoint,
               base_url = base_url, token = token)
  }

}

