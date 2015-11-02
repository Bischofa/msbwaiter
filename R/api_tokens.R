#' Prompt for authorization token
#'
#' \code{prompt_for_token} prompts the user for an authorization token. The token
#' entered is set as an evironment variable named 'msbwaiter_token'. If the user has
#' set 'msbwaiter_token' in the global environment, \code{prompt_for_token} will return an error.
#'
#' @seealso \code{\link[base]{Sys.setenv}}, \code{\link[base]{get_token}}
#' @export

prompt_for_token = function(){
  if(exists("msbwaiter_token")){
    stop("msbwaiter_token has been set in the global environment")
  }
    Sys.setenv(msbwaiter_token = readline("Please enter your authorization token (without quotes): "))
}

#' Search for authorization token
#'
#' \code{get_token} searches the global environment for \code{token_name} and returns its value if it exists. If
#' \code{token_name} has not been set in the global environment, \code{get_token} will search the environment
#' for \code{token_name} and return its value if it exisits. If \code{token_name} is not found in either environment,
#' \code{get_token} will return an error.
#'
#' @seealso \code{\link[base]{Sys.setenv}}, \code{\link[base]{prompt_for_token}}
#' @export

get_token = function(token_name = "msbwaiter_token"){
  if(exists(token_name)){
    get(token_name)
  } else{
    token = Sys.getenv(token_name)
    if(token == ""){
      stop(token_name, " has not been set")
    }
    token
  }
}










