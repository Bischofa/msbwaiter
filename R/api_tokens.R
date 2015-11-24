#' Search for authorization token or prompt for it.
#'
#' \code{get_token} searches for the environment variable MSBWAITER_TOKEN. If not
#' set, it prompts the user for an authorization token. If the session is not 
#' interactive, the function will throw an error.
#'
#'@param overwrite A logical. Should we prompt if token exists? Usual use
#'by other functions calls for the default behaviour \code{overwrite = FALSE}.
#'
#' @seealso \code{\link[base]{Sys.setenv}}
#' @export

get_token = function(overwrite = FALSE){
  # default message
  msg <- "Please enter your authorization token (without quotes):"
  # token exists
  if ((current_token <- Sys.getenv("MSBWAITER_TOKEN")) != "") {
    if (!overwrite) {
      return(current_token)
    }
    cat(sprintf("Current token is:\n%s\n", current_token))
    msg <- "Please enter your new authorization token (without quotes), or press enter to keep existing one:"
  }
  # ask for the token
  if (!interactive()) { stop("TOKEN: cannot prompt for token in a non-interactive session. Please set it manually.") }
  new_token <- readline(msg)
  if (new_token == "" & current_token != "") {
    invisible(current_token)
  } else {
    while (new_token == "") { # If non-existant, ask again until different than "".
      cat("Sorry, this is an empty token...\n")
      new_token <- readline(msg)
    }
    Sys.setenv(MSBWAITER_TOKEN = new_token)
    invisible(new_token)
  }
}










