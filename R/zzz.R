.onLoad <- function(libname, pkgname) {
  if(!exists("msbwaiter_token")){
    prompt_for_token()
  }
}
.onUnload <- function(libname, pkgname) {
  Sys.unsetenv("msbwaiter_token")
}
