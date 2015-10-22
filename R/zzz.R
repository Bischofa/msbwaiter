.onLoad <- function(libname, pkgname) {
  Sys.setenv(msbwaiter_token = readline("Please enter your authorization token (without quotes): "))
}
.onUnload <- function(libname, pkgname) {
  Sys.unsetenv("msbwaiter_token")
}
