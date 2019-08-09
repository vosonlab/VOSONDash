#' @title Run the VOSON Dashboard Shiny Application
#' 
#' @description This function launches the \pkg{VOSONDash} Shiny app in the default web browser. 
#' 
#' @param pkgStartupMsgs Logical. Display app package loading messages. Default is \code{FALSE}.
#' @param isLocal Logical. Manually set app local or server mode flag.
#' 
#' @return None
#' 
#' @export
runVOSONDash <- function(pkgStartupMsgs = FALSE, isLocal = NULL) {
  app_dir <- system.file("", "vosondash", package = "VOSONDash")
  if (app_dir == "") {
    stop("Could not find the app try re-installing the VOSONDash package.", call. = FALSE)
  }
  
  if (!is.logical(pkgStartupMsgs)) { pkgStartupMsgs = FALSE }
  if (!is.logical(isLocal)) { isLocal <- NULL }
  
  # set verbose package loading by app
  # .GlobalEnv$.VOSONPkgMsgs <- pkgStartupMsgs
  # gbl_vars <- c(".VOSONPkgMsgs")
  shiny::shinyOptions(VOSONPkgMsgs = pkgStartupMsgs, VOSONIsLocal = isLocal)
  
  # set app mode if specified
  # if (!missing(isLocal) && is.logical(isLocal)) {
    # .GlobalEnv$.VOSONIsLocal <- isLocal
    # gbl_vars <- append(gbl_vars, ".VOSONIsLocal")
    # shiny::shinyOptions(VOSONIsLocal = isLocal)
  # }
  
  # on.exit(rm(list = gbl_vars, envir = .GlobalEnv))
  on.exit(shiny::shinyOptions(VOSONPkgMsgs = NULL, VOSONIsLocal = NULL))
  
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}
